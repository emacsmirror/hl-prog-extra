;;; hl-prog-extra_html.el --- HTML export of fontified text -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-2.0-or-later

;;; Commentary:

;; Export the fontified buffer to HTML, serving two purposes:
;;
;; - Test validation, comparing the result with an expected string.
;; - Viewing the result in a browser, to check highlighting by eye.
;;
;; By default only the faces `hl-prog-extra' applies are wrapped,
;; so tests are not coupled to the major-mode's own fontification
;; which changes between Emacs versions.
;;
;; NOTE: attributes are quoted with single quotes, which is valid HTML
;; and keeps expected strings in tests free of escaped double quotes.

;;; Code:

(eval-when-compile
  ;; For `pcase-dolist', `pcase-let'.
  (require 'pcase))

(require 'hl-prog-extra)


;; ---------------------------------------------------------------------------
;; Internal Utilities

(defun hl-prog-extra-html--escape (text)
  "Return TEXT with HTML special characters escaped."
  (declare (important-return-value t))
  ;; The ampersand must be replaced first.
  (pcase-dolist (`(,char . ,entity) (list (cons "&" "&amp;") (cons "<" "&lt;") (cons ">" "&gt;")))
    (setq text (replace-regexp-in-string char entity text t t)))
  text)

(defun hl-prog-extra-html--color-to-hex (color)
  "Return COLOR as a CSS hex color, or nil when it can't be resolved."
  (declare (important-return-value t))
  ;; NOTE: `color-values' can't be used here since it maps the color through the
  ;; frame's palette, which in batch mode is the 8 color terminal palette
  ;; (both "Firebrick" and "#006000" come back as fully saturated primaries).
  ;; `color-name-rgb-alist' is a plain table so it gives the true value.
  (cond
   ;; A face may set an attribute to nil or `unspecified' to clear it.
   ((null (stringp color))
    nil)
   ((string-prefix-p "#" color)
    (let ((digits (length (substring color 1))))
      (cond
       ;; Already usable as CSS.
       ((memq digits (list 3 6))
        color)
       ;; Emacs also writes 3 and 4 digits per component, take the high byte.
       ((memq digits (list 9 12))
        (let ((step (/ digits 3)))
          (concat
           "#"
           (mapconcat
            (lambda (i) (substring color (+ 1 (* i step)) (+ 3 (* i step)))) ;
            (number-sequence 0 2)
            ""))))
       (t
        nil))))
   (t
    ;; Names are stored without spaces and in lower case.
    (let ((rgb (assoc-string (tty-color-canonicalize color) color-name-rgb-alist t)))
      (when rgb
        (format "#%02X%02X%02X" (/ (nth 1 rgb) 256) (/ (nth 2 rgb) 256) (/ (nth 3 rgb) 256)))))))

;; NOTE: Emacs uses more weights and slants than CSS understands, writing them
;; directly gives declarations such as `font-weight: semi-bold' which the
;; browser discards. Values without an equivalent are skipped.
(defconst hl-prog-extra-html--weight-alist
  (list
   (cons 'ultra-bold "900")
   (cons 'extra-bold "800")
   (cons 'bold "bold")
   (cons 'semi-bold "600")
   (cons 'medium "500")
   (cons 'normal "normal")
   (cons 'semi-light "300")
   (cons 'light "300")
   (cons 'extra-light "200")
   (cons 'ultra-light "100"))
  "Map Emacs `:weight' values onto CSS.")

(defconst hl-prog-extra-html--slant-alist
  (list (cons 'italic "italic") (cons 'oblique "oblique") (cons 'normal "normal"))
  "Map Emacs `:slant' values onto CSS.")

(defconst hl-prog-extra-html--attr-alist
  (list
   (list :foreground "color" #'hl-prog-extra-html--color-to-hex)
   (list :background "background-color" #'hl-prog-extra-html--color-to-hex)
   (list
    :weight "font-weight" (lambda (value) (cdr (assq value hl-prog-extra-html--weight-alist))))
   (list :slant "font-style" (lambda (value) (cdr (assq value hl-prog-extra-html--slant-alist))))
   (list :underline "text-decoration" (lambda (value) (and value "underline")))
   (list :strike-through "text-decoration" (lambda (value) (and value "line-through"))))
  "Map a face attribute onto a CSS property and a function to convert its value.
Only the attributes a highlight is likely to use are listed, anything else is
skipped rather than raising an error, which isn't worth failing an export over.
A converter returning nil skips the attribute.")

(defun hl-prog-extra-html--css-from-plist (plist)
  "Return a CSS declaration string for the face PLIST, or nil when empty."
  (declare (important-return-value t))
  (let ((decl-list (list))
        (css-key-seen (list)))
    ;; NOTE: step in pairs while both are present. The legacy
    ;; `(foreground-color . COLOR)' face is not a property list,
    ;; skip it instead of raising an error part way through.
    (while (and (consp plist) (consp (cdr plist)))
      (let ((key (pop plist))
            (value (pop plist)))
        (pcase-let ((`(,css-key ,convert-fn) (cdr (assq key hl-prog-extra-html--attr-alist))))
          (when css-key
            (let ((css-value (funcall convert-fn value)))
              ;; `:underline' and `:strike-through' share a property,
              ;; keep the first since the last declaration would win.
              (when (and css-value (null (member css-key css-key-seen)))
                (push css-key css-key-seen)
                (push (concat css-key ": " css-value) decl-list)))))))
    (when decl-list
      (mapconcat #'identity (nreverse decl-list) "; "))))

(defun hl-prog-extra-html--face-as-list (face)
  "Return FACE as a list of faces.
A property list is a single face, not a list of faces."
  (declare (important-return-value t))
  (cond
   ((null face)
    nil)
   ;; NOTE: `proper-list-p' excludes a dotted pair, needed for the legacy
   ;; `(foreground-color . COLOR)' face which would otherwise be walked
   ;; as a list of faces.
   ((and (proper-list-p face) (null (keywordp (car face))))
    face)
   (t
    (list face))))

(defvar hl-prog-extra-html-display
  (list
   ;; A value may be a list, `defface' specs use several names for a
   ;; graphical display, e.g. both `(type graphic)' and `(type x-toolkit)'.
   (cons 'type (list 'graphic 'x-toolkit 'x 'w32 'ns 'pgtk))
   (cons 'class 'color)
   (cons 'min-colors 16777216)
   (cons 'background 'light))
  "The display used to resolve named faces into attributes.

NOTE: `face-attribute' can't be used for this in batch mode, where the frame is
a terminal reporting no color support, so every `(class color)' clause in a
\`defface' fails to match and only the fall-back t clause is used, leaving
text bold or italic but never colored.  Instead the face specs are matched
against this display directly, which also makes the output independent of the
frame Emacs happens to run in.

`htmlfontify' solves the same problem with `hfy-display-class', however its
matching compares `min-colors' with `eq' instead of numerically, so a display
of 16777216 colors fails to match a spec requiring 88 (or the 89 used by the
`tango' theme), and it ignores themes entirely.  So do the matching here.")

(defun hl-prog-extra-html--display-match-p (display)
  "Return non-nil when the `defface' DISPLAY matches `hl-prog-extra-html-display'."
  (declare (important-return-value t))
  (cond
   ((eq display t)
    t)
   (t
    (let ((found t))
      (pcase-dolist (`(,key . ,value-list) display)
        (when found
          (let ((value (cdr (assq key hl-prog-extra-html-display))))
            (setq found
                  (cond
                   ;; Conditions not described by the display (such as `supports')
                   ;; are not treated as a mismatch, being strict would reject
                   ;; specs that are otherwise usable.
                   ((null value)
                    t)
                   ((eq key 'min-colors)
                    (>= value (car value-list)))
                   (t
                    (let ((found-value nil))
                      (dolist (value-sub
                               (cond
                                ((listp value)
                                 value)
                                (t
                                 (list value))))
                        (when (memq value-sub value-list)
                          (setq found-value t)))
                      found-value)))))))
      found))))

(defun hl-prog-extra-html--plist-merge (plist plist-other)
  "Return PLIST with the keys of PLIST-OTHER it doesn't already set."
  (declare (important-return-value t))
  (let ((result plist))
    (while (and (consp plist-other) (consp (cdr plist-other)))
      (let ((key (pop plist-other))
            (value (pop plist-other)))
        (unless (plist-member result key)
          (setq result (append result (list key value))))))
    result))

(defun hl-prog-extra-html--face-spec-list (face)
  "Return the `defface' style specs for FACE, highest precedence first."
  (declare (important-return-value t))
  (append
   (get face 'customized-face) ;
   (get face 'saved-face) ;
   ;; Themes are stored as (THEME SPEC), most recently enabled first.
   (apply #'append (mapcar #'cadr (get face 'theme-face))) ;
   (get face 'face-defface-spec)))

(defun hl-prog-extra-html--face-to-plist (face &optional depth)
  "Return the attributes of the named FACE as a property list.
DEPTH limits how far `:inherit' is followed."
  (declare (important-return-value t))
  (let ((plist nil)
        ;; A `default' entry is not a display, it holds attributes shared by
        ;; every display which the matching entry then overrides.
        (plist-default nil)
        (plist-match nil))
    (pcase-dolist (`(,display . ,attrs) (hl-prog-extra-html--face-spec-list face))
      ;; Both `(DISPLAY . PLIST)' and the older `(DISPLAY (PLIST))' are used.
      (when (and attrs (listp (car attrs)))
        (setq attrs (car attrs)))
      (cond
       ((eq display 'default)
        (unless plist-default
          (setq plist-default attrs)))
       ((and (null plist-match) (hl-prog-extra-html--display-match-p display))
        (setq plist-match attrs))))
    (setq plist (hl-prog-extra-html--plist-merge plist-match plist-default))

    ;; Fall back for faces defined without a spec, e.g. by `set-face-attribute'.
    ;; NOTE: the keys come from the attribute table so this doesn't have to be
    ;; kept in step with it, `face-attribute' resolves `:inherit' itself here.
    (unless plist
      (pcase-dolist (`(,key . ,_) hl-prog-extra-html--attr-alist)
        (let ((value (face-attribute face key nil t)))
          (unless (memq value (list nil 'unspecified))
            (setq plist (append plist (list key value)))))))

    ;; Resolve `:inherit' so faces defined only by inheritance are not lost.
    ;; NOTE: the value may be a list of faces, merged in order with the earlier
    ;; taking priority, which `hl-prog-extra-html--plist-merge' already does.
    (let ((face-parent-list (hl-prog-extra-html--face-as-list (plist-get plist :inherit))))
      (when face-parent-list
        (setq depth (1+ (or depth 0)))
        ;; Unlikely, guard against a cycle in the inheritance chain.
        (when (< depth 8)
          (dolist (face-parent face-parent-list)
            ;; Skip `default', it holds the frame's own colors which in batch
            ;; mode are the unusable `unspecified-fg' and `unspecified-bg'.
            (when (and (facep face-parent) (null (eq face-parent 'default)))
              (let ((plist-parent (hl-prog-extra-html--face-to-plist face-parent depth)))
                (setq plist (hl-prog-extra-html--plist-merge plist plist-parent))))))))
    plist))

(defun hl-prog-extra-html--face-attrs (face inline-style)
  "Return the HTML attributes representing FACE, or nil when there are none.
When INLINE-STYLE is non-nil, named faces are resolved to their attributes
instead of being written as a class."
  (declare (important-return-value t))
  ;; NOTE: named faces map to a class and anonymous faces to an inline style.
  ;; Using the face itself (never a generated name) keeps the output stable
  ;; when unrelated entries are added to `hl-prog-extra-list'.
  (let ((class-list (list))
        (style-list (list)))
    (dolist (face-sub (hl-prog-extra-html--face-as-list face))
      (when (and inline-style (symbolp face-sub))
        (setq face-sub (hl-prog-extra-html--face-to-plist face-sub)))
      (cond
       ;; A face may define no attributes at all (`font-lock-negation-char-face'
       ;; is ((t nil)) for e.g.), checked before `symbolp' which takes the
       ;; resulting nil for a face name, writing "class='nil'".
       ((null face-sub))
       ((symbolp face-sub)
        (push (symbol-name face-sub) class-list))
       ((listp face-sub)
        (let ((css (hl-prog-extra-html--css-from-plist face-sub)))
          (when css
            (push css style-list))))))

    (let ((attr-list (list)))
      (when class-list
        (push (concat "class='" (mapconcat #'identity (nreverse class-list) " ") "'") attr-list))
      (when style-list
        (push (concat "style='" (mapconcat #'identity (nreverse style-list) "; ") "'") attr-list))
      (when attr-list
        (mapconcat #'identity (nreverse attr-list) " ")))))

(defun hl-prog-extra-html--face-filter-test (face face-filter)
  "Return non-nil when any face in FACE is a member of FACE-FILTER."
  (declare (important-return-value t))
  (let ((found nil))
    (dolist (face-sub (hl-prog-extra-html--face-as-list face))
      ;; Use `member' since anonymous faces are compared by value.
      (when (member face-sub face-filter)
        (setq found t)))
    found))


;; ---------------------------------------------------------------------------
;; Public Functions

(defun hl-prog-extra-html-face-list ()
  "Return the list of faces `hl-prog-extra' applies in this buffer."
  (declare (important-return-value t))
  (when hl-prog-extra--data
    (append (nth 1 (car hl-prog-extra--data)) nil)))

(defun hl-prog-extra-html-export (&optional face-filter inline-style newline)
  "Return the fontified buffer as an HTML fragment.

FACE-FILTER limits which faces are wrapped:
- A list of faces wraps only those, all other text is written without markup.
  An empty list wraps nothing, which is not the same as passing t.
- The symbol t wraps every face.
When NEWLINE is non-nil, each element is followed by a line break,
so that comparing the result reports the differing element.
INLINE-STYLE is handled by `hl-prog-extra-html--face-attrs'."
  (declare (important-return-value t))
  (let ((result (list))
        (pos (point-min))
        (pos-max (point-max)))
    (while (< pos pos-max)
      (let* ((pos-next (next-single-property-change pos 'face nil pos-max))
             ;; NOTE: use `get-text-property' (not `get-char-property') to exclude
             ;; overlays, matching how `hl-prog-extra' inspects faces itself.
             (face (get-text-property pos 'face))
             (text (hl-prog-extra-html--escape (buffer-substring-no-properties pos pos-next)))
             (attrs
              (when (and face
                         (or (eq face-filter t)
                             (hl-prog-extra-html--face-filter-test face face-filter)))
                (hl-prog-extra-html--face-attrs face inline-style))))

        (push (cond
               (attrs
                (concat "<span " attrs ">" text "</span>" (and newline "\n")))
               (t
                text))
              result)
        (setq pos pos-next)))
    (apply #'concat (nreverse result))))

(defun hl-prog-extra-html-export-document (&optional title)
  "Return the fontified buffer as a self-contained HTML document.
TITLE names the document and is shown as a heading, when given."
  (declare (important-return-value t))
  ;; All faces are included and resolved inline against
  ;; `hl-prog-extra-html-display', so the result needs no style-sheet and shows
  ;; the same colors a graphical frame would, even when run in batch mode.
  (let ((title-text (and title (hl-prog-extra-html--escape title))))
    (concat
     "<!DOCTYPE html>\n<html>\n<head>\n<meta charset='utf-8'>\n"
     (and title-text (concat "<title>" title-text "</title>\n"))
     "</head>\n"
     "<body style='background-color: #FFFFFF; color: #000000'>\n"
     (and title-text (concat "<h1>" title-text "</h1>\n"))
     "<pre>"
     (hl-prog-extra-html-export t t)
     "</pre>\n</body>\n</html>\n")))

(defun hl-prog-extra-html-export-file (file-in file-out)
  "Fontify FILE-IN, writing a HTML document to FILE-OUT."
  (declare (important-return-value nil))
  (with-temp-buffer
    (insert-file-contents file-in)
    ;; Needed for `set-auto-mode' to detect the mode from the file name.
    (let ((buffer-file-name (expand-file-name file-in)))
      (set-auto-mode))
    (hl-prog-extra-mode 1)
    (font-lock-ensure)
    (let ((text (hl-prog-extra-html-export-document)))
      (with-temp-file file-out
        (insert text)))))

(provide 'hl-prog-extra_html)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-format-quoted: nil
;; End:
;;; hl-prog-extra_html.el ends here
