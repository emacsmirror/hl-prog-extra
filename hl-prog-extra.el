;;; hl-prog-extra.el --- Customizable highlighting for source-code -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;; Copyright (C) 2021  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://gitlab.com/ideasman42/emacs-hl-prog-extra
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "26.2"))

;;; Commentary:

;; This package provides an easy way to highlight words in programming modes,
;; where terms can be highlighted on code, comments or strings.
;;

;;; Usage

;;
;; Write the following code to your .emacs file:
;;
;;   (require 'hl-prog-extra)
;;   (global-hl-prog-extra-mode)
;;
;; Or with `use-package':
;;
;;   (use-package hl-prog-extra)
;;   (global-hl-prog-extra-mode)
;;
;; If you prefer to enable this per-mode, you may do so using
;; mode hooks instead of calling `global-hl-prog-extra-mode'.
;; The following example enables this for org-mode:
;;
;;   (add-hook 'python-mode-hook
;;     (lambda ()
;;       (hl-prog-extra-mode)))
;;

;;; Code:

;; ---------------------------------------------------------------------------
;; Custom VarIables

(defgroup hl-prog-extra nil
  "Custom additional user-defined faces for comments/strings or code."
  :group 'faces)

;; Default to URL's and email addresses, avoid adding too many here
;; as users may want to extend to this list for their own purposes.
(defcustom hl-prog-extra-list
  (list
    ;; Match `http://xyz' (URL)
    '("\\<https?://[^[:blank:]]*" 0 comment font-lock-constant-face)
    ;; Match `<email@address.com>' email address.
    '("<\\([[:alnum:]\\._-]+@[[:alnum:]\\._-]+\\)>" 1 comment font-lock-constant-face)

    ;; Highlight `TODO` or `TODO(text): and similar.
    '
    ("\\<\\(TODO\\|NOTE\\)\\(([^)+]+)\\)?"
      0
      comment
      '(:background "#006000" :foreground "#FFFFFF"))
    '
    ("\\<\\(FIXME\\|XXX\\|WARNING\\|BUG\\)\\(([^)+]+)\\)?"
      0
      comment
      '(:background "#800000" :foreground "#FFFFFF")))
  "Lists that match faces (context face regex regex-group)

`regex':
  The regular expression to match.
`regex-subexpr':
  Group to use when highlighting the expression (zero for the whole match).
`context':
  A symbol in:
  - 'comment - All comments.
  - 'comment-only - Only non-documentation comments.
  - 'comment-doc - Only documentation comments.
  - 'string - All strings.
  - 'string-only - Only non-documentation strings.
  - 'string-doc - Documentation strings.
  - nil - Non comments or strings.

  This limits the highlighting to only these parts of the text,
  where nil is used for anything that doesn't match a comment or string.

  A list of these symbols is also supported
  (allowing a single item to match multiple contexts).
`face':
  The face to apply.

Modifying this while variable `hl-prog-extra-mode' is enabled requires calling
`hl-prog-extra-refresh'to update the internal state."
  :type
  '
  (repeat
    (list
      regexp integer
      (choice
        (const :tag "Comment (Any)" :value comment)
        (const :tag "Comment (Only)" :value comment-only)
        (const :tag "Comment (Doc)" :value comment-doc)
        (const :tag "String (Any)" :value string)
        (const :tag "String (Only)" :value string-only)
        (const :tag "String (Doc)" :value string-doc)
        (const :tag "Other" :value nil)
        ;; A list of choices is also supported.
        (repeat symbol))
      face)))

(defcustom hl-prog-extra-preset nil
  "Include the default preset for the major modes (when available)."
  :type 'boolean)

(defcustom hl-prog-extra-global-ignore-modes nil
  "List of major-modes to exclude when `hl-prog-extra' has been enabled globally."
  :type '(repeat symbol))

(defvar-local hl-prog-extra-global-ignore-buffer nil
  "When non-nil, Global `hl-prog-extra' will not be enabled for this buffer.
This variable can also be a predicate function, in which case
it'll be called with one parameter (the buffer in question), and
it should return non-nil to make Global `hl-prog-extra' Mode not
check this buffer.")


;; ---------------------------------------------------------------------------
;; Internal Variables

(defvar-local hl-prog-extra--data nil
  "Internal data used for `hl-prog-extra--match' to do font locking.")


;; ---------------------------------------------------------------------------
;; Generic Utilities

(defun hl-prog-extra--match-first (match)
  "Return a the first valid group from MATCH and it's zero based index."
  (setq match (cddr match))
  (let ((i 0))
    (while (and match (null (car match)))
      (setq match (cddr match))
      (setq i (1+ i)))
    (cons match i)))

(defun hl-prog-extra--match-index-set (beg end index)
  "Set the match data from BEG to END at INDEX."
  (let ((gen-match (list beg end)))
    (dotimes (_ index)
      (setq gen-match (cons nil (cons nil gen-match))))
    (setq gen-match (cons beg (cons end gen-match)))
    (set-match-data gen-match)))

(defun hl-prog-extra--regexp-valid-or-error (re)
  "Return nil if RE is not a valid regexp."
  (condition-case err
    (prog1 nil
      (string-match-p re ""))
    (error (error-message-string err))))


;; ---------------------------------------------------------------------------
;; Pre-Compute Font Locking

(defun hl-prog-extra--precompute-keywords (face-vector)
  "Create data to pass to `font-lock-add-keywords' from FACE-VECTOR."
  ;; The generated result will look something like this.
  ;; (list
  ;;   (list
  ;;     'hl-prog-extra--match
  ;;     (cons 2 (list 'font-lock-string-face t t))
  ;;     (cons 1 (list 'font-lock-warning-face t t))
  ;;     (cons 0 (list 'font-lock-constant-face t t))
  ;;
  ;; Counting down is important so the first matching group that is met is used.
  (let ((keywords (list)))
    (dotimes (i (length face-vector))
      (let ((face (aref face-vector i)))

        ;; Without this, a face that is not yet loaded will raise an error in font lock.
        (when (and (symbolp face) (not (boundp face)))
          (setq face (list 'quote face)))

        ;; The first number is the regex-group to match (starting at 1).
        ;;
        ;; The two booleans after `face' are:
        ;; - Ignore error, this is important as the groups start with the highest number first.
        ;; - Counting down. If a match isn't met, keep looking and don't error.
        (push (cons (1+ i) (list face t t)) keywords)))

    (setq keywords (nreverse keywords))
    ;; Put the matching function at the head of the list.
    (push 'hl-prog-extra--match keywords)

    ;; There is only one highlighter at the moment.
    (list keywords)))

(defun hl-prog-extra--precompute-regex (syn-regex-list)
  "Pre-compute data from the SYN-REGEX-LIST.

Return (re-string face-table) where:

regex-string:
  A list of 3 strings containing grouped regex statements from SYN-REGEX-LIST.

`face-list':
  Unique faces.
`uniq-list':
  Unique data for each regex group.
`face-table':
  Map the regex group index to the `face-list'.

Tables are aligned with SYN-REGEX-LIST."
  (let ((len (length syn-regex-list)))
    (let
      ( ;; Group regex by the context they search in.
        (re-comment-only (list))
        (re-comment-doc (list))
        (re-string-only (list))
        (re-string-doc (list))
        (re-rest (list))
        ;; Store variables for the doc/only variables are different.
        ;; This is useful since calculating this information is expensive.
        (is-complex-comment nil)
        (is-complex-string nil)

        ;; Unique faces, use to build the arguments for font locking.
        (face-list (list))
        (face-list-contents (make-hash-table :test 'eq :size len))
        ;; Unique values aligned with the regex groups.
        ;; Each element be a list if other kinds of data needs to be referenced.
        (uniq-list (list))
        (uniq-list-contents (make-hash-table :test 'eql :size len))
        ;; Map the regex-group index to the face-list index.
        (face-table (list))

        ;; Error checking.
        (item-error-prefix "hl-prog-extra, error parsing `hl-prog-extra-list'")
        (item-index 0)
        (item-context-valid-items
          (list
            'comment 'comment-only 'comment-doc ;; Comments.
            'string 'string-only 'string-doc ;; Strings.
            nil)))

      (dolist (item syn-regex-list)
        (pcase-let ((`(,re ,re-subexpr ,context ,face) item))
          ;; Ensure the context is a list (users may provide a single symbol).
          ;; Supporting both is nice as it allows multiples contexts to be used at once.
          (unless (and context (listp context))
            (setq context (list context)))

          (let
            ( ;; Validate inputs.
              ;;
              ;; Be strict here since any errors on font-locking are difficult for users to debug.
              (has-error
                (cond
                  ;; Check `re'.
                  ((not (stringp re))
                    (message
                      "%s: 1st (regex) expected a string! (at %d)"
                      item-error-prefix
                      item-index)
                    t)
                  (
                    (let ((item-error (hl-prog-extra--regexp-valid-or-error re)))
                      (when item-error
                        (message
                          "%s: 1st (regex) invalid regex \"%s\" (at %d)"
                          item-error-prefix
                          item-error
                          item-index)
                        t))
                    t)

                  ;; Check `re-subexpr'.
                  ((not (integerp re-subexpr))
                    (message
                      "%s: 2nd (regex sub-expression) expected an integer! (at %d)"
                      item-error-prefix
                      item-index)
                    t)
                  ((< re-subexpr 0)
                    (message
                      "%s: 2nd (regex sub-expression) cannot be negative! (at %d)"
                      item-error-prefix
                      item-index)
                    t)

                  ;; Check `context' (coerced into a list or left as nil).
                  ((not (listp context))
                    (message
                      "%s: 3rd (context) expected a symbol, a list of symbols or nil!) (at %d)"
                      item-error-prefix
                      item-index))
                  ;; Check that all items in the list are symbols (includes nil).
                  ( ;; If the list is non-empty, there is some unexpected expression.
                    (delq
                      t
                      (mapcar
                        (lambda (context-symbol)
                          (cond
                            ((memq context-symbol item-context-valid-items)
                              t)
                            (t
                              (message
                                (concat
                                  "%s: 3rd (context) unexpected symbol %S, "
                                  "expected a value in %S! "
                                  "(at %d)")
                                item-error-prefix
                                context-symbol
                                item-context-valid-items
                                item-index)
                              nil)))
                        context))

                    t)

                  ;; Check `face'
                  ((not (or (facep face) (listp face)))
                    (message
                      (concat
                        "%s: 4th (face) expected a symbol, string, face or "
                        "list of face properties. "
                        "%S is not known! (at %d)")
                      item-error-prefix face item-index)
                    t)

                  ;; No error.
                  (t
                    nil)))
              ;; End error checking.

              (uniq-index nil)
              (face-index nil))

            (unless has-error
              (when (and re-subexpr (zerop re-subexpr))
                (setq re-subexpr nil))

              (let ((key face))
                (setq face-index (gethash key face-list-contents))

                (unless face-index
                  (setq face-index (hash-table-count face-list-contents))
                  (push face face-list)
                  (puthash key face-index face-list-contents)))

              (let ((key (cons face re-subexpr)))
                (setq uniq-index (gethash key uniq-list-contents))
                (unless uniq-index
                  (setq uniq-index (hash-table-count uniq-list-contents))
                  (push re-subexpr uniq-list)
                  (puthash key uniq-index uniq-list-contents)
                  ;; Unrelated to maintaining `uniq-list-contents'.
                  (push face-index face-table)))

              (let ((regex-fmt (format "\\(?%d:%s\\)" (1+ uniq-index) re)))
                (dolist (context-symbol context)
                  (cond
                    ((eq context-symbol 'comment)
                      (push regex-fmt re-comment-only)
                      (push regex-fmt re-comment-doc))
                    ((eq context-symbol 'comment-only)
                      (setq is-complex-comment t)
                      (push regex-fmt re-comment-only))
                    ((eq context-symbol 'comment-doc)
                      (setq is-complex-comment t)
                      (push regex-fmt re-comment-doc))
                    ((eq context-symbol 'string)
                      (push regex-fmt re-string-only)
                      (push regex-fmt re-string-doc))
                    ((eq context-symbol 'string-only)
                      (setq is-complex-string t)
                      (push regex-fmt re-string-only))
                    ((eq context-symbol 'string-doc)
                      (setq is-complex-string t)
                      (push regex-fmt re-string-doc))
                    ((null context-symbol)
                      (push regex-fmt re-rest))
                    (t ;; Checked for above.
                      (error "Invalid context %S" context-symbol))))))))

        (setq item-index (1+ item-index)))

      (list
        ;; Join all regex groups into single strings.
        (mapcar
          (lambda (re)
            (when re
              (mapconcat #'identity (nreverse re) "\\|")))
          (list re-comment-only re-comment-doc re-string-only re-string-doc re-rest))

        (vconcat (nreverse face-list))
        (vconcat (nreverse uniq-list))
        (vconcat (nreverse face-table))
        is-complex-comment
        is-complex-string))))


;; ---------------------------------------------------------------------------
;; Internal Font Lock Match

(defun hl-prog-extra--check-face-at-point (pos face-test)
  "Return t when FACE-TEST is used at POS."
  ;; NOTE: use `get-text-property' instead of `get-char-property' so overlays are excluded,
  ;; since this causes overlays with `hl-line-mode' (for example) to mask other faces.
  ;; If we want to include faces of overlays, this could be supported.
  (let ((faceprop (get-text-property pos 'face)))
    (cond
      ((facep faceprop)
        (when (eq faceprop face-test)
          t))
      ((face-list-p faceprop)
        (let ((found nil))
          (while faceprop
            (when (eq (pop faceprop) face-test)
              (setq found t)
              (setq faceprop nil)))
          found))
      (t
        nil))))

(defun hl-prog-extra--is-doc-state-p (state)
  "Return t, when the comment or string is a doc-string or doc-comment at STATE."
  (let ((start (nth 8 state)))
    (hl-prog-extra--check-face-at-point start 'font-lock-doc-face)))

(defun hl-prog-extra--match (bound)
  "MATCHER for the font lock keyword in `hl-prog-extra--data', until BOUND."
  (let
    (
      (found nil)
      (state-at-pt (syntax-ppss))
      (state-at-pt-next nil)
      (info (car hl-prog-extra--data))
      ;; Always case sensitive.
      (case-fold-search nil))
    (pcase-let
      (
        ( ;; Unpack (car info)
          `
          (,`(,re-comment-only ,re-comment-doc ,re-string-only ,re-string-doc ,re-rest)
            ;; Unpack (cdr info)
            ,_ ,uniq-array ,face-table ,is-complex-comment ,is-complex-string)
          info))
      (while (and (null found) (< (point) bound))
        (let
          (
            (re-context
              (cond
                ((nth 3 state-at-pt)
                  (cond
                    ((and is-complex-string (hl-prog-extra--is-doc-state-p state-at-pt))
                      re-string-doc)
                    (t
                      re-string-only)))
                ((nth 4 state-at-pt)
                  (cond
                    ((and is-complex-comment (hl-prog-extra--is-doc-state-p state-at-pt))
                      re-comment-doc)
                    (t
                      re-comment-only)))
                (t
                  re-rest))))

          ;; Only search in the current context.
          (cond
            (re-context
              (let
                (
                  (bound-context
                    (save-excursion
                      (setq state-at-pt-next
                        (parse-partial-sexp (point) bound nil nil state-at-pt 'syntax-table))
                      (point)))

                  (bound-context-clamp nil))

                ;; Without this, the beginning of a comment is seen as code,
                ;; in practice this means C-style comments such as `/*XXX*/',
                ;; end up considering the first two characters as code.
                ;; This causes problems if we want to highlight operators,
                ;; eg: `*' or `/' characters.
                ;; Avoid this by further clamping the context not to step
                ;; into the beginning of a comment or string.
                ;; Note that this introduces 'gaps', where the beginnings of
                ;; comments can't be matched.
                ;; Properly adjusting all beginnings and ends ends up being quite
                ;; complex since state isn't maintained between calls to this function.
                ;; So unless there is an important use-case for this, accept the limitation since
                ;; not being able to properly match symbols in code is a much larger limitation.
                (when (or (nth 3 state-at-pt-next) (nth 4 state-at-pt-next))
                  (let ((comment-or-string-start (nth 8 state-at-pt-next)))
                    (when (<= (point) comment-or-string-start)
                      (setq bound-context-clamp comment-or-string-start))))

                (cond
                  ((re-search-forward re-context (or bound-context-clamp bound-context) t)
                    (setq found t)
                    (pcase-let*
                      ( ;; The `uniq-index' is always needed so the original font can be found
                        ;; and so it's possible to check for a sub-expression.
                        (`(,match-tail . ,uniq-index) (hl-prog-extra--match-first (match-data)))
                        (`(,beg-final ,end-final) match-tail))

                      ;; When sub-expressions are used, they need to be extracted.
                      (let ((sub-expr (aref uniq-array uniq-index)))
                        (when sub-expr
                          (pcase-let ((`(,beg ,end) (nthcdr (* 2 sub-expr) match-tail)))
                            ;; The configuration may have an out of range `sub-expr'
                            ;; just ignore this and use the whole expression since raising
                            ;; an error during font-locking in this case isn't practical.
                            (when (and beg end)
                              (setq beg-final beg)
                              (setq end-final end)))))

                      ;; Remap the absolute table to the unique face.
                      (hl-prog-extra--match-index-set
                        (marker-position beg-final)
                        (marker-position end-final)
                        (aref face-table uniq-index)))

                    (when bound-context-clamp
                      ;; If the clamped bounds is met, step to the un-clamped bounds.
                      (when (>= (point) bound-context-clamp)
                        (goto-char bound-context))))

                  ;; Not found, skip to the next context.
                  (t
                    (goto-char bound-context)
                    (setq state-at-pt state-at-pt-next)))))

            ;; Nothing to search for, simply skip over this context.
            (t
              (setq state-at-pt
                (parse-partial-sexp (point) bound nil nil state-at-pt 'syntax-table)))))))
    found))


;; ---------------------------------------------------------------------------
;; Presets

;;;###autoload
(defun hl-prog-extra-preset (&rest args)
  "Load a preset for current mode.
ARGS the first two arguments are positional,
The first is MODE-VALUE to override the current `major-mode'.
The second is QUIET, when non-nil, don't show a message
when the preset isn't found.
The rest are expected to be keyword arguments,
to control the behavior of each preset,
see it's documentation for available keywords."
  (let
    (
      (mode-value nil)
      (quiet nil)
      (args-positional t)
      (args-count 0))

    (while (and args args-positional)
      (let ((arg (car args)))
        (cond
          ((keywordp arg)
            ;; Found a keyword argument, break.
            (setq args-positional nil))
          (t
            (pcase args-count
              (0 (setq mode-value arg))
              (1 (setq quiet arg))
              (_ (error "Only two positional arguments must be given")))
            (setq args-count (1+ args-count))
            (setq args (cdr args))))))

    (unless mode-value
      (setq mode-value (symbol-name major-mode)))
    (let ((preset-sym (intern (concat "hl-prog-extra-preset-" mode-value))))
      (when
        (condition-case err
          (progn
            (require preset-sym)
            t)
          (error
            (unless quiet
              (message "hl-prog-extra: preset %S not found! (%S)" mode-value err))
            nil))
        (apply preset-sym args)))))


;; ---------------------------------------------------------------------------
;; Define Minor Mode

(defun hl-prog-extra-mode-enable ()
  "Turn on option `hl-prog-extra-mode' for the current buffer."
  ;; Paranoid.
  (when hl-prog-extra--data
    (font-lock-remove-keywords nil (cdr hl-prog-extra--data)))

  (let ((list-with-preset hl-prog-extra-list))
    (when hl-prog-extra-preset
      ;; Optionally add presets to the end of the list.
      (setq list-with-preset (append list-with-preset (hl-prog-extra-preset nil t))))
    (let ((info (hl-prog-extra--precompute-regex list-with-preset)))
      (let ((keywords (hl-prog-extra--precompute-keywords (nth 1 info))))
        (font-lock-add-keywords nil keywords 'append)
        (font-lock-flush)
        (setq hl-prog-extra--data (cons info keywords))))))

(defun hl-prog-extra-mode-disable ()
  "Turn off option `hl-prog-extra-mode' for the current buffer."
  (when hl-prog-extra--data
    (font-lock-remove-keywords nil (cdr hl-prog-extra--data))
    (font-lock-flush))
  (kill-local-variable 'hl-prog-extra--data))

;;;###autoload
(defun hl-prog-extra-refresh ()
  "Update internal data after changing `hl-prog-extra-list'."
  (when hl-prog-extra--data
    (hl-prog-extra-mode-disable)
    (hl-prog-extra-mode-enable)))

;;;###autoload
(define-minor-mode hl-prog-extra-mode
  "Highlight matches for `hl-prog-extra-list' in comments, strings and code."
  :lighter ""
  :keymap nil

  (cond
    (hl-prog-extra-mode
      (hl-prog-extra-mode-enable))
    (t
      (hl-prog-extra-mode-disable))))

(defun hl-prog-extra--mode-turn-on ()
  "Enable the option `hl-prog-extra-mode' where possible."
  (when
    (and
      ;; Not already enabled.
      (not hl-prog-extra-mode)
      ;; Not in the mini-buffer.
      (not (minibufferp))
      ;; Not a special mode (package list, tabulated data ... etc)
      ;; Instead the buffer is likely derived from `text-mode' or `prog-mode'.
      (not (derived-mode-p 'special-mode))
      ;; Not explicitly ignored.
      (not (memq major-mode hl-prog-extra-global-ignore-modes))
      ;; Optionally check if a function is used.
      (or
        (null hl-prog-extra-global-ignore-buffer)
        (cond
          ((functionp hl-prog-extra-global-ignore-buffer)
            (not (funcall hl-prog-extra-global-ignore-buffer (current-buffer))))
          (t
            nil))))
    (hl-prog-extra-mode 1)))

;;;###autoload
(define-globalized-minor-mode
  global-hl-prog-extra-mode
  hl-prog-extra-mode
  hl-prog-extra--mode-turn-on)


(provide 'hl-prog-extra)
;;; hl-prog-extra.el ends here
