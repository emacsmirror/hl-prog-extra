;;; hl-prog-extra.el --- Customizable highlighting for source-code -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;; Copyright (C) 2021  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-hl-prog-extra
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

(eval-when-compile
  ;; For `pcase-dolist'.
  (require 'pcase))

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
   (list "\\<https?://[^[:blank:]]*" 0 'comment 'font-lock-constant-face)
   ;; Match `<email@address.com>' email address.
   (list "<\\([[:alnum:]\\._-]+@[[:alnum:]\\._-]+\\)>" 1 'comment 'font-lock-constant-face)

   ;; Highlight `TODO` or `TODO(text): and similar.
   (list
    "\\<\\(TODO\\|NOTE\\)\\(([^)+]+)\\)?"
    0
    'comment
    '(:background "#006000" :foreground "#FFFFFF"))
   (list
    "\\<\\(FIXME\\|XXX\\|WARNING\\|BUG\\)\\(([^)+]+)\\)?"
    0
    'comment
    '(:background "#800000" :foreground "#FFFFFF")))
  "Lists that match faces (context face regex regex-group)

`regex':
  The regular expression to match.
`regex-subexpr':
  Group to use when highlighting the expression (zero for the whole match).
`context':
  A symbol in:
  - `'comment' - All comments.
  - `'comment-only' - Only non-documentation comments.
  - `'comment-doc' - Only documentation comments.
  - `'string' - All strings.
  - `'string-only' - Only non-documentation strings.
  - `'string-doc' - Documentation strings.
  - nil - Non comments or strings (other source-code).

  This limits the highlighting to only these parts of the text,
  where nil is used for anything that doesn't match a comment or string.

  A list of these symbols is also supported
  (allowing a single item to match multiple contexts).
`face':
  The face to apply.

Modifying this while variable `hl-prog-extra-mode' is enabled requires calling
`hl-prog-extra-refresh'to update the internal state."
  :type
  '(repeat
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

;; NOTE: Accumulating values in in keyword parser doesn't have any
;; problems in the common case but isn't totally fool-proof.
;; If parsing is interrupted  for any reason (for e.g.)
;; stale keyword highlighting data could be left in the stack.
;; To avoid using stale data, ensure the state is what we expect from the last call.
(defvar-local hl-prog-extra--data-match-stack nil
  "Internal data used for `hl-prog-extra--match' to do font locking.")
(defvar-local hl-prog-extra--data-match-stack-state nil
  "Internal data used to ensure a stale stack is never used.")

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

(defun hl-prog-extra--maybe-prefix (prefix msg)
  "Prefix MSG with PREFIX or return MSG when nil."
  (cond
   ((null msg)
    nil)
   (t
    (concat prefix msg))))

;; ---------------------------------------------------------------------------
;; Pre-Compute Font Locking

(defun hl-prog-extra--validate-keyword-item (item)
  "Validate ITEM, return an message or nil on success."
  (let* ((item-context-valid-items
          (list
           'comment 'comment-only 'comment-doc ; Comments.
           'string 'string-only 'string-doc ; Strings.
           nil))

         ;; ----------------------------
         ;; Check `re', 1st argument.

         (validate-re-fn
          (lambda (re)
            (catch 'error
              (when (not (stringp re))
                (throw 'error (format "expected a string, not a %S!" (type-of re))))

              (let ((item-error (hl-prog-extra--regexp-valid-or-error re)))
                (when item-error
                  (throw 'error (format "invalid regex \"%s\"" item-error)))))))

         ;; ---------------------------------
         ;; Check `re-subexpr', 2nd argument.

         (validate-re-subexpr-single-fn
          (lambda (re-subexpr)
            ;; `intergerp' ensured by caller.
            (catch 'error
              (when (< re-subexpr 0)
                (throw 'error "cannot be negative!")))))

         (validate-re-subexpr-list-fn
          (lambda (re-subexpr)
            ;; `listp' ensured by caller.
            (catch 'error
              (dolist (re-subexpr-sub re-subexpr)
                (unless (integerp re-subexpr-sub)
                  (throw 'error
                         (format "expected an integer, not a %S!" (type-of re-subexpr-sub))))
                (let ((error-msg (funcall validate-re-subexpr-single-fn re-subexpr-sub)))
                  (when error-msg
                    (throw 'error error-msg)))))))

         (validate-re-subexpr-fn
          (lambda (re-subexpr)
            (cond
             ((integerp re-subexpr)
              (funcall validate-re-subexpr-single-fn re-subexpr))
             ((and re-subexpr (listp re-subexpr))
              (funcall validate-re-subexpr-list-fn re-subexpr))
             (t
              (format "expected an integer or a list of integers, not a %S!"
                      (type-of re-subexpr))))))

         ;; ------------------------------
         ;; Check `context', 3rd argument.

         (validate-context-single-fn
          (lambda (context)
            ;; Not `listp' ensured by caller.
            (catch 'error
              (unless (symbolp context)
                (throw 'error (format "expected a symbol or nil!, not a %S" (type-of context))))
              (unless (memq context item-context-valid-items)
                (throw 'error
                       (format "unexpected symbol %S, expected a value in %S!"
                               context
                               item-context-valid-items))))))

         (validate-context-list-fn
          (lambda (context)
            ;; `listp' ensured by caller.
            (catch 'error
              (dolist (context-sub context)
                (let ((error-msg (funcall validate-context-single-fn context-sub)))
                  (when error-msg
                    (throw 'error error-msg)))))))

         (validate-context-fn
          (lambda (context)
            (cond
             ((null context) ; Do nothing (correct input).
              nil)
             ((not (listp context))
              (funcall validate-context-single-fn context))
             ((listp context)
              (funcall validate-context-list-fn context))
             (t
              (format "expected a list or symbol, not a %S!" (type-of context))))))

         ;; ---------------------------
         ;; Check `face', 4th argument.

         (validate-face-single-fn
          (lambda (face)
            (catch 'error
              (when (not (or (facep face) (listp face)))
                (throw
                 'error
                 (format
                  "expected a symbol, string, face or list of face properties. %S is not known!"
                  face))))))

         (validate-face-list-fn
          (lambda (face re-subexpr)
            (catch 'error
              (unless (and face (listp face))
                (throw 'error (format "expected a list of faces, not %S" (type-of face))))
              (dolist (face-sub face)
                (let ((error-msg (funcall validate-face-single-fn face-sub)))
                  (when error-msg
                    (throw 'error error-msg))))

              ;; Now check the face list is compatible with `re-subexpr'.
              (when (not (eq (length re-subexpr) (length face)))
                (throw 'error "list lengths do not match!")))))

         (validate-face-fn
          (lambda (face re-subexpr)
            (cond
             ((not (listp re-subexpr))
              (funcall validate-face-single-fn face))
             (t ; List.
              (funcall validate-face-list-fn face re-subexpr))))))


    (pcase-let ((`(,re ,re-subexpr ,context ,face) item))

      ;; Return on the first error, or nil.
      (or
       ;; Check `regex' (1st).
       (hl-prog-extra--maybe-prefix "1st (regex) " (funcall validate-re-fn re))
       ;; Check `re-subexpr' (2nd).
       (hl-prog-extra--maybe-prefix "2nd (sub-expr) " (funcall validate-re-subexpr-fn re-subexpr))
       ;; Check `context' (3rd), coerced into a list or left as nil.
       (hl-prog-extra--maybe-prefix "3rd (context) " (funcall validate-context-fn context))
       ;; Check `face' (4th).
       (hl-prog-extra--maybe-prefix "4th (face) " (funcall validate-face-fn face re-subexpr))))))


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
        (when (or
               ;; Quote unquoted symbol.
               (and (symbolp face) (not (boundp face)))
               ;; Quote unquoted list.
               (and (consp face) (not (eq 'quote (car face)))))
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

Tables are aligned with SYN-REGEX-LIST."
  (let ((len (length syn-regex-list)))
    (let ((item-index 0)
          ;; Error checking.
          (item-error-prefix "hl-prog-extra, error parsing `hl-prog-extra-list'")

          ;; Group regex by the context they search in.
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
          ;; Use `equal` so vector can be used as keys.
          (uniq-list-contents (make-hash-table :test 'equal :size len)))

      (pcase-dolist (`(,re ,re-subexpr ,context ,face) syn-regex-list)

        ;; Ensure the context is a list (users may provide a single symbol).
        ;; Supporting both is nice as it allows multiples contexts to be used at once.
        (unless (and context (listp context))
          (setq context (list context)))

        (let ((error-msg
               ;; Be strict here since any errors on font-locking are difficult for users to debug.
               (hl-prog-extra--validate-keyword-item (list re re-subexpr context face)))

              ;; Handle cases with multiple sub-expressions.
              (is-multi nil)
              (uniq-index-multi nil)

              (uniq-index nil)
              (face-index nil))

          (cond
           (error-msg
            (message "%s: %s (item %d)" item-error-prefix error-msg item-index))
           (t ; No error.

            (cond
             ((and re-subexpr (listp re-subexpr))
              (when (<= 1 (length re-subexpr))
                (setq is-multi t)))
             (t ; Move into a list to avoid duplicate code-paths.
              (setq re-subexpr (list re-subexpr))
              (setq face (list face))))

            (while re-subexpr
              (let ((re-sub (pop re-subexpr))
                    (face-sub (pop face)))

                ;; Note that a zero `re-sub' is not the same as nil,
                ;; since a zero group is needed for matching the first level of parenthisis.

                (let ((key face-sub))
                  (setq face-index (gethash key face-list-contents))

                  (unless face-index
                    (setq face-index (hash-table-count face-list-contents))
                    (push face-sub face-list)
                    (puthash key face-index face-list-contents)))

                (let ((key (cons face-sub re-sub)))
                  (setq uniq-index (gethash key uniq-list-contents))
                  (unless uniq-index
                    (setq uniq-index (hash-table-count uniq-list-contents))
                    (when is-multi
                      (push uniq-index uniq-index-multi))

                    (push (cons re-sub face-index) uniq-list)
                    (puthash key uniq-index uniq-list-contents)))))

            (when is-multi
              (setq uniq-index-multi (sort uniq-index-multi #'>))
              (let ((key uniq-index-multi))
                (setq uniq-index (gethash key uniq-list-contents))
                (unless uniq-index
                  (setq uniq-index (hash-table-count uniq-list-contents))
                  (push (vconcat uniq-index-multi) uniq-list)
                  (puthash key uniq-index uniq-list-contents))))

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
                 (t ; Checked for above.
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

(defun hl-prog-extra--match-impl (bound)
  "MATCHER for the font lock keyword in `hl-prog-extra--data', until BOUND."
  (let ((found nil)
        (state-at-pt (syntax-ppss))
        (state-at-pt-next nil)
        (info (car hl-prog-extra--data))
        ;; Always case sensitive.
        (case-fold-search nil))
    ;; Unpack `car' of info, (first block), remainder unpacks the `cdr' of info.
    (pcase-let ((`(,`(,re-comment-only ,re-comment-doc ,re-string-only ,re-string-doc ,re-rest)
                   ;; Second block.
                   ,_ ,uniq-array ,is-complex-comment ,is-complex-string)
                 info))
      (while (and (null found) (< (point) bound))
        (let ((re-context
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
            (let ((bound-context
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
                ;; The `uniq-index' is always needed so the original font can be found
                ;; and so it's possible to check for a sub-expression.
                (pcase-let* ((`(,match-tail . ,uniq-index)
                              (hl-prog-extra--match-first (match-data)))
                             (`(,beg-final ,end-final) match-tail))

                  (let ((uniq-data (aref uniq-array uniq-index)))
                    (cond
                     ;; This is really an indirection (list of unique indices).
                     ;; `uniq-data' is a list of indices into `uniq-array'.
                     ((vectorp uniq-data)
                      (let ((beg-end-index-list (list)))
                        (dotimes (i (length uniq-data))
                          (let ((uniq-data-sub (aref uniq-array (aref uniq-data i))))
                            (pcase-let ((`(,sub-expr . ,face-index) uniq-data-sub))
                              (pcase-let ((`(,beg ,end) (nthcdr (* 2 sub-expr) match-tail)))
                                (when (and beg end)
                                  (push (list
                                         (marker-position beg)
                                         (marker-position end)
                                         face-index)
                                        beg-end-index-list))))))

                        (cond
                         ;; Empty, do nothing.
                         ((null beg-end-index-list)
                          nil)

                         ;; A single item, use simple handling (nothing clever).
                         ;; This can happen when a multi-item has optional matches,
                         ;; and only one is found.
                         ((null (cdr beg-end-index-list))
                          (pcase-let ((`(,beg ,end ,face-index) (car beg-end-index-list)))
                            (hl-prog-extra--match-index-set beg end face-index)))

                         ;; Multi-item match, handle the first, store the rest
                         ;; in the stack to be returned before further searching.
                         (t
                          (setq hl-prog-extra--data-match-stack beg-end-index-list)
                          (hl-prog-extra--match-impl-precalc bound)
                          ;; Don't step into the next context, allow for the remaining
                          ;; items in the stack to be handled first.
                          (setq bound-context-clamp nil)))))

                     (t
                      ;; When sub-expressions are used, they need to be extracted.
                      (when uniq-data
                        (pcase-let ((`(,sub-expr . ,face-index) uniq-data))
                          (when sub-expr
                            (pcase-let ((`(,beg ,end) (nthcdr (* 2 sub-expr) match-tail)))
                              ;; The configuration may have an out of range `sub-expr'
                              ;; just ignore this and use the whole expression since raising
                              ;; an error during font-locking in this case isn't practical.
                              (when (and beg end)
                                (setq beg-final beg)
                                (setq end-final end))))

                          ;; Remap the absolute table to the unique face.
                          (hl-prog-extra--match-index-set
                           (marker-position beg-final)
                           (marker-position end-final)
                           face-index)))))))

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

(defun hl-prog-extra--match-impl-precalc (bound)
  "Set the match state based on pre-calculated values.
Argument BOUND is only used to validate the state."
  (let ((item (pop hl-prog-extra--data-match-stack)))
    (cond
     (item
      (pcase-let ((`(,beg ,end ,index) item))
        (hl-prog-extra--match-index-set beg end index)

        ;; Check this before using items in `hl-prog-extra--data-match-stack'.
        (setq hl-prog-extra--data-match-stack-state
              (cond
               (hl-prog-extra--data-match-stack
                ;; Go to the beginning of the next match to prevent the bounds from being reached.
                (goto-char (min (car (car hl-prog-extra--data-match-stack)) bound))
                ;; Check this is unchanged on re-entry.
                (cons (point) bound))
               (t
                ;; Go to the end since there is no further data to parse.
                (goto-char (min beg bound))
                nil)))
        ;; Found.
        t))
     (t
      nil))))

(defun hl-prog-extra--match (bound)
  "MATCHER for the font lock keyword in `hl-prog-extra--data', until BOUND."

  (when hl-prog-extra--data-match-stack
    ;; Ensure stale data from this stack is never used (even though it's unlikely).
    ;; See `hl-prog-extra--data-match-stack-state' for details.
    (when (or
           ;; Should never happen, check more for correctness.
           (null hl-prog-extra--data-match-stack-state)
           ;; The point moved since last search.
           (not (eq (point) (car hl-prog-extra--data-match-stack-state)))
           ;; The bound moved since last search.
           (not (eq bound (cdr hl-prog-extra--data-match-stack-state))))

      (setq hl-prog-extra--data-match-stack-state nil)
      (setq hl-prog-extra--data-match-stack nil)))

  (cond
   (hl-prog-extra--data-match-stack
    (hl-prog-extra--match-impl-precalc bound))
   (t
    (hl-prog-extra--match-impl bound))))


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
  (let ((mode-value nil)
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
      (when (condition-case err
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

(defun hl-prog-extra--mode-enable ()
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

(defun hl-prog-extra--mode-disable ()
  "Turn off option `hl-prog-extra-mode' for the current buffer."
  (when hl-prog-extra--data
    (font-lock-remove-keywords nil (cdr hl-prog-extra--data))
    (font-lock-flush))
  (kill-local-variable 'hl-prog-extra--data)
  (kill-local-variable 'hl-prog-extra--data-match-stack)
  (kill-local-variable 'hl-prog-extra--data-match-stack-state))

;;;###autoload
(defun hl-prog-extra-refresh ()
  "Update internal data after changing `hl-prog-extra-list'."
  (when hl-prog-extra--data
    (hl-prog-extra--mode-disable)
    (hl-prog-extra--mode-enable)))

;;;###autoload
(define-minor-mode hl-prog-extra-mode
  "Highlight matches for `hl-prog-extra-list' in comments, strings and code."
  :lighter ""
  :keymap nil

  (cond
   (hl-prog-extra-mode
    (hl-prog-extra--mode-enable))
   (t
    (hl-prog-extra--mode-disable))))

(defun hl-prog-extra--mode-turn-on ()
  "Enable the option `hl-prog-extra-mode' where possible."
  (when (and
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
         (or (null hl-prog-extra-global-ignore-buffer)
             (cond
              ((functionp hl-prog-extra-global-ignore-buffer)
               (not (funcall hl-prog-extra-global-ignore-buffer (current-buffer))))
              (t
               nil))))
    (hl-prog-extra-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-hl-prog-extra-mode
  hl-prog-extra-mode
  hl-prog-extra--mode-turn-on)


(provide 'hl-prog-extra)
;;; hl-prog-extra.el ends here
