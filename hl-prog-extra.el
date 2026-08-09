;;; hl-prog-extra.el --- Customizable highlighting for source-code -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;; Copyright (C) 2021  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-hl-prog-extra
;; Keywords: convenience
;; Version: 0.2
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This package provides an easy way to highlight words in programming modes,
;; where terms can be highlighted in code, comments or strings.
;;

;;; Usage:

;;
;; Write the following code to your .emacs file:
;;
;;   (require 'hl-prog-extra)
;;   (hl-prog-extra-global-mode)
;;
;; Or with `use-package':
;;
;;   (use-package hl-prog-extra)
;;   (hl-prog-extra-global-mode)
;;
;; If you prefer to enable this per-mode, you may do so using
;; mode hooks instead of calling `hl-prog-extra-global-mode'.
;; The following example enables this for python-mode:
;;
;;   (add-hook 'python-mode-hook
;;     (lambda ()
;;       (hl-prog-extra-mode)))
;;

;;; Code:

(eval-when-compile
  ;; For `pcase-dolist', `pcase-let'.
  (require 'pcase))


;; ---------------------------------------------------------------------------
;; Compatibility

(eval-when-compile
  (when (version< emacs-version "31.1")
    (defmacro incf (place &optional delta)
      "Increment PLACE by DELTA or 1."
      (declare (debug (gv-place &optional form)))
      (gv-letplace (getter setter) place
        (funcall setter `(+ ,getter ,(or delta 1)))))
    (defmacro decf (place &optional delta)
      "Decrement PLACE by DELTA or 1."
      (declare (debug (gv-place &optional form)))
      (gv-letplace (getter setter) place
        (funcall setter `(- ,getter ,(or delta 1)))))))


;; ---------------------------------------------------------------------------
;; Custom Variables

(defgroup hl-prog-extra nil
  "Custom additional user-defined faces for comments/strings or code."
  :group 'faces)

;; Default to URLs and email addresses, avoid adding too many here
;; as users may want to extend this list for their own purposes.
(defcustom hl-prog-extra-list
  (list
   ;; Match `http://xyz' (URL)
   (list "\\<https?://[^[:blank:]\n]*" 0 'comment 'font-lock-constant-face)
   ;; Match `<email@address.com>' email address.
   (list "<\\([[:alnum:]._-]+@[[:alnum:]._-]+\\)>" 1 'comment 'font-lock-constant-face)

   ;; Highlight "TODO" or "TODO(text):" and similar.
   (list
    "\\<\\(TODO\\|NOTE\\)\\>\\(([^)]+)\\)?"
    0
    'comment
    '(:background "#006000" :foreground "#FFFFFF"))
   (list
    "\\<\\(FIXME\\|XXX\\|WARNING\\|BUG\\)\\>\\(([^)]+)\\)?"
    0
    'comment
    '(:background "#800000" :foreground "#FFFFFF")))
  "List of patterns that define face matches (regex regex-subexpr context face)

`regex':
  The regular expression to match.

  The expression must not number its own groups (\"\\\\(?1:..\\\\)\") or refer
  back to them (\"\\\\1\"), as combining items renumbers the groups, see below.
  Items which do are reported and skipped.
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
  - nil - Neither comments nor strings (other source-code).

  This limits the highlighting to only these parts of the text,
  where nil is used for anything that doesn't match a comment or string.

  A list of these symbols is also supported
  (allowing a single item to match multiple contexts).
`face':
  The face to apply.

Modifying this while the variable `hl-prog-extra-mode' is enabled requires calling
`hl-prog-extra-refresh' to update the internal state."
  :type
  '(repeat
    (list
     regexp
     integer
     (choice (const :tag "Comment (Any)" :value comment)
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
  "Include the default preset for the major mode (when available)."
  :type 'boolean)

(defcustom hl-prog-extra-global-ignore-modes nil
  "List of major-modes to exclude when `hl-prog-extra' has been enabled globally."
  :type '(repeat symbol))

(defvar-local hl-prog-extra-global-ignore-buffer nil
  "When non-nil, Global `hl-prog-extra' will not be enabled for this buffer.
This variable can also be a predicate function, in which case
it'll be called with one parameter (the buffer in question), and
it should return non-nil to exclude this buffer from Global `hl-prog-extra' Mode.")


;; ---------------------------------------------------------------------------
;; Internal Variables

(defvar-local hl-prog-extra--data nil
  "Internal data used for `hl-prog-extra--match' to do font locking.")

;; NOTE: Accumulating values in the keyword parser doesn't have any
;; problems in the common case but isn't totally foolproof.
;; If parsing is interrupted for any reason (e.g. subsequent user input),
;; stale keyword highlighting data could be left in the stack.
;; To avoid using stale data, ensure the state is what we expect from the last call.
(defvar-local hl-prog-extra--data-match-stack nil
  "Internal data used for `hl-prog-extra--match' to do font locking.")
(defvar-local hl-prog-extra--data-match-stack-state nil
  "Internal data used to ensure a stale stack is never used.")


;; ---------------------------------------------------------------------------
;; Generic Utilities

(defun hl-prog-extra--match-first (match)
  "Return the first valid group from MATCH and its zero-based index."
  (declare (important-return-value t))
  ;; Skip the first two elements (group zero, which defines the entire match).
  (setq match (cddr match))
  (let ((i 0))
    (while (and match (null (car match)))
      (setq match (cddr match))
      (incf i))
    (cons match i)))

(defun hl-prog-extra--match-index-set (beg end index)
  "Set the match data from BEG to END at INDEX."
  (declare (important-return-value nil))
  (set-match-data (append (list beg end) (make-list (* 2 index) nil) (list beg end))))

(defun hl-prog-extra--regexp-valid-or-error (re)
  "Return nil if RE is a valid regexp, otherwise the error string."
  (declare (important-return-value t))
  (condition-case err
      (prog1 nil
        (string-match-p re ""))
    (error
     (error-message-string err))))

(defun hl-prog-extra--regexp-number-first (re re-search)
  "Return the number RE-SEARCH matches in RE, otherwise nil.
RE-SEARCH must place the number in its first group.
Matches which aren't part of the expression itself are skipped."
  (declare (important-return-value t))
  ;; NOTE: `save-match-data' since this runs when the mode is enabled,
  ;; where clobbering the caller's match data would be surprising.
  ;; This follows `regexp-opt-depth' which scans for groups the same way.
  (save-match-data
    (let ((pos 0)
          (result nil))
      (while (and (null result) (string-match re-search re pos))
        ;; NOTE: read the match before `subregexp-context-p' which overwrites the match data.
        (let ((beg (match-beginning 0))
              (group-num (string-to-number (match-string 1 re))))
          (setq pos (match-end 0))
          ;; Skip a false positive inside brackets or after an escaped backslash.
          ;;
          ;; NOTE: `subregexp-context-p' parses RE from its start on every check,
          ;; `regexp-opt-depth' avoids this by passing the end of the last group
          ;; it confirmed as START. That doesn't transfer here, the first
          ;; confirmed match ends this scan, so there is never a confirmed
          ;; position behind a check to pass, only false positives scan again.
          ;; In practice expressions with enough of them for the re-parsing to
          ;; matter are rare, so leave as-is.
          (when (subregexp-context-p re beg)
            (setq result group-num))))
      result)))

(defun hl-prog-extra--maybe-prefix (prefix msg)
  "Return MSG prefixed with PREFIX, or nil if MSG is nil."
  (declare (important-return-value t))
  (cond
   ((null msg)
    nil)
   (t
    (concat prefix msg))))

;; ---------------------------------------------------------------------------
;; Pre-Compute Font Locking

(defun hl-prog-extra--validate-keyword-item (item)
  "Validate ITEM, return a message or nil on success."
  (declare (important-return-value t))
  (let* ((item-context-valid-items
          (list
           'comment 'comment-only 'comment-doc ; Comments.
           'string 'string-only 'string-doc ; Strings.
           nil))

         ;; -------------------------
         ;; Check `re', 1st argument.

         (validate-re-fn
          (lambda (re)
            ;; NOTE: each check only runs once those before it pass,
            ;; the order matters since they build on each other.
            (let ((item-error nil)
                  (group-num nil))
              (cond
               ((null (stringp re))
                (format "expected a string, not a %S!" (type-of re)))

               ((setq item-error (hl-prog-extra--regexp-valid-or-error re))
                (format "invalid regex \"%s\"" item-error))

               ;; Each item is wrapped in a numbered group so its own numbering would
               ;; conflict, see `uniq-list-len' in `hl-prog-extra--precompute-regex'.
               ((setq group-num (hl-prog-extra--regexp-number-first re "\\\\(\\?\\([0-9]+\\):"))
                (format "explicitly numbered group \"\\(?%d:\" isn't supported" group-num))

               ;; A back reference is numbered the same way, so it refers to the group
               ;; wrapping this item (which is still open, an invalid back reference)
               ;; or to a group of an unrelated item, never the group the regex declares.
               ((setq group-num (hl-prog-extra--regexp-number-first re "\\\\\\([1-9]\\)"))
                (format "back reference \"\\%d\" isn't supported" group-num))))))

         ;; ---------------------------------
         ;; Check `re-subexpr', 2nd argument.

         (validate-re-subexpr-single-fn
          (lambda (re-subexpr)
            ;; `integerp' ensured by caller.
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
            ;; Caller ensures this is not a list.
            (catch 'error
              (unless (symbolp context)
                (throw 'error (format "expected a symbol or nil, not a %S" (type-of context))))
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
             ((listp context)
              (funcall validate-context-list-fn context))
             (t ; Expected to be a symbol.
              (funcall validate-context-single-fn context)))))

         ;; ---------------------------
         ;; Check `face', 4th argument.

         (validate-face-single-fn
          (lambda (face)
            (catch 'error
              (unless (or (facep face) (listp face))
                (throw
                 'error
                 (format
                  "expected a symbol, string, face, or list of face properties; %S is not known!"
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

              ;; Now check that the face list is compatible with `re-subexpr'.
              (unless (eq (length re-subexpr) (length face))
                (throw 'error "list lengths do not match!")))))

         (validate-face-fn
          (lambda (face re-subexpr)
            (cond
             ((null (listp re-subexpr))
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
       ;; Check `context' (3rd).
       (hl-prog-extra--maybe-prefix "3rd (context) " (funcall validate-context-fn context))
       ;; Check `face' (4th).
       (hl-prog-extra--maybe-prefix "4th (face) " (funcall validate-face-fn face re-subexpr))))))


(defun hl-prog-extra--precompute-keywords (face-vector)
  "Create data to pass to `font-lock-add-keywords' from FACE-VECTOR."
  (declare (important-return-value t))
  ;; The generated result will look something like this.
  ;; (list
  ;;   (list
  ;;     'hl-prog-extra--match
  ;;     (cons 1 (list 'font-lock-constant-face t t))
  ;;     (cons 2 (list 'font-lock-warning-face t t))
  ;;     (cons 3 (list 'font-lock-string-face t t))))
  ;;
  ;; There is a group for each face, `hl-prog-extra--match' sets the match data so only the
  ;; group of the matched face is set. Font lock applies every highlight it's given,
  ;; so the order of the groups isn't meaningful.
  (let ((keywords (list)))
    (dotimes (i (length face-vector))
      (let ((face (aref face-vector i)))

        ;; Without this, a face that is not yet loaded will raise an error in font lock.
        (when (or
               ;; Quote unquoted symbol.
               (and (symbolp face) (null (boundp face)))
               ;; Quote unquoted list.
               (and (consp face) (null (eq 'quote (car face)))))
          (setq face (list 'quote face)))

        ;; The first number is the regex-group to match (starting at 1).
        ;;
        ;; The two booleans after `face' are:
        ;; - OVERRIDE: if non-nil, allow overriding existing fontification.
        ;; - LAXMATCH: if non-nil, don't signal error when SUBEXP has no match.
        (push (cons (1+ i) (list face t t)) keywords)))

    (setq keywords (nreverse keywords))
    ;; Put the matching function at the head of the list.
    (push 'hl-prog-extra--match keywords)

    ;; There is only one highlighter at the moment.
    (list keywords)))

(defun hl-prog-extra--precompute-regex (syn-regex-list)
  "Pre-compute data from the SYN-REGEX-LIST.

Return a list of (regex-list face-vector uniq-vector is-complex-comment is-complex-string):

`regex-list':
  A list of 5 strings (or nil) containing grouped regex statements from SYN-REGEX-LIST
  for: comment-only, comment-doc, string-only, string-doc, and other contexts.
`face-vector':
  A vector of unique faces.
`uniq-vector':
  A vector aligned with the regex groups, each element is a list of
  (sub-expr . face-index) ordered by sub-expr, nil for group numbers
  which no item uses (taken by the groups an item's own regex declares).
`is-complex-comment':
  Non-nil when comment-only and comment-doc differ.
`is-complex-string':
  Non-nil when string-only and string-doc differ."
  (declare (important-return-value t))
  (let ((len (length syn-regex-list)))
    (let ((item-index 0)
          ;; Error checking.
          (item-error-prefix "hl-prog-extra: error parsing `hl-prog-extra-list'")

          ;; Group regexes by the context they search in.
          (re-comment-only (list))
          (re-comment-doc (list))
          (re-string-only (list))
          (re-string-doc (list))
          (re-rest (list))
          ;; Store whether the doc/only variables are different.
          ;; This is useful since calculating this information is expensive.
          (is-complex-comment nil)
          (is-complex-string nil)

          ;; Unique faces, used to build the arguments for font locking.
          (face-list (list))
          (face-list-contents (make-hash-table :test 'eq :size len))
          ;; Unique values aligned with the regex groups, each element is a list of
          ;; (sub-expr . face-index) for the item using that group, nil for an unused group.
          ;; The length doubles as the highest regex group number used so far.
          ;;
          ;; Each item is wrapped in an explicitly numbered group, any groups the item's own
          ;; regex declares then take the numbers directly following it. Sub-expressions are
          ;; looked up relative to the item's group, so an item's group number must always be
          ;; higher than every group number used before it, otherwise the numbers Emacs assigns
          ;; to the item's own groups don't line up and unrelated groups are highlighted.
          (uniq-list (list))
          (uniq-list-len 0)
          ;; Use `equal' since the keys are lists of (sub-expr . face-index) pairs.
          ;; Faces are keyed by `face-list-contents', which resolves them to an index first.
          (uniq-list-contents (make-hash-table :test 'equal :size len)))

      (pcase-dolist (`(,re ,re-subexpr ,context ,face) syn-regex-list)

        ;; Ensure the context is a list (users may provide a single symbol).
        ;; Supporting both is nice as it allows multiple contexts to be used at once.
        (unless (and context (listp context))
          (setq context (list context)))

        (let ((error-msg
               ;; Be strict here since any errors in font-locking are difficult for users to debug.
               (hl-prog-extra--validate-keyword-item (list re re-subexpr context face)))

              ;; Each sub-expression and the face to use for it.
              (sub-face-list nil)

              (uniq-index nil)
              (face-index nil))

          (cond
           (error-msg
            (message "%s: %s (item %d)" item-error-prefix error-msg item-index))
           (t ; No error.

            (unless (listp re-subexpr)
              ;; Move into a list to avoid duplicate code-paths.
              (setq re-subexpr (list re-subexpr))
              (setq face (list face)))

            (while re-subexpr
              (let ((re-sub (pop re-subexpr))
                    (face-sub (pop face)))

                ;; Note that a zero `re-sub' is not the same as nil,
                ;; since a zero group is needed for matching the first level of parentheses.

                (let ((key face-sub))
                  (setq face-index (gethash key face-list-contents))

                  (unless face-index
                    (setq face-index (hash-table-count face-list-contents))
                    (push face-sub face-list)
                    (puthash key face-index face-list-contents)))

                (push (cons re-sub face-index) sub-face-list)))

            ;; Order by sub-expression, which is the order in the buffer since groups are
            ;; numbered from left to right. The match stack relies on this as it steps forward.
            ;;
            ;; NOTE: not `car-less-than-car', only `sort.el' defines it (without an autoload),
            ;; which Emacs 30 pre-dumps, masking the missing `require' on older releases.
            (setq sub-face-list (sort (nreverse sub-face-list) (lambda (a b) (< (car a) (car b)))))

            ;; A sub-expression repeated with the same face highlights one region twice,
            ;; where the duplicate only takes a slot in the match stack.
            (setq sub-face-list (delete-dups sub-face-list))

            ;; The group may only be shared between items which use the whole match,
            ;; which only ever reads the shared group. Groups a sharing item's own regex
            ;; declares don't line up with the slots padded for them (Emacs numbers them
            ;; from the highest number in the combined regex so far, not from the shared
            ;; group), which is harmless as nothing reads them, while the padding keeps
            ;; every later item's group above the numbers they can be assigned.
            (let ((uniq-is-shared
                   (and (null (cdr sub-face-list)) (zerop (car (car sub-face-list))))))
              (when uniq-is-shared
                (setq uniq-index (gethash sub-face-list uniq-list-contents)))

              (unless uniq-index
                (setq uniq-index uniq-list-len)
                (push sub-face-list uniq-list)
                (incf uniq-list-len)
                (when uniq-is-shared
                  (puthash sub-face-list uniq-index uniq-list-contents))))

            ;; Reserve the group numbers Emacs assigns to the groups this item's regex
            ;; declares, they follow on from the highest number used so far.
            ;; NOTE: `regexp-opt-depth' doesn't count explicitly numbered groups, so a regex
            ;; which numbers its own groups isn't supported, the numbers wouldn't line up.
            (dotimes (_ (regexp-opt-depth re))
              (push nil uniq-list)
              (incf uniq-list-len))

            ;; Group the regex into a larger numbered regex using \\(?NUM:...).
            ;; These expressions are then joined to make a single regex
            ;; (per-unique context combination) outside of this loop.
            (let ((regex-fmt (format "\\(?%d:%s\\)" (1+ uniq-index) re))
                  (in-comment-only nil)
                  (in-comment-doc nil)
                  (in-string-only nil)
                  (in-string-doc nil)
                  (in-rest nil))
              (dolist (context-symbol context)
                (cond
                 ((eq context-symbol 'comment)
                  (setq in-comment-only t)
                  (setq in-comment-doc t))
                 ((eq context-symbol 'comment-only)
                  (setq in-comment-only t))
                 ((eq context-symbol 'comment-doc)
                  (setq in-comment-doc t))
                 ((eq context-symbol 'string)
                  (setq in-string-only t)
                  (setq in-string-doc t))
                 ((eq context-symbol 'string-only)
                  (setq in-string-only t))
                 ((eq context-symbol 'string-doc)
                  (setq in-string-doc t))
                 ((null context-symbol)
                  (setq in-rest t))
                 (t ; Checked for above.
                  (error "Invalid context %S" context-symbol))))

              ;; Take this from the contexts the item ends up in, not the symbols naming
              ;; them. An overlapping list such as `(comment comment-only)' reaches both,
              ;; where checking for a documentation comment can only cost time.
              (unless (eq in-comment-only in-comment-doc)
                (setq is-complex-comment t))
              (unless (eq in-string-only in-string-doc)
                (setq is-complex-string t))

              ;; NOTE: take the contexts as a set. An overlapping list such as
              ;; `(comment comment-only)' would otherwise name this item twice in the
              ;; same regex, where the duplicate can never match anything the first
              ;; doesn't, but its groups still take numbers no item reserved.
              (when in-comment-only
                (push regex-fmt re-comment-only))
              (when in-comment-doc
                (push regex-fmt re-comment-doc))
              (when in-string-only
                (push regex-fmt re-string-only))
              (when in-string-doc
                (push regex-fmt re-string-doc))
              (when in-rest
                (push regex-fmt re-rest))))))

        (incf item-index))

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
  ;; since including overlays causes `hl-line-mode' (for example) to mask other faces.
  ;; If we want to include faces of overlays, this could be supported.
  (declare (important-return-value t))
  (let ((faceprop (get-text-property pos 'face)))
    (cond
     ((facep faceprop)
      (eq faceprop face-test))
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
  "Return t when the comment or string is a doc-string or doc-comment at STATE."
  (declare (important-return-value t))
  (let ((start (ppss-comment-or-string-start state)))
    (hl-prog-extra--check-face-at-point start 'font-lock-doc-face)))

(defun hl-prog-extra--match-impl (bound)
  "MATCHER for the font lock keyword in `hl-prog-extra--data', until BOUND."
  (declare (important-return-value t))
  (let ((found nil)
        (state-at-pt (syntax-ppss))
        (state-at-pt-next nil)
        (info (car hl-prog-extra--data))
        ;; Always case sensitive.
        (case-fold-search nil))
    ;; Unpack the first element of info; the remainder unpacks from the rest.
    (pcase-let ((`(,`(,re-comment-only ,re-comment-doc ,re-string-only ,re-string-doc ,re-rest)
                   ,_
                   ,uniq-array
                   ,is-complex-comment
                   ,is-complex-string)
                 info))
      (while (and (null found) (< (point) bound))
        (let ((re-context
               (cond
                ((ppss-string-terminator state-at-pt)
                 (cond
                  ((and is-complex-string (hl-prog-extra--is-doc-state-p state-at-pt))
                   re-string-doc)
                  (t
                   re-string-only)))
                ((ppss-comment-depth state-at-pt)
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

              ;; Without this, the beginning of a comment is seen as code.
              ;; In practice this means C-style comments such as `/*XXX*/'
              ;; end up considering the first two characters as code.
              ;; This causes problems if we want to highlight operators,
              ;; e.g.: `*' or `/' characters.
              ;; Avoid this by further clamping the context not to step
              ;; into the beginning of a comment or string.
              ;; Note that this introduces 'gaps', where the beginnings of
              ;; comments can't be matched.
              ;; Properly adjusting all beginnings and endings ends up being quite
              ;; complex since state isn't maintained between calls to this function.
              ;; So unless there is an important use-case for this, accept the limitation since
              ;; not being able to properly match symbols in code is a much larger limitation.
              (when (or (ppss-string-terminator state-at-pt-next)
                        (ppss-comment-depth state-at-pt-next))
                (let ((comment-or-string-start (ppss-comment-or-string-start state-at-pt-next)))
                  (when (<= (point) comment-or-string-start)
                    (setq bound-context-clamp comment-or-string-start))))

              (cond
               ((re-search-forward re-context (or bound-context-clamp bound-context) t)
                (setq found t)
                ;; The `uniq-index' is always needed to find the original font
                ;; and to check for a sub-expression.
                ;;
                ;; NOTE: request integers so a marker isn't created for every group in the
                ;; combined regex. This appends the buffer to the list, which is never reached
                ;; since the group wrapping this item always matches.
                (pcase-let* ((`(,match-tail . ,uniq-index)
                              (hl-prog-extra--match-first (match-data t)))
                             (`(,beg-final ,end-final) match-tail)
                             (uniq-data (aref uniq-array uniq-index))
                             (beg-end-index-list (list)))

                  ;; Extract the region of each sub-expression, skipping those that didn't
                  ;; match (they may be optional or out of range).
                  (pcase-dolist (`(,sub-expr . ,face-index) uniq-data)
                    (pcase-let ((`(,beg ,end) (nthcdr (* 2 sub-expr) match-tail)))
                      (when (and beg end)
                        (push (list beg end face-index) beg-end-index-list))))
                  (setq beg-end-index-list (nreverse beg-end-index-list))

                  (cond
                   ;; Nothing to highlight.
                   ((null beg-end-index-list)
                    (cond
                     ;; A single sub-expression falls back to the whole match, since the
                     ;; sub-expression may be out of range and raising an error during
                     ;; font-locking isn't practical.
                     ((and uniq-data (null (cdr uniq-data)))
                      (hl-prog-extra--match-index-set beg-final end-final (cdr (car uniq-data))))
                     (t
                      ;; Clear the groups so the match data left by the search isn't used,
                      ;; as its groups don't correspond to the faces of this item.
                      (set-match-data (list beg-final end-final)))))

                   ;; A single region, use simple handling (nothing clever).
                   ((null (cdr beg-end-index-list))
                    (pcase-let ((`(,beg ,end ,face-index) (car beg-end-index-list)))
                      (hl-prog-extra--match-index-set beg end face-index)))

                   ;; Multiple regions, handle the first, store the rest
                   ;; in the stack to be returned before further searching.
                   (t
                    (setq hl-prog-extra--data-match-stack beg-end-index-list)
                    (hl-prog-extra--match-impl-precalc bound)
                    ;; Don't step into the next context, allow for the remaining
                    ;; items in the stack to be handled first.
                    (setq bound-context-clamp nil))))

                (when bound-context-clamp
                  ;; If the clamped bounds are met, step to the unclamped bounds.
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
BOUND is only used to validate the state."
  (declare (important-return-value nil))
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
                ;; Check that this is unchanged on re-entry.
                (cons (point) bound))
               (t
                ;; Go to the end since there is no further data to parse.
                (goto-char (min end bound))
                nil)))
        ;; Found.
        t))
     (t
      nil))))

(defun hl-prog-extra--match (bound)
  "MATCHER for the font lock keyword in `hl-prog-extra--data', until BOUND."
  (declare (important-return-value t))

  (when hl-prog-extra--data-match-stack
    ;; Ensure stale data from this stack is never used (even though it's unlikely).
    ;; See `hl-prog-extra--data-match-stack-state' for details.
    (when (or
           ;; Should never happen, added check for extra safety.
           (null hl-prog-extra--data-match-stack-state)
           ;; The point moved since last search.
           (null (eq (point) (car hl-prog-extra--data-match-stack-state)))
           ;; The bound moved since last search.
           (null (eq bound (cdr hl-prog-extra--data-match-stack-state))))

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
  "Load a preset for the current mode.
ARGS: The first two arguments are positional.
The first is MODE-VALUE to override the current `major-mode'.
The second is QUIET; when non-nil, don't show a message
when the preset isn't found.
The rest are expected to be keyword arguments
to control the behavior of each preset;
see each preset's documentation for available keywords."
  (declare (important-return-value t))
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
            (_ (error "Too many positional arguments")))
          (incf args-count)
          (setq args (cdr args))))))

    (unless mode-value
      (setq mode-value (symbol-name major-mode)))
    ;; NOTE: `intern-soft' could be used to avoid adding symbols to the obarray
    ;; for presets that don't exist, however `require' is still needed to load
    ;; the feature, so error handling remains necessary either way.
    (let ((preset-sym (intern (concat "hl-prog-extra-preset-" mode-value))))
      (when (condition-case err
                (require preset-sym nil 'noerror)
              (error
               (lwarn
                'hl-prog-extra
                :error "preset %S failed: %s" mode-value (error-message-string err))
               nil))
        (cond
         ((fboundp preset-sym)
          (apply preset-sym args))
         (t
          (lwarn
           'hl-prog-extra
           :error "preset %S loaded but did not define function `%S'" mode-value preset-sym)
          ;; The caller appends this to `hl-prog-extra-list',
          ;; returning the string from `lwarn' would create a malformed list.
          nil))))))


;; ---------------------------------------------------------------------------
;; Define Minor Mode

(defun hl-prog-extra--mode-enable ()
  "Turn on option `hl-prog-extra-mode' for the current buffer."
  (declare (important-return-value nil))
  ;; Unlikely, but remove any existing keywords to avoid duplicates.
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
  (declare (important-return-value nil))
  (when hl-prog-extra--data
    (font-lock-remove-keywords nil (cdr hl-prog-extra--data))
    (font-lock-flush))
  (kill-local-variable 'hl-prog-extra--data)
  (kill-local-variable 'hl-prog-extra--data-match-stack)
  (kill-local-variable 'hl-prog-extra--data-match-stack-state))

;;;###autoload
(defun hl-prog-extra-refresh ()
  "Update internal data after changing `hl-prog-extra-list'."
  (declare (important-return-value nil))
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
  (declare (important-return-value nil))
  (when (and
         ;; Not already enabled.
         (not hl-prog-extra-mode)
         ;; Not in the mini-buffer.
         (null (minibufferp))
         ;; Not a special mode (package list, tabulated data, ...)
         ;; The buffer should be derived from `text-mode' or `prog-mode'.
         (null (derived-mode-p 'special-mode))
         ;; Not explicitly ignored.
         (null (memq major-mode hl-prog-extra-global-ignore-modes))
         ;; If `hl-prog-extra-global-ignore-buffer' is a function, call it.
         (or (null hl-prog-extra-global-ignore-buffer)
             (cond
              ((functionp hl-prog-extra-global-ignore-buffer)
               (null (funcall hl-prog-extra-global-ignore-buffer (current-buffer))))
              (t
               nil))))
    (hl-prog-extra-mode 1)))

;;;###autoload
(define-globalized-minor-mode hl-prog-extra-global-mode
  hl-prog-extra-mode
  hl-prog-extra--mode-turn-on)

(define-obsolete-function-alias 'global-hl-prog-extra-mode #'hl-prog-extra-global-mode "0.2")

(provide 'hl-prog-extra)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-format-quoted: nil
;; End:
;;; hl-prog-extra.el ends here
