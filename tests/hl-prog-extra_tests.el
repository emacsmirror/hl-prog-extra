;;; hl-prog-extra_tests.el --- Testing -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-2.0-or-later

;;; Commentary:

;; See: `hl-prog-extra_tests.py' for launching this script.

(require 'ert)

;;; Code:

(defvar hl-prog-extra-tests-basedir (concat (file-name-directory load-file-name) "..")
  "Base directory for hl-prog-extra tests.")
(add-to-list 'load-path hl-prog-extra-tests-basedir)
(add-to-list 'load-path (file-name-directory load-file-name))
(require 'hl-prog-extra)
(require 'hl-prog-extra_html)


;; ---------------------------------------------------------------------------
;; Internal Functions/Macros

;; NOTE: tests use dedicated faces instead of the values in `hl-prog-extra-list'
;; so the expected HTML stays short and doesn't change when the defaults change.
(defface hl-prog-extra-test-a '((t :foreground "#FF0000"))
  "Face for testing."
  :group 'hl-prog-extra)
(defface hl-prog-extra-test-b '((t :foreground "#00FF00"))
  "Face for testing."
  :group 'hl-prog-extra)
(defface hl-prog-extra-test-c '((t :foreground "#0000FF"))
  "Face for testing."
  :group 'hl-prog-extra)

;; Faces for checking how specs are resolved, defined here rather than using the
;; faces of a major-mode, whose definitions change between Emacs versions.
(defface hl-prog-extra-test-inherit '((default :inherit hl-prog-extra-test-a))
  "Face for testing, inheriting through a `default' spec entry."
  :group 'hl-prog-extra)
(defface hl-prog-extra-test-display
  '((((class color) (min-colors 88) (background light)) :foreground "#111111")
    (((class color) (min-colors 88) (background dark)) :foreground "#222222")
    (((class grayscale)) :foreground "#333333")
    (t :weight bold))
  "Face for testing, defined per display."
  :group 'hl-prog-extra)
(defface hl-prog-extra-test-display-graphic
  '((((type graphic) (class color)) :foreground "#444444")
    (t :weight bold))
  "Face for testing, using the `graphic' display type."
  :group 'hl-prog-extra)
(defface hl-prog-extra-test-inherit-list
  '((t :inherit (hl-prog-extra-test-a hl-prog-extra-test-b)))
  "Face for testing, inheriting from a list of faces."
  :group 'hl-prog-extra)
(defface hl-prog-extra-test-empty '((t nil))
  "Face for testing, defining no attributes."
  :group 'hl-prog-extra)

(defmacro with-hl-prog-extra-test (initial-buffer-text mode &rest body)
  "Run BODY in a buffer using MODE, set to INITIAL-BUFFER-TEXT."
  (declare (indent 2))
  `(with-temp-buffer
     (let ((inhibit-message t))
       (insert ,initial-buffer-text)
       (funcall ,mode)
       (hl-prog-extra-mode 1)
       (font-lock-ensure)
       ,@body)))


;; ---------------------------------------------------------------------------
;; HTML Output

;; Each test's buffer is written out as a document, to check the highlighting by
;; eye. Unlike the text the tests compare, these include the major-mode's own
;; faces, showing what the highlighting looks like in use.

(defvar hl-prog-extra-test-html-dir (getenv "HL_PROG_EXTRA_TEST_HTML_DIR")
  "Directory to write each test's buffer into as a HTML document.
Taken from the environment so this works however the tests are launched.
Nothing is written when unset, the tests behave the same either way.")

(defvar hl-prog-extra-test-html-file-list nil
  "Documents written this run as (FILENAME . DESCRIPTION), most recent first.")

(defvar hl-prog-extra-test-html-name nil
  "Name of the test `hl-prog-extra-test-html-index' counts within.")

(defvar hl-prog-extra-test-html-index 0
  "Number of documents the current test has written.")

(defun hl-prog-extra-test-html-write ()
  "Write the current buffer into `hl-prog-extra-test-html-dir'."
  (when hl-prog-extra-test-html-dir
    (let* ((test (ert-running-test))
           ;; The name is only unset when called outside a test, which the
           ;; helpers here never do, handle it rather than write "nil.html".
           (name
            (cond
             (test
              (symbol-name (ert-test-name test)))
             (t
              "unknown")))
           ;; A test checking several inputs writes a document for each,
           ;; the first keeps the plain name so the common case reads well.
           (index
            (cond
             ((equal name hl-prog-extra-test-html-name)
              (setq hl-prog-extra-test-html-index (1+ hl-prog-extra-test-html-index)))
             (t
              (setq hl-prog-extra-test-html-name name)
              (setq hl-prog-extra-test-html-index 1))))
           (id
            (cond
             ((eq index 1)
              name)
             (t
              (format "%s-%d" name index))))
           (filename (concat id ".html"))
           (doc (and test (ert-test-documentation test))))
      (push
       (cons filename (and doc (car (split-string doc "\n")))) hl-prog-extra-test-html-file-list)
      (let ((text (hl-prog-extra-html-export-document id)))
        (with-temp-file (expand-file-name filename hl-prog-extra-test-html-dir)
          (insert text))))))

(defun hl-prog-extra-test-html-write-index ()
  "Write an index linking each document written this run."
  (when hl-prog-extra-test-html-file-list
    (let ((file (expand-file-name "index.html" hl-prog-extra-test-html-dir)))
      (with-temp-file file
        (insert
         "<!DOCTYPE html>\n<html>\n<head>\n<meta charset='utf-8'>\n"
         "<title>hl-prog-extra tests</title>\n</head>\n<body>\n<ul>\n")
        (pcase-dolist (`(,filename . ,doc) (reverse hl-prog-extra-test-html-file-list))
          (insert
           (format "<li><a href='%s'>%s</a>%s</li>\n"
                   filename (hl-prog-extra-html--escape (file-name-base filename))
                   (cond
                    (doc
                     (concat " - " (hl-prog-extra-html--escape doc)))
                    (t
                     "")))))
        (insert "</ul>\n</body>\n</html>\n"))
      (message "Written: %s" file))))

(when hl-prog-extra-test-html-dir
  (make-directory hl-prog-extra-test-html-dir t)
  ;; NOTE: the tests that run are not known up-front (a selector may skip most
  ;; of them), so the index can only be written once the run has finished.
  (add-hook 'kill-emacs-hook #'hl-prog-extra-test-html-write-index))

(defun hl-prog-extra-test-html (&optional face-list inline-style)
  "Return the HTML for the current buffer, limited to `hl-prog-extra' faces.
FACE-LIST overrides the faces to wrap, needed once the mode has been disabled
and the buffer no longer reports them.
INLINE-STYLE resolves named faces to their attributes."
  ;; NOTE: the filter is by face, not by which keyword applied it. A test using
  ;; a face the major-mode also uses (`font-lock-constant-face' for e.g.) would
  ;; wrap the mode's own text too, so tests use their own faces.
  (hl-prog-extra-test-html-write)
  (hl-prog-extra-html-export (or face-list (hl-prog-extra-html-face-list)) inline-style t))


;; ---------------------------------------------------------------------------
;; Load Tests

(ert-deftest load ()
  "Check the package loads."
  (should (featurep 'hl-prog-extra)))


;; ---------------------------------------------------------------------------
;; HTML Export Tests

(ert-deftest html-export-escape ()
  "Check HTML special characters in the buffer text are escaped."
  (let ((hl-prog-extra-list nil)
        (text-initial "a & b < c > d")
        (text-expected "a &amp; b &lt; c &gt; d"))
    (with-hl-prog-extra-test text-initial #'fundamental-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))

(ert-deftest html-export-file ()
  "Check the document export used by \"make view\"."
  ;; NOTE: the other checks limit the export to the packages own faces, this is
  ;; the only one taking the path that resolves every face of the major-mode,
  ;; where an unusable value would otherwise go unnoticed until viewing a file.
  (let ((file-in (make-temp-file "hl-prog-extra-tests" nil ".c"))
        (file-out (make-temp-file "hl-prog-extra-tests" nil ".html"))
        (hl-prog-extra-list (list (list "\\<TODO\\>" 0 'comment 'hl-prog-extra-test-a))))
    (unwind-protect
        (let ((inhibit-message t))
          (with-temp-file file-in
            (insert "int i; /* TODO */\n"))
          (hl-prog-extra-html-export-file file-in file-out)
          (let ((text
                 (with-temp-buffer
                   (insert-file-contents file-out)
                   (buffer-string))))
            (should (string-prefix-p "<!DOCTYPE html>" text))
            (should (string-suffix-p "</html>\n" text))
            ;; Faces are written inline, the document needs no style-sheet.
            (should (string-match-p "<span style='color: #FF0000'>TODO</span>" text))
            ;; The major-mode's own faces are resolved to a color too, the point
            ;; of the display matching. Don't check which, modes change.
            (should (string-match-p "<span style='color: #[0-9A-F]\\{6\\}'>int</span>" text))))
      (delete-file file-in)
      (delete-file file-out))))

(ert-deftest html-export-no-match ()
  "Check text without a match is written without markup."
  (let ((hl-prog-extra-list (list (list "\\<TODO\\>" 0 'comment 'hl-prog-extra-test-a)))
        (text-initial "/* nothing */")
        (text-expected "/* nothing */"))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))


;; ---------------------------------------------------------------------------
;; Context Tests

(ert-deftest context-comment ()
  "Check a match inside a comment is highlighted."
  (let ((hl-prog-extra-list (list (list "\\<TODO\\>" 0 'comment 'hl-prog-extra-test-a)))
        (text-initial "/* TODO */")
        (text-expected "/* <span class='hl-prog-extra-test-a'>TODO</span>\n */"))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))

(ert-deftest context-comment-excludes-code ()
  "Check the `comment' context does not match in code."
  (let ((hl-prog-extra-list (list (list "\\<TODO\\>" 0 'comment 'hl-prog-extra-test-a)))
        (text-initial "int TODO = 1; /* TODO */")
        (text-expected "int TODO = 1; /* <span class='hl-prog-extra-test-a'>TODO</span>\n */"))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))

(ert-deftest context-comment-multi-line ()
  "Check a match on a later line of a multi-line comment is highlighted."
  (let ((hl-prog-extra-list (list (list "\\<TODO\\>" 0 'comment 'hl-prog-extra-test-a)))
        (text-initial "/* one\n   TODO\n*/\n")
        (text-expected "/* one\n   <span class='hl-prog-extra-test-a'>TODO</span>\n\n*/\n"))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))

(ert-deftest context-string ()
  "Check a match inside a string is highlighted."
  (let ((hl-prog-extra-list (list (list "\\<TODO\\>" 0 'string 'hl-prog-extra-test-a)))
        (text-initial "char *s = \"TODO\";")
        (text-expected "char *s = \"<span class='hl-prog-extra-test-a'>TODO</span>\n\";"))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))

(ert-deftest context-nil-is-code-only ()
  "Check a nil context matches code but not comments or strings."
  (let ((hl-prog-extra-list (list (list "\\<TODO\\>" 0 nil 'hl-prog-extra-test-a)))
        (text-initial "int TODO = 1; /* TODO */")
        (text-expected "int <span class='hl-prog-extra-test-a'>TODO</span>\n = 1; /* TODO */"))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))

(ert-deftest context-multiple ()
  "Check a list of contexts matches each of them."
  (let ((hl-prog-extra-list
         (list (list "\\<TODO\\>" 0 (list 'comment 'string) 'hl-prog-extra-test-a)))
        (text-initial "/* TODO */ char *s = \"TODO\";")
        (text-expected
         (concat
          "/* <span class='hl-prog-extra-test-a'>TODO</span>\n */ "
          "char *s = \"<span class='hl-prog-extra-test-a'>TODO</span>\n\";")))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))


;; ---------------------------------------------------------------------------
;; Documentation Context Tests

;; NOTE: these exercise the `is-complex-comment' and `is-complex-string'
;; code-paths, where a comment or string has to be classified as documentation
;; before it can be searched.

(defconst hl-prog-extra-test-lisp-text
  (concat
   "(defun f ()\n" ;
   "  \"XX\"\n" ;
   "  (message \"XX\") ; XX\n" ;
   "  nil)")
  "Lisp with the same word in a doc-string, a plain string and a comment.")

(ert-deftest context-overlapping ()
  "Check an overlapping context list doesn't shift a later item's sub-expression."
  ;; Both symbols select the plain comment context, where naming the item twice
  ;; used to take group numbers the second item had already reserved.
  (let ((hl-prog-extra-list
         (list
          (list
           "A\\(x\\)\\(y\\)" 0 (list 'comment 'comment-only) 'hl-prog-extra-test-a)
          (list "B\\([a-z]\\)" 1 'comment 'hl-prog-extra-test-b)))
        (text-initial "/* Axy Bz */")
        (text-expected
         (concat
          "/* <span class='hl-prog-extra-test-a'>Axy</span>\n" ;
          " B<span class='hl-prog-extra-test-b'>z</span>\n */")))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))

(ert-deftest context-overlapping-is-not-complex ()
  "Check the documentation check is only used when the two contexts differ."
  (dolist (case
           (list
            ;; Reaching both comment contexts leaves nothing to tell apart.
            (cons 'comment nil)
            (cons (list 'comment 'comment-only) nil)
            (cons (list 'comment-only 'comment-doc) nil)
            ;; Only one of the two, so the contexts differ.
            (cons 'comment-only t)
            (cons 'comment-doc t)))
    (pcase-let ((`(,context . ,expected) case))
      (ert-info
       ((format "context %S" context))
       (pcase-let ((`(,_re-list ,_face-vector ,_uniq-vector ,is-complex-comment ,_)
                    (hl-prog-extra--precompute-regex
                     (list (list "\\<XX\\>" 0 context 'hl-prog-extra-test-a)))))
         (should (eq expected (and is-complex-comment t))))))))

(ert-deftest context-string-doc ()
  "Check `string-doc' matches a documentation string and not a plain string."
  (let ((hl-prog-extra-list (list (list "\\<XX\\>" 0 'string-doc 'hl-prog-extra-test-a)))
        (text-expected
         (concat
          "(defun f ()\n" ;
          "  \"<span class='hl-prog-extra-test-a'>XX</span>\n\"\n" ;
          "  (message \"XX\") ; XX\n" ;
          "  nil)")))
    (with-hl-prog-extra-test hl-prog-extra-test-lisp-text #'emacs-lisp-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))

(ert-deftest context-string-only ()
  "Check `string-only' matches a plain string and not a documentation string."
  (let ((hl-prog-extra-list (list (list "\\<XX\\>" 0 'string-only 'hl-prog-extra-test-a)))
        (text-expected
         (concat
          "(defun f ()\n" ;
          "  \"XX\"\n" ;
          "  (message \"<span class='hl-prog-extra-test-a'>XX</span>\n\") ; XX\n" ;
          "  nil)")))
    (with-hl-prog-extra-test hl-prog-extra-test-lisp-text #'emacs-lisp-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))

(ert-deftest context-string-matches-both ()
  "Check `string' matches documentation and plain strings."
  (let ((hl-prog-extra-list (list (list "\\<XX\\>" 0 'string 'hl-prog-extra-test-a)))
        (text-expected
         (concat
          "(defun f ()\n" ;
          "  \"<span class='hl-prog-extra-test-a'>XX</span>\n\"\n" ;
          "  (message \"<span class='hl-prog-extra-test-a'>XX</span>\n\") ; XX\n" ;
          "  nil)")))
    (with-hl-prog-extra-test hl-prog-extra-test-lisp-text #'emacs-lisp-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))

(ert-deftest context-comment-only ()
  "Check `comment-only' matches a comment and neither kind of string."
  (let ((hl-prog-extra-list (list (list "\\<XX\\>" 0 'comment-only 'hl-prog-extra-test-a)))
        (text-expected
         (concat
          "(defun f ()\n" ;
          "  \"XX\"\n" ;
          "  (message \"XX\") ; <span class='hl-prog-extra-test-a'>XX</span>\n\n" ;
          "  nil)")))
    (with-hl-prog-extra-test hl-prog-extra-test-lisp-text #'emacs-lisp-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))


;; ---------------------------------------------------------------------------
;; Sub-Expression Tests

(ert-deftest subexpr-group ()
  "Check a non-zero sub-expression limits the highlight to that group."
  (let ((hl-prog-extra-list (list (list "<\\([a-z]+\\)>" 1 'comment 'hl-prog-extra-test-a)))
        (text-initial "/* <word> */")
        (text-expected "/* &lt;<span class='hl-prog-extra-test-a'>word</span>\n&gt; */"))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))

(ert-deftest subexpr-multiple ()
  "Check a list of sub-expressions highlights each with its own face."
  (let ((hl-prog-extra-list
         (list
          (list
           "\\([a-z]+\\)=\\([a-z]+\\)"
           (list 1 2)
           'comment
           (list 'hl-prog-extra-test-a 'hl-prog-extra-test-b))))
        (text-initial "/* key=value */")
        (text-expected
         (concat
          "/* <span class='hl-prog-extra-test-a'>key</span>\n"
          "=<span class='hl-prog-extra-test-b'>value</span>\n */")))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))

;; NOTE: the sub-expressions of a single match are returned one at a time from a
;; stack which steps forward through the buffer, so they must be ordered by group
;; and not by the order they happen to be listed in.

(ert-deftest subexpr-multiple-out-of-order ()
  "Check sub-expressions listed in descending order are all highlighted."
  (let ((hl-prog-extra-list
         (list
          (list
           "\\([a-z]+\\)=\\([a-z]+\\)"
           (list 2 1)
           'comment
           (list 'hl-prog-extra-test-b 'hl-prog-extra-test-a))))
        (text-initial "/* key=value */")
        ;; The same result as listing the sub-expressions in ascending order,
        ;; the order they are given in doesn't change what is highlighted.
        (text-expected
         (concat
          "/* <span class='hl-prog-extra-test-a'>key</span>\n" ;
          "=<span class='hl-prog-extra-test-b'>value</span>\n */")))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))

(ert-deftest subexpr-multiple-out-of-order-shuffled ()
  "Check sub-expressions listed in an arbitrary order are all highlighted."
  ;; Unlike a descending list, neither this order nor its reverse is sorted,
  ;; so handling that only reverses the list would still fail here.
  (let ((hl-prog-extra-list
         (list
          (list
           "\\([a-z]+\\)=\\([a-z]+\\):\\([a-z]+\\)"
           (list 3 1 2)
           'comment
           (list 'hl-prog-extra-test-c 'hl-prog-extra-test-a 'hl-prog-extra-test-b))))
        (text-initial "/* key=value:extra */")
        (text-expected
         (concat
          "/* <span class='hl-prog-extra-test-a'>key</span>\n" ;
          "=<span class='hl-prog-extra-test-b'>value</span>\n" ;
          ":<span class='hl-prog-extra-test-c'>extra</span>\n */")))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))

(ert-deftest subexpr-multiple-order ()
  "Check sub-expressions are returned ordered by group, not as they are listed."
  ;; Either order highlights the same text, so step the matcher by hand to check
  ;; the order it reports them in, which nothing else here depends on.
  (let ((hl-prog-extra-list
         (list
          (list
           "\\([a-z]+\\)=\\([a-z]+\\)"
           (list 2 1)
           'comment
           (list 'hl-prog-extra-test-b 'hl-prog-extra-test-a))))
        (text-initial "/* key=value */"))
    (with-hl-prog-extra-test text-initial #'c-mode
      (let ((match-list (list)))
        (goto-char (point-min))
        (while (hl-prog-extra--match (point-max))
          (push (match-string-no-properties 0) match-list)
          ;; Step over a match that didn't move the point, as font lock does.
          (unless (< (match-beginning 0) (point))
            (forward-char 1)))
        (should (equal (list "key" "value") (nreverse match-list)))))))

(ert-deftest subexpr-multiple-optional-none-match ()
  "Check a match none of whose sub-expressions were found is left alone."
  ;; NOTE: a third face is needed to see this. The group the search left set was
  ;; used as a face index, which only names a face once the table has enough
  ;; entries, so the last item's face was applied to the whole match.
  (let ((hl-prog-extra-list
         (list
          (list
           "X\\(a\\)?\\(b\\)?"
           (list 1 2)
           'comment
           (list 'hl-prog-extra-test-a 'hl-prog-extra-test-b))
          (list "\\<ZZ\\>" 0 'comment 'hl-prog-extra-test-c)))
        (text-initial "/* X ZZ */")
        ;; Neither optional group matched, so the first item highlights nothing.
        (text-expected "/* X <span class='hl-prog-extra-test-c'>ZZ</span>\n */"))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))

(ert-deftest subexpr-duplicate-is-merged ()
  "Check a sub-expression repeated with the same face is only stored once."
  ;; The duplicate highlights one region twice and takes a slot in the match
  ;; stack, which relies on each of its entries stepping forward in the buffer.
  (pcase-let ((`(,_re-list ,_face-vector ,uniq-vector ,_ ,_)
               (hl-prog-extra--precompute-regex
                (list
                 (list
                  "\\(a\\)b" (list 1 1) 'comment
                  (list 'hl-prog-extra-test-a 'hl-prog-extra-test-a))))))
    ;; One entry for the item, followed by the group its own regex declares.
    (should (equal (vector (list (cons 1 0)) nil) uniq-vector))))

;; NOTE: each item is wrapped in a numbered regex group and its sub-expression is
;; looked up relative to that group, so an item's number must be above every
;; number used before it. These check the cases where that used to break down,
;; the whole match was highlighted instead of the sub-expression.

(ert-deftest subexpr-group-face-reused ()
  "Check items sharing a face and sub-expression keep their own groups."
  (let ((hl-prog-extra-list
         (list
          (list "<\\([a-z]+\\)>" 1 'comment 'hl-prog-extra-test-a)
          (list "{\\([a-z]+\\)}" 1 'comment 'hl-prog-extra-test-a)))
        (text-initial "/* <one> {two} */")
        (text-expected
         (concat
          "/* &lt;<span class='hl-prog-extra-test-a'>one</span>\n" ;
          "&gt; {<span class='hl-prog-extra-test-a'>two</span>\n} */")))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))

(ert-deftest subexpr-group-after-item-with-groups ()
  "Check an item's own groups don't shift the sub-expression of a later item."
  ;; The first item declares two groups of its own, which take the numbers
  ;; following it, the second item must be numbered above them.
  (let ((hl-prog-extra-list
         (list
          (list "(\\([a-z]+\\))\\(!\\)" 1 'comment 'hl-prog-extra-test-a)
          (list "\\[\\([a-z]+\\)\\]" 1 'comment 'hl-prog-extra-test-b)))
        (text-initial "/* (one)! [two] */")
        (text-expected
         (concat
          "/* (<span class='hl-prog-extra-test-a'>one</span>\n" ;
          ")! [<span class='hl-prog-extra-test-b'>two</span>\n] */")))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))

(ert-deftest subexpr-group-after-item-multiple ()
  "Check an item with multiple sub-expressions doesn't shift a later item."
  ;; However many sub-expressions an item uses, they share one entry, what is
  ;; reserved after it is one entry for each group the item's own regex declares.
  ;; This regex declares more groups than the item lists sub-expressions, so a
  ;; count of either one alone would leave a gap the second item can't be read
  ;; across.
  (let ((hl-prog-extra-list
         (list
          (list
           "@\\([a-z]+\\)=\\([a-z]+\\)\\(!\\)\\(;\\)"
           (list 1 2)
           'comment
           (list 'hl-prog-extra-test-a 'hl-prog-extra-test-b))
          (list "\\[\\([a-z]+\\)\\]" 1 'comment 'hl-prog-extra-test-b)))
        (text-initial "/* @one=two!; [three] */")
        (text-expected
         (concat
          "/* @<span class='hl-prog-extra-test-a'>one</span>\n" ;
          "=<span class='hl-prog-extra-test-b'>two</span>\n" ;
          "!; [<span class='hl-prog-extra-test-b'>three</span>\n] */")))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))


(ert-deftest subexpr-group-after-shared-items-with-groups ()
  "Check items sharing a group don't shift the sub-expression of a later item."
  ;; Both items use the whole match with one face so they share a group, while
  ;; the groups their own regexes declare are padded for & never read. The
  ;; third item's sub-expression is looked up relative to its own group, which
  ;; must sit above all of them.
  (let ((hl-prog-extra-list
         (list
          (list "<\\([a-z]+\\)>" 0 'comment 'hl-prog-extra-test-a)
          (list "{\\([a-z]+\\)}" 0 'comment 'hl-prog-extra-test-a)
          (list "\\[\\([a-z]+\\)\\]" 1 'comment 'hl-prog-extra-test-b)))
        (text-initial "/* <one> {two} [three] */")
        (text-expected
         (concat
          "/* <span class='hl-prog-extra-test-a'>&lt;one&gt;</span>\n" ;
          " <span class='hl-prog-extra-test-a'>{two}</span>\n" ;
          " [<span class='hl-prog-extra-test-b'>three</span>\n] */")))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))

(ert-deftest subexpr-group-after-shared-items-across-contexts ()
  "Check a shared item's own groups in one context don't shift another's."
  ;; The combined regex is built per context, so a sharing item's declared
  ;; groups take lower numbers in a context which holds fewer of the items,
  ;; where the padded slots must still keep every later group above them.
  (let ((hl-prog-extra-list
         (list
          (list "<\\([a-z]+\\)>" 0 'comment 'hl-prog-extra-test-a)
          (list "<\\([a-z]+\\)>" 0 'string 'hl-prog-extra-test-a)
          (list "\\[\\([a-z]+\\)\\]" 1 'string 'hl-prog-extra-test-b)))
        (text-initial "/* <one> */ \"<two> [three]\"")
        (text-expected
         (concat
          "/* <span class='hl-prog-extra-test-a'>&lt;one&gt;</span>\n" ;
          " */ \"<span class='hl-prog-extra-test-a'>&lt;two&gt;</span>\n" ;
          " [<span class='hl-prog-extra-test-b'>three</span>\n]\"")))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))


(ert-deftest context-comment-start-is-not-matched ()
  "Check the documented gap at the start of a comment."
  ;; The scan is clamped so a comment's opening characters are not treated as
  ;; code, at the cost of not being able to match them at all.
  ;; See the note in `hl-prog-extra--match-impl'.
  ;; This pins a deliberate limitation, it is not a wanted behavior.
  (let ((hl-prog-extra-list (list (list "\\*" 0 'comment 'hl-prog-extra-test-a)))
        (text-initial "/* a * b */")
        (text-expected
         (concat
          ;; The `*' of the opening `/*' is in the gap and is never matched.
          "/* a <span class='hl-prog-extra-test-a'>*</span>\n" ;
          " b <span class='hl-prog-extra-test-a'>*</span>\n" ;
          "/")))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))

(ert-deftest subexpr-multiple-advances-point ()
  "Check scanning doesn't step back over text that has already been matched."
  ;; NOTE: the point must move past each match, otherwise font-lock searches the
  ;; same text again. The resulting faces are identical either way,
  ;; so drive the matcher directly and check where the point lands.
  (let ((hl-prog-extra-list
         (list
          (list
           "\\([a-z]+\\)=\\([a-z]+\\)"
           (list 1 2)
           'comment
           (list 'hl-prog-extra-test-a 'hl-prog-extra-test-b))))
        (text-initial "/* aa=bb cc=dd */"))
    (with-hl-prog-extra-test text-initial #'c-mode
      (goto-char (point-min))
      (let ((point-prev 0)
            (point-count 0))
        (while (hl-prog-extra--match (point-max))
          (should (> (point) point-prev))
          (setq point-prev (point))
          (setq point-count (1+ point-count)))
        ;; Two matches, each with two sub-expressions.
        (should (equal 4 point-count))))))


;; ---------------------------------------------------------------------------
;; Font Lock Region Tests

(ert-deftest fontify-region-partial ()
  "Check fontifying part of a buffer uses the context at the region start."
  ;; NOTE: this is what `jit-lock' does while scrolling, the region begins
  ;; inside the comment without the comment's opening for the scan to find.
  ;; The other checks fontify the whole buffer, which never takes this path.
  (let ((hl-prog-extra-list (list (list "\\<TODO\\>" 0 'comment 'hl-prog-extra-test-a)))
        (text-initial "/* one\n   TODO\n   TODO */\n")
        ;; The match on the third line is left alone, showing the region really
        ;; was partial, so this doesn't pass by fontifying the whole buffer.
        (text-expected
         (concat
          "/* one\n" ;
          "   <span class='hl-prog-extra-test-a'>TODO</span>\n\n" ;
          "   TODO */\n")))
    (with-temp-buffer
      (let ((inhibit-message t))
        (insert text-initial)
        (c-mode)
        (hl-prog-extra-mode 1)
        ;; Fontify the second line only.
        (goto-char (point-min))
        (forward-line 1)
        (let ((pos-beg (point)))
          (forward-line 1)
          (font-lock-fontify-region pos-beg (point)))
        (should (equal text-expected (hl-prog-extra-test-html)))))))


;; ---------------------------------------------------------------------------
;; Anonymous Face Tests

(ert-deftest face-anonymous ()
  "Check an anonymous face is written as an inline style."
  (let ((hl-prog-extra-list
         (list (list "\\<TODO\\>" 0 'comment '(:background "#006000" :foreground "#FFFFFF"))))
        (text-initial "/* TODO */")
        (text-expected
         (concat
          "/* <span style='background-color: #006000; color: #FFFFFF'>TODO</span>\n" " */")))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))


;; ---------------------------------------------------------------------------
;; Mode Tests

(ert-deftest mode-disable-removes-highlight ()
  "Check turning the mode off removes its highlighting."
  (let ((hl-prog-extra-list (list (list "\\<XX\\>" 0 'comment 'hl-prog-extra-test-a)))
        (text-initial "/* XX */"))
    (with-hl-prog-extra-test text-initial #'c-mode
      ;; Keep the faces, they are unavailable once the mode is disabled.
      (let ((face-list (hl-prog-extra-html-face-list)))
        (should (consp face-list))
        (hl-prog-extra-mode 0)
        (font-lock-flush)
        (font-lock-ensure)
        (should (equal "/* XX */" (hl-prog-extra-test-html face-list)))))))

(ert-deftest mode-refresh-applies-changed-list ()
  "Check `hl-prog-extra-refresh' picks up a changed `hl-prog-extra-list'."
  (let ((hl-prog-extra-list (list (list "\\<XX\\>" 0 'comment 'hl-prog-extra-test-a)))
        (text-initial "/* XX YY */"))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should
       (equal "/* <span class='hl-prog-extra-test-a'>XX</span>\n YY */" (hl-prog-extra-test-html)))
      (setq hl-prog-extra-list (list (list "\\<YY\\>" 0 'comment 'hl-prog-extra-test-a)))
      (hl-prog-extra-refresh)
      (font-lock-ensure)
      (should
       (equal
        "/* XX <span class='hl-prog-extra-test-a'>YY</span>\n */" (hl-prog-extra-test-html))))))

(ert-deftest rule-invalid-regex-is-skipped ()
  "Check a rule whose regex doesn't compile is skipped."
  (let ((hl-prog-extra-list
         (list
          (list "\\(" 0 'comment 'hl-prog-extra-test-a)
          (list "\\<XX\\>" 0 'comment 'hl-prog-extra-test-a)))
        (text-initial "/* XX */"))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should
       (equal "/* <span class='hl-prog-extra-test-a'>XX</span>\n */" (hl-prog-extra-test-html))))))

(ert-deftest rule-invalid-only-highlights-nothing ()
  "Check every rule being invalid leaves the text unmarked."
  ;; The face filter is empty here, which must not be read as no filter at all.
  (dolist (rule
           (list
            ;; A negative sub-expression.
            (list "\\<XX\\>" -1 'comment 'hl-prog-extra-test-a)
            ;; A context that isn't a known symbol.
            (list "\\<XX\\>" 0 'not-a-context 'hl-prog-extra-test-a)
            ;; More sub-expressions than faces.
            (list "\\(X\\)\\(X\\)" (list 1 2) 'comment (list 'hl-prog-extra-test-a))))
    (ert-info
     ((format "rule %S" rule))
     (let ((hl-prog-extra-list (list rule))
           (text-initial "/* XX */"))
       (with-hl-prog-extra-test text-initial #'c-mode
         (should (null (hl-prog-extra-html-face-list)))
         (should (equal "/* XX */" (hl-prog-extra-test-html))))))))

(ert-deftest rule-invalid-numbered-group-is-skipped ()
  "Check a rule whose regex numbers its own groups is skipped."
  ;; Each item is wrapped in a numbered group, so an item numbering its own
  ;; conflicts, which used to make the combined regex invalid. The error was
  ;; raised while font locking, where nothing in that context highlighted.
  (let ((hl-prog-extra-list
         (list
          (list "\\(?1:XX\\)" 0 'comment 'hl-prog-extra-test-a)
          (list "\\<YY\\>" 0 'comment 'hl-prog-extra-test-b)))
        (text-initial "/* XX YY */"))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should
       (equal
        "/* XX <span class='hl-prog-extra-test-b'>YY</span>\n */" (hl-prog-extra-test-html))))))

(ert-deftest rule-invalid-numbered-group-reason ()
  "Check the reason reported for a numbered group names it."
  ;; Any number is rejected, not only those that collide with the item's own.
  (dolist (re (list "\\(?1:XX\\)" "\\(?9:XX\\)" "X\\(?12:X\\)"))
    (ert-info
     ((format "regex %S" re))
     (let ((error-msg
            (hl-prog-extra--validate-keyword-item (list re 0 'comment 'hl-prog-extra-test-a))))
       (should (stringp error-msg))
       (should (string-match-p "explicitly numbered group" error-msg))))))

(ert-deftest rule-invalid-back-reference-is-skipped ()
  "Check a rule whose regex uses a back reference is skipped."
  ;; The back reference is numbered as if the group wrapping this item were its
  ;; own, which raised an invalid back reference while font locking, where
  ;; nothing in that context highlighted.
  (let ((hl-prog-extra-list
         (list
          (list "\\([a-z]\\)\\1" 0 'comment 'hl-prog-extra-test-a)
          (list "\\<YY\\>" 0 'comment 'hl-prog-extra-test-b)))
        (text-initial "/* xx YY */"))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should
       (equal
        "/* xx <span class='hl-prog-extra-test-b'>YY</span>\n */" (hl-prog-extra-test-html))))))

(ert-deftest rule-invalid-back-reference-reason ()
  "Check the reason reported for a back reference names it."
  (dolist (re (list "\\([a-z]\\)\\1" "\\(a\\)\\(b\\)X\\2"))
    (ert-info
     ((format "regex %S" re))
     (let ((error-msg
            (hl-prog-extra--validate-keyword-item (list re 0 'comment 'hl-prog-extra-test-a))))
       (should (stringp error-msg))
       (should (string-match-p "back reference" error-msg))))))

(ert-deftest rule-valid-group-not-numbered ()
  "Check regexes which only look like a numbered group are accepted."
  (dolist (re
           (list
            ;; A shy group, which takes no number.
            "\\(?:XX\\)"
            ;; The characters of a bracket expression, not a group.
            "[\\(?1:]+"
            ;; An escaped back-slash, the parenthesis that follows is a literal.
            "X\\\\(?1:"
            ;; The characters of a bracket expression, not a back reference.
            "[\\1]+"
            ;; An escaped back-slash, the digit that follows is a literal.
            "X\\\\1"))
    (ert-info
     ((format "regex %S" re))
     (should
      (null (hl-prog-extra--validate-keyword-item (list re 0 'comment 'hl-prog-extra-test-a)))))))

(ert-deftest rule-group-shared-reserves-own-groups ()
  "Check what items sharing a group reserve for the groups they declare."
  ;; The shared group is claimed once while each item's own groups are still
  ;; padded for. Sharing failing here doubles what such items take from the
  ;; group limit, where a large list stops highlighting at half the groups
  ;; Emacs numbers.
  (let ((rule-list
         (list
          (list "<\\([a-z]+\\)>" 0 'comment 'hl-prog-extra-test-a)
          (list "{\\([a-z]+\\)}" 0 'comment 'hl-prog-extra-test-a))))
    (pcase-let ((`(,_re-list ,_face-vector ,uniq-vector ,_ ,_)
                 (hl-prog-extra--precompute-regex rule-list)))
      ;; One shared group, then one padded slot per item's declared group.
      (should (eq 3 (length uniq-vector))))))

(ert-deftest rule-validate-keeps-match-data ()
  "Check validating a rule leaves the caller's match data alone."
  ;; Validation runs when the mode is enabled, which may happen from a hook
  ;; while the code that ran the search is still using its match data.
  (should (eq 0 (string-match "\\(foo\\)bar" "foobar")))
  (should
   (stringp
    (hl-prog-extra--validate-keyword-item (list "\\(?1:XX\\)" 0 'comment 'hl-prog-extra-test-a))))
  (should (equal "foo" (match-string 1 "foobar"))))

(ert-deftest rule-invalid-is-skipped ()
  "Check an invalid rule is skipped without discarding the valid rules."
  (let ((hl-prog-extra-list
         (list
          ;; The regex must be a string.
          (list 123 0 'comment 'hl-prog-extra-test-a)
          (list "\\<XX\\>" 0 'comment 'hl-prog-extra-test-a)))
        (text-initial "/* XX */"))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should
       (equal "/* <span class='hl-prog-extra-test-a'>XX</span>\n */" (hl-prog-extra-test-html))))))


;; ---------------------------------------------------------------------------
;; Global Mode Tests

;; NOTE: these call `hl-prog-extra--mode-turn-on' (what the global mode runs for
;; each buffer) instead of enabling the global mode, which would stay enabled
;; for every buffer the remaining tests create.

(defun hl-prog-extra-test-turn-on (mode setup-fn)
  "Run SETUP-FN in a buffer using MODE, then turn the mode on globally.
Return non-nil when the mode was enabled."
  (with-temp-buffer
    (let ((inhibit-message t))
      (funcall mode)
      (funcall setup-fn)
      (hl-prog-extra--mode-turn-on)
      hl-prog-extra-mode)))

(ert-deftest global-mode-turn-on ()
  "Check a regular buffer has the mode enabled."
  (should (hl-prog-extra-test-turn-on #'c-mode #'ignore)))

(ert-deftest global-mode-ignore-modes ()
  "Check a major-mode in `hl-prog-extra-global-ignore-modes' is skipped."
  (should
   (null
    (hl-prog-extra-test-turn-on
     #'c-mode (lambda () (setq-local hl-prog-extra-global-ignore-modes (list 'c-mode)))))))

(ert-deftest global-mode-ignore-buffer ()
  "Check a buffer setting `hl-prog-extra-global-ignore-buffer' is skipped."
  (should
   (null
    (hl-prog-extra-test-turn-on
     #'c-mode (lambda () (setq-local hl-prog-extra-global-ignore-buffer t))))))

(ert-deftest global-mode-ignore-buffer-predicate ()
  "Check the predicate form of `hl-prog-extra-global-ignore-buffer'."
  ;; The buffer is excluded when the predicate returns non-nil.
  (should
   (null
    (hl-prog-extra-test-turn-on
     #'c-mode (lambda () (setq-local hl-prog-extra-global-ignore-buffer (lambda (_buf) t))))))
  (should
   (hl-prog-extra-test-turn-on
    #'c-mode (lambda () (setq-local hl-prog-extra-global-ignore-buffer (lambda (_buf) nil))))))

(ert-deftest global-mode-special-mode ()
  "Check a `special-mode' buffer is skipped."
  (should (null (hl-prog-extra-test-turn-on #'special-mode #'ignore))))


;; ---------------------------------------------------------------------------
;; Preset Tests

(defun hl-prog-extra-test-preset-mode-list ()
  "Return the modes shipping a preset, taken from the file names.
Discovered instead of listed so a preset added later is not skipped."
  (let ((result (list))
        (re "\\`hl-prog-extra-preset-\\(.*\\)\\.el\\'"))
    (dolist (filename (directory-files hl-prog-extra-tests-basedir nil re))
      (when (string-match re filename)
        (push (match-string 1 filename) result)))
    (sort result #'string-lessp)))

(defvar hl-prog-extra-test-warning-list nil
  "Warnings collected while running a test, most recent first.")

(defun hl-prog-extra-test--warning-capture (type message &rest _args)
  "Collect TYPE and MESSAGE instead of displaying the warning."
  (push (cons type message) hl-prog-extra-test-warning-list)
  ;; Return a string as `display-warning' does in batch mode.
  ;; Returning something else would hide a caller mishandling the result.
  message)

(defun hl-prog-extra-test-warning-match-p (re)
  "Return non-nil when a collected warning matches RE."
  (let ((found nil))
    (pcase-dolist (`(,_type . ,message) hl-prog-extra-test-warning-list)
      (when (string-match-p re message)
        (setq found t)))
    found))

(defmacro with-hl-prog-extra-test-preset-file (filename content &rest body)
  "Run BODY with a preset FILENAME containing CONTENT on `load-path'.
Warnings are collected in `hl-prog-extra-test-warning-list' instead of being
displayed, which both quiets the expected warnings and allows checking them."
  (declare (indent 2))
  `(let ((dir (make-temp-file "hl-prog-extra-tests" t))
         (hl-prog-extra-test-warning-list (list)))
     ;; NOTE: `warning-minimum-level' can't be used to quiet these,
     ;; in batch mode the warning is written to stderr without checking it.
     (advice-add 'display-warning :override #'hl-prog-extra-test--warning-capture)
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name ,filename dir)
             (insert ";;; -*- lexical-binding: t -*-\n")
             (insert ,content))
           (let ((load-path (cons dir load-path)))
             ,@body))
       (advice-remove 'display-warning #'hl-prog-extra-test--warning-capture)
       (delete-directory dir t))))

(ert-deftest preset-mode-list-discovered ()
  "Check presets are found at all.
The checks that loop over them pass trivially otherwise."
  (let ((modes (hl-prog-extra-test-preset-mode-list)))
    (should (consp modes))
    (should (member "c-mode" modes))))

(ert-deftest preset-known-modes ()
  "Check every mode shipping a preset returns rules."
  (dolist (mode (hl-prog-extra-test-preset-mode-list))
    (let ((rules (hl-prog-extra-preset mode)))
      (should (consp rules)))))

(ert-deftest preset-rules-valid ()
  "Check every rule of every preset passes the package's own validation."
  (dolist (mode (hl-prog-extra-test-preset-mode-list))
    (dolist (rule (hl-prog-extra-preset mode))
      (ert-info
       ((format "preset %S rule %S" mode rule))
       (should (null (hl-prog-extra--validate-keyword-item rule)))))))

(ert-deftest preset-unknown-mode ()
  "Check an unknown mode returns nil instead of raising an error."
  (should (null (hl-prog-extra-preset "hl-prog-extra-no-such-mode"))))

(ert-deftest preset-mode-from-major-mode ()
  "Check the mode defaults to `major-mode' when not given."
  (with-temp-buffer
    (c-mode)
    (should (equal (hl-prog-extra-preset "c-mode") (hl-prog-extra-preset)))))

(ert-deftest preset-keyword-args ()
  "Check keyword arguments reach the preset."
  ;; The escape rule is the only rule of the C preset.
  (should (consp (hl-prog-extra-preset "c-mode" nil)))
  (should (null (hl-prog-extra-preset "c-mode" nil :no-string-escape t))))

(ert-deftest preset-keyword-args-python ()
  "Check disabling one group of rules leaves the others."
  (let ((rules-all (hl-prog-extra-preset "python-mode" nil))
        (rules-no-sphinx (hl-prog-extra-preset "python-mode" nil :no-sphinx t)))
    (should (< (length rules-no-sphinx) (length rules-all)))
    (should (consp rules-no-sphinx))))

(ert-deftest preset-keyword-args-invalid ()
  "Check an unknown keyword argument raises an error."
  (should-error (hl-prog-extra-preset "c-mode" nil :no-such-keyword t))
  (should-error (hl-prog-extra-preset "c-mode" nil :no-string-escape)))

(ert-deftest preset-positional-args-too-many ()
  "Check more positional arguments than expected raises an error."
  (should-error (hl-prog-extra-preset "c-mode" nil 'extra)))

(ert-deftest preset-load-error ()
  "Check a preset failing to load returns nil instead of raising an error."
  (with-hl-prog-extra-test-preset-file "hl-prog-extra-preset-tests-broken-mode.el"
      "(error \"Broken preset\")\n(provide 'hl-prog-extra-preset-tests-broken-mode)\n"
    (should (null (hl-prog-extra-preset "tests-broken-mode")))
    ;; The cause must be surfaced, not silently treated as a missing preset.
    (should (hl-prog-extra-test-warning-match-p "failed: Broken preset"))))

(ert-deftest preset-loaded-without-function ()
  "Check a preset that loads without defining its function returns nil."
  ;; The result is appended to `hl-prog-extra-list', so anything other than a
  ;; list (the string `lwarn' returns for e.g.) makes the list malformed.
  (with-hl-prog-extra-test-preset-file "hl-prog-extra-preset-tests-nofn-mode.el"
      "(provide 'hl-prog-extra-preset-tests-nofn-mode)\n"
    (unwind-protect
        (progn
          (should (null (hl-prog-extra-preset "tests-nofn-mode")))
          (should (hl-prog-extra-test-warning-match-p "did not define function")))
      (setq features (delq 'hl-prog-extra-preset-tests-nofn-mode features)))))

(ert-deftest preset-enabled-with-broken-preset ()
  "Check enabling the mode with an unusable preset doesn't raise an error."
  (with-hl-prog-extra-test-preset-file "hl-prog-extra-preset-tests-nofn2-mode.el"
      "(provide 'hl-prog-extra-preset-tests-nofn2-mode)\n"
    (unwind-protect
        (let ((hl-prog-extra-preset t)
              (hl-prog-extra-list (list (list "\\<XX\\>" 0 'comment 'hl-prog-extra-test-a))))
          (with-temp-buffer
            (insert "/* XX */")
            (c-mode)
            (setq major-mode 'tests-nofn2-mode)
            (hl-prog-extra-mode 1)
            (font-lock-ensure)
            ;; The user's own rules still apply.
            (should
             (equal
              "/* <span class='hl-prog-extra-test-a'>XX</span>\n */" (hl-prog-extra-test-html)))))
      (setq features (delq 'hl-prog-extra-preset-tests-nofn2-mode features)))))

(ert-deftest preset-highlights-string-escape ()
  "Check the C preset highlights escape sequences in strings."
  (let ((hl-prog-extra-list nil)
        (hl-prog-extra-preset t)
        (text-initial "char *s = \"a\\nb\";")
        (text-expected "char *s = \"a<span class='escape-glyph'>\\n</span>\nb\";"))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))

(ert-deftest preset-appends-to-user-list ()
  "Check preset rules are used alongside the user's own."
  (let ((hl-prog-extra-list (list (list "\\<XX\\>" 0 'comment 'hl-prog-extra-test-a)))
        (hl-prog-extra-preset t)
        (text-initial "/* XX */ char *s = \"a\\nb\";")
        (text-expected
         (concat
          "/* <span class='hl-prog-extra-test-a'>XX</span>\n */ " ;
          "char *s = \"a<span class='escape-glyph'>\\n</span>\nb\";")))
    (with-hl-prog-extra-test text-initial #'c-mode
      (should (equal text-expected (hl-prog-extra-test-html))))))


;; ---------------------------------------------------------------------------
;; Face Resolution Tests

(ert-deftest color-to-hex-name ()
  "Check color names resolve to hex independently of the frame."
  ;; These would all resolve to saturated primaries via `color-values' in batch.
  (should (equal "#B22222" (hl-prog-extra-html--color-to-hex "Firebrick")))
  (should (equal "#0000FF" (hl-prog-extra-html--color-to-hex "Blue1")))
  ;; Spaces and case are not significant.
  (should (equal "#008B8B" (hl-prog-extra-html--color-to-hex "dark cyan"))))

(ert-deftest color-to-hex-hex ()
  "Check hex colors pass through, reducing the wider forms."
  (should (equal "#006000" (hl-prog-extra-html--color-to-hex "#006000")))
  (should (equal "#0F0" (hl-prog-extra-html--color-to-hex "#0F0")))
  (should (equal "#00FF00" (hl-prog-extra-html--color-to-hex "#0000FFFF0000"))))

(ert-deftest color-to-hex-unknown ()
  "Check an unknown color is skipped instead of being written as CSS."
  ;; Used by the `default' face when the frame has no color.
  (should (null (hl-prog-extra-html--color-to-hex "unspecified-fg"))))

(ert-deftest face-resolve-display ()
  "Check the face spec matching the requested display is used."
  (let ((face-plist-fn
         (lambda (background)
           (let ((hl-prog-extra-html-display
                  (list
                   (cons 'type 'x-toolkit)
                   (cons 'class 'color)
                   (cons 'min-colors 16777216)
                   (cons 'background background))))
             (hl-prog-extra-html--face-to-plist 'hl-prog-extra-test-display)))))

    ;; A display of 16777216 colors must satisfy a spec asking for 88.
    (should (equal "#111111" (plist-get (funcall face-plist-fn 'light) :foreground)))
    (should (equal "#222222" (plist-get (funcall face-plist-fn 'dark) :foreground)))))

(ert-deftest face-resolve-display-graphic ()
  "Check a spec naming a display type other than the first is still matched."
  ;; `defface' uses several names for a graphical display,
  ;; matching only one of them silently falls back to the `t' spec.
  (let ((plist (hl-prog-extra-html--face-to-plist 'hl-prog-extra-test-display-graphic)))
    (should (equal "#444444" (plist-get plist :foreground)))))

(ert-deftest css-weight-and-slant ()
  "Check weights and slants without a CSS equivalent are skipped."
  (should (equal "font-weight: bold" (hl-prog-extra-html--css-from-plist '(:weight bold))))
  (should (equal "font-weight: 600" (hl-prog-extra-html--css-from-plist '(:weight semi-bold))))
  (should (equal "font-style: italic" (hl-prog-extra-html--css-from-plist '(:slant italic))))
  ;; Writing these directly would give CSS the browser discards.
  (should (null (hl-prog-extra-html--css-from-plist '(:weight book))))
  (should (null (hl-prog-extra-html--css-from-plist '(:slant reverse-italic)))))

(ert-deftest css-shared-property ()
  "Check attributes mapping onto the same CSS property are written once."
  ;; Writing both would leave only the last one in effect.
  (should
   (equal
    "text-decoration: underline"
    (hl-prog-extra-html--css-from-plist '(:underline t :strike-through t))))
  ;; A repeated attribute keeps the first, matching how a face resolves.
  (should
   (equal
    "color: #B22222"
    (hl-prog-extra-html--css-from-plist '(:foreground "Firebrick" :foreground "#00FF00")))))

(ert-deftest css-from-plist-unusable-values ()
  "Check attribute values that are not colors are skipped."
  ;; A face may clear an attribute with either of these.
  (should (null (hl-prog-extra-html--css-from-plist '(:foreground nil))))
  (should (null (hl-prog-extra-html--css-from-plist '(:background unspecified))))
  ;; The legacy `(foreground-color . COLOR)' face is not a property list.
  (should (null (hl-prog-extra-html--css-from-plist '(foreground-color . "red")))))

(ert-deftest face-unusable-does-not-error ()
  "Check faces the package accepts but that can't be written as CSS."
  ;; These all pass `hl-prog-extra--validate-keyword-item', so they reach the
  ;; export and must be skipped rather than raise an error part way through.
  (dolist (face (list '(:foreground nil) '(:foreground unspecified) '(foreground-color . "red")))
    (ert-info
     ((format "face %S" face))
     (let ((hl-prog-extra-list (list (list "\\<XX\\>" 0 'comment face)))
           (text-initial "/* XX */"))
       (with-hl-prog-extra-test text-initial #'c-mode
         ;; Nothing is wrapped, the face has no CSS representation.
         (should (equal "/* XX */" (hl-prog-extra-test-html))))))))

(ert-deftest face-resolve-inherit ()
  "Check `:inherit' through a `default' spec entry is resolved."
  (let ((plist (hl-prog-extra-html--face-to-plist 'hl-prog-extra-test-inherit)))
    (should (equal "#FF0000" (plist-get plist :foreground)))))

(ert-deftest face-resolve-inherit-list ()
  "Check `:inherit' naming a list of faces is resolved."
  ;; Only a symbol used to be followed, so such a face resolved to nothing.
  (let ((plist (hl-prog-extra-html--face-to-plist 'hl-prog-extra-test-inherit-list)))
    ;; The earlier face of the list takes priority.
    (should (equal "#FF0000" (plist-get plist :foreground)))))

(ert-deftest face-resolve-attribute-fallback ()
  "Check a face defined without a spec resolves through its attributes."
  ;; The fall-back used to read a hard-coded set of attributes,
  ;; so a face setting any other one resolved to nothing.
  (let ((face 'hl-prog-extra-test-nospec))
    ;; `make-face' creates the face without a `defface' spec.
    (make-face face)
    (set-face-attribute face nil :underline t)
    (should
     (equal
      "text-decoration: underline"
      (hl-prog-extra-html--css-from-plist (hl-prog-extra-html--face-to-plist face))))))

(ert-deftest face-resolve-empty ()
  "Check a face defining no attributes is skipped."
  ;; `font-lock-negation-char-face' is defined this way, resolving it gave nil
  ;; which was taken for a face name, writing "class='nil'".
  (should (null (hl-prog-extra-html--face-attrs 'hl-prog-extra-test-empty t)))
  (let ((hl-prog-extra-list (list (list "\\<TODO\\>" 0 'comment 'hl-prog-extra-test-empty)))
        (text-initial "/* TODO */"))
    (with-hl-prog-extra-test text-initial #'c-mode
      ;; Nothing is wrapped, the face has no CSS representation.
      (should (equal "/* TODO */" (hl-prog-extra-test-html nil t))))))

(ert-deftest face-resolve-inherit-override ()
  "Check an inherited attribute does not override the face's own."
  (let ((hl-prog-extra-list (list (list "\\<TODO\\>" 0 'comment 'hl-prog-extra-test-inherit)))
        (text-initial "/* TODO */"))
    (with-hl-prog-extra-test text-initial #'c-mode
      ;; The color is written once, from the inherited face.
      (should
       (equal
        "/* <span style='color: #FF0000'>TODO</span>\n */" (hl-prog-extra-test-html nil t))))))

(provide 'hl-prog-extra_tests)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-format-quoted: nil
;; End:
;;; hl-prog-extra_tests.el ends here
