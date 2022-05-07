;;; hl-prog-extra-preset-python-mode.el --- Python preset -*- lexical-binding: t -*-
;; URL: https://codeberg.com/ideasman42/emacs-hl-prog-extra
;; Version: 0.1
;; Package-Requires: ((emacs "26.2"))

;;; Commentary:
;; Preset for Python.

;;; Code:

;;;###autoload
(defun hl-prog-extra-preset-python-mode (&rest args)
  "Presets for `python-mode' with optional ARGS keyword arguments.
:no-string-escape
  Don't use escape strings.
:no-sphinx
  Don't use sphinx in doc-strings."
  (let
    ( ;; Keywords.
      (no-string-escape nil)
      (no-sphinx nil)

      ;; Constant's for regex.
      (re-identifier "[[:alpha:]_]+[[:alnum:]_]*")

      (result (list)))

    ;; Parse keywords.
    (while args
      (let ((arg-current (pop args)))
        (cond
          ((keywordp arg-current)
            (unless args
              (error "Keyword argument %S has no value!" arg-current))
            (let ((v (pop args)))
              (pcase arg-current
                (:no-string-escape
                  (unless (memq v (list nil t))
                    (error ":quiet expected a boolean"))
                  (setq no-string-escape v))
                (:no-sphinx
                  (unless (memq v (list nil t))
                    (error ":no-sphinx expected a boolean"))
                  (setq no-sphinx v))
                (_ (error "Unknown argument %S" arg-current)))))
          (t
            (error
              "Arguments must be keyword, value pairs, found %S = %S"
              (type-of arg-current)
              arg-current)))))

    (unless no-string-escape
      (push
        (list
          (concat
            ;; Back-slash.
            "\\\\"
            ;; Group.
            "\\("
            ;; "\n" and similar single escape characters.
            "[abnrtfv\"\\\\]\\|"
            ;; "\x" number.
            "x[0-9]+\\|"
            ;; "\u" unicode.
            "u[0-9a-fA-F]+\\|"
            ;; "\x000".
            "[0-9]+\\|"
            ;; "\N{NAMED UNICODE}".
            "N{[A-Z\\s\\-]+}\\|"
            ;; Trailing slash to escape a newline (continue string).
            "\n"
            ;; End group.
            "\\)")
          0 'string 'escape-glyph)
        result))

    ;; Note that this isn't an attempt to be a full Sphinx-in-Python solution
    ;; it's mainly intended to isolate Sphinx markup so it's the from plain text.
    ;; This can be useful when reading over doc-strings and also allows
    ;; spell checkers (such as `spell-fu' to ignore some parts of the text).
    (unless no-sphinx
      ;; Match ``some.text`` as a constant.
      (push (list "``\\(.*?\\)``" 0 'string-doc 'font-lock-constant-face) result)

      ;; Match :text:
      ;; Add here so more general checks such as :class:`text` get priority.
      (push (list ":[a-z_]+:" 0 'string-doc 'font-lock-constant-face) result)

      ;; Covers most roles: :class:`some.class`, etc.
      (push (list ":[a-z_]+:`[^\n`]+`" 0 'string-doc 'font-lock-constant-face) result)


      ;; Argument syntax.
      (dolist (prefix (list "arg" "type"))
        (push
          (list
            (concat ":" prefix "[[:blank:]]+" re-identifier ":")
            0
            'string-doc
            'font-lock-constant-face)
          result)))

    (dolist (id (list "return" "rtype"))
      (push (list (concat ":" id ":") 0 'string-doc 'font-lock-constant-face) result))

    ;; Match ">>> code".
    (push (list "^[[:blank:]]*>>>[^\n]+$" 0 'string-doc 'font-lock-constant-face) result)

    ;; Covers most directives.
    (push
      (list
        (concat "^[[:blank:]]*" "\\.\\.[[:blank:]]+[[:alpha:]]+[[:alpha:]_\\-]*::[[:blank:]\n]")
        0
        'string-doc
        'font-lock-constant-face)
      result)

    result))

(provide 'hl-prog-extra-preset-python-mode)
;;; hl-prog-extra-preset-python-mode.el ends here
