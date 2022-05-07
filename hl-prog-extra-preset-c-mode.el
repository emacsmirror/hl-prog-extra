;;; hl-prog-extra-preset-c-mode.el --- C preset -*- lexical-binding: t -*-
;; URL: https://codeberg.com/ideasman42/emacs-hl-prog-extra
;; Version: 0.1
;; Package-Requires: ((emacs "26.2"))

;;; Commentary:
;; Preset for C.

;;; Code:

;;;###autoload
(defun hl-prog-extra-preset-c-mode (&rest args)
  "Presets for `c-mode' with optional ARGS keyword arguments.
:no-string-escape
  Don't use escape strings."
  (let
    ( ;; Keywords.
      (no-string-escape nil)

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
                    (error ":no-string-escape expected a boolean"))
                  (setq no-string-escape v))
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
            "[abefnrtv\"\\\\]\\|"
            ;; "\x" number.
            "x[0-9a-fA-F]+\\|"
            ;; "\u" unicode.
            "[uU][0-9a-fA-F]+\\|"
            ;; "\x000".
            "[0-9]+"
            ;; End group.
            "\\)")
          0 'string 'escape-glyph)
        result))

    result))

(provide 'hl-prog-extra-preset-c-mode)
;;; hl-prog-extra-preset-c-mode.el ends here
