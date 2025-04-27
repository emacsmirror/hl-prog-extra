;;; hl-prog-extra-preset-lua-mode.el --- Lua preset -*- lexical-binding: t -*-
;; URL: https://codeberg.org/ideasman42/emacs-hl-prog-extra
;; Version: 0.1

;;; Commentary:
;; Preset for Lua.

;;; Code:

;;;###autoload
(defun hl-prog-extra-preset-lua-mode (&rest args)
  "Presets for `lua-mode' with optional ARGS keyword arguments.
:no-string-escape
  Don't use escape strings."
  (declare (important-return-value t))
  ;; Keywords.
  (let ((no-string-escape nil)
        ;; End keywords.

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
          (error "Arguments must be keyword, value pairs, found %S = %S"
                 (type-of arg-current)
                 arg-current)))))

    (unless no-string-escape
      ;; See: http://www.lua.org/manual/5.3/manual.html
      (push (list
             (concat
              ;; Back-slash.
              "\\\\"
              ;; Group.
              "\\("
              ;; "\n" and similar single escape characters.
              "[abfnrtv\"\\\\]\\|"
              ;; "\x" number.
              "x[0-9a-fA-F]\\{2\\}\\|"
              ;; "\u" unicode.
              "[uU][0-9a-fA-F]\\{4\\}\\|"
              ;; "\000".
              "[0-7]{1,3}"
              ;; End group.
              "\\)")
             0 'string 'escape-glyph)
            result))

    result))

(provide 'hl-prog-extra-preset-lua-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; package-lint-main-file: "hl-prog-extra.el"
;; End:
;;; hl-prog-extra-preset-lua-mode.el ends here
