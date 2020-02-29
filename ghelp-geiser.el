;;; ghelp-geiser.el --- Ghelp backend for geiser      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'geiser-doc)

(defun ghelp-geiser-backend (&optional prompt symbol)
  "Backend for geiser."
  (let* ((default-symbol (geiser--symbol-at-point))
         (symbol (ghelp-maybe-prompt prompt
                     default-symbol
                   (geiser-completion--read-symbol
                    (format "Symbol (%s): " default-symbol)
                    (symbol-name default-symbol)))))
    (when symbol
      (let* ((impl geiser-impl--implementation)
             (module (geiser-doc--module (geiser-eval--get-module)
                                         impl)))
        (let ((ds (geiser-doc--get-docstring symbol module)))
          (if (or (not ds) (not (listp ds)))
              (message "No documentation available for '%s'" symbol)
            `(,(symbol-name symbol)
              ;; an entry
              ((,(symbol-name symbol) ;; title
                ,(with-temp-buffer ;; body
                   (geiser-doc--render-docstring ds symbol module impl)
                   (buffer-string)))))))))))

(ghelp-register-backend 'geiser-repl-mode #'ghelp-geiser-backend)

(provide 'ghelp-geiser)

;;; ghelp-geiser.el ends here
