;;; ghelp-geiser.el --- Ghelp backend for geiser      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This file contains the geiser.el backend for Ghelp. I copied code
;; from geiser.el and modified them to work with Ghelp.

;;; Code:
;;

(require 'geiser-doc)

(defun ghelp-geiser-backend (command data)
  "Backend for geiser."
  (pcase command
    ('symbol (geiser-completion--read-symbol "Symbol: "))
    ('doc (let* ((symbol (intern-soft (plist-get data :symbol)))
                 (impl geiser-impl--implementation)
                 (module (geiser-doc--module (geiser-eval--get-module)
                                             impl)))
            (ghelp-geiser--doc-symbol-advice symbol module impl)))))

(defun ghelp-geiser--doc-symbol-advice (symbol &optional module impl)
  (let ((doc (let ((ds (geiser-doc--get-docstring symbol module)))
               (if (or (not ds) (not (listp ds)))
                   nil
                 (with-temp-buffer
                   (geiser-doc--render-docstring ds symbol module impl)
                   (buffer-string)))))
        (sym-name (symbol-name symbol)))
    (if (not (derived-mode-p 'ghelp-page-mode))
        doc
      (let ((mode (plist-get (ghelp-get-page-data) :mode)))
        (ghelp--show-page `((,sym-name ,doc))
                          `(:symbol ,sym-name :mode ,mode)
                          (selected-window))))))

(advice-add 'geiser-doc-symbol
            :override #'ghelp-geiser--doc-symbol-advice)

(ghelp-register-backend 'geiser-repl-mode #'ghelp-geiser-backend)

(provide 'ghelp-geiser)

;;; ghelp-geiser.el ends here
