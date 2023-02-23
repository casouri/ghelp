;;; ghelp-eglot.el --- Ghelp+Eglot      -*- lexical-binding: t; -*-

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This file contains the eglot.el backend for Ghelp. I copied code
;; from eglot.el and modified them to work with Ghelp.

;;; Code:
;;

(require 'eglot)
(require 'pcase)

(defun ghelp-eglot-backend (command data)
  "Eglot backend."
  (pcase command
    ('available-p eglot--managed-mode)
    ('symbol (user-error "Eglot backend doesn’t support symbol lookup"))
    ('doc (save-excursion
            (goto-char (plist-get data :marker))
            (when eglot--managed-mode
              (eglot--dbind
                  ((Hover) contents range)
                  (jsonrpc-request (eglot--current-server-or-lose)
                                   :textDocument/hover
                                   (eglot--TextDocumentPositionParams))
                (when (not (seq-empty-p contents))
                  (concat (eglot--hover-info contents range) "\n"))))))))



(provide 'ghelp-eglot)

;;; ghelp-eglot.el ends here
