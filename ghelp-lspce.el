;;; ghelp-lspce.el --- Ghelp + lspce      -*- lexical-binding: t; -*-

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This file contains the lspce backend for Ghelp. It will make use of
;; ‘markdown-mode’ to render markdown-formatted documentation, if
;; ‘markdown-mode’ is available.

;;; Code:

(require 'lspce)
(require 'pcase)

(defun ghelp-lspce-backend (command data)
  "Lspce backend for ghelp.
For COMMAND and DATA, see ‘ghelp-register-backend’."
  (pcase command
    ('available-p lspce-mode)
    ('symbol (user-error "Lspce backend doesn’t support symbol lookup"))
    ('doc (save-excursion
            (goto-char (plist-get data :marker))
            (when lspce-mode
              (when-let ((hover-info (lspce--hover-at-point)))
                (with-temp-buffer
                  (let ((markdown-fontify-code-blocks-natively t)
                        (view-inhibit-help-message t))
                    (insert (cadr hover-info))
                    (when (and (equal (car hover-info) "markdown")
                               (require 'markdown-mode nil t))
                      (gfm-view-mode)
                      (font-lock-ensure))
                    (buffer-string)))))))))



(provide 'ghelp-lspce)

;;; ghelp-lspce.el ends here
