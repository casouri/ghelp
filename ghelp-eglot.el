;;; ghelp-eglot.el --- Ghelp+Eglot      -*- lexical-binding: t; -*-

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'eglot)

(defvar ghelp-eglot-supported-modes (let (list)
                                      (dolist (cell eglot-server-programs list)
                                        (if (symbolp (car cell))
                                            (push (car cell) list)
                                          (setq list (append (car cell) list)))))
  "A list of major modes that are supported by eglot.")

(defun ghelp-eglot-backend (&optional no-prompt refresh)
  (when eglot--managed-mode
    (let* ((marker (point-marker))
           (symbol (if refresh (save-excursion
                                 (goto-char
                                  (ghelp-page-store-get 'refresh-marker))
                                 (symbol-at-point))
                     (symbol-name (symbol-at-point))))
           (mode (ghelp-get-mode))
           (doc (catch 'ret
                  (eglot--dbind
                      ((Hover) contents range)
                      (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover
                                       (eglot--TextDocumentPositionParams))
                    (when (seq-empty-p contents) (throw 'ret nil))
                    (concat (eglot--hover-info contents range) "\n")))))
      (when doc
        (list symbol
              `((,symbol ,doc))
              `((refresh-marker ,marker)))))))



(provide 'ghelp-eglot)

;;; ghelp-eglot.el ends here
