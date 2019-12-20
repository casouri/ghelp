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

(defun ghelp-eglot-backend (&optional no-prompt)
  (when eglot--managed-mode
    (let* ((symbol (symbol-name (symbol-at-point)))
           (mode (ghelp-get-mode))
           (doc (catch 'ret
                  (eglot--dbind
                      ((Hover) contents range)
                      (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover
                                       (eglot--TextDocumentPositionParams))
                    (when (seq-empty-p contents) (throw 'ret nil))
                    (concat (eglot--hover-info contents range) "\n")))))
      (when doc
        (with-current-buffer (ghelp-get-page-or-create mode symbol)
          (ghelp-page-clear)
          (ghelp-page-insert-entry (list symbol doc))
          (current-buffer))))))



(provide 'ghelp-eglot)

;;; ghelp-eglot.el ends here
