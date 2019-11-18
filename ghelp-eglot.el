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

(defun ghelp-eglot--describe-symbol (symbol buffer point)
  (catch 'ret
    (when (and eglot--managed-mode point)
      (save-excursion
        (goto-char point)
        (let ((doc (eglot--dbind
                       ((Hover) contents range)
                       (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover
                                        (eglot--TextDocumentPositionParams))
                     (when (seq-empty-p contents) (throw 'ret nil))
                     (concat (eglot--hover-info contents range) "\n"))))
          (list (list symbol doc)))))))



(provide 'ghelp-eglot)

;;; ghelp-eglot.el ends here
