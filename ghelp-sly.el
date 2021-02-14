;;; ghelp-sly.el --- Ghelp sly backend      -*- lexical-binding: t; -*-

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This file contains the sly backend for Ghelp. I copied code from
;; sly.el and modified them to work with Ghelp.

;;; Code:
;;

(require 'sly)
(require 'pcase)

(defun ghelp-sly-backend (command data)
  "Sly backend."
  (pcase command
    ('symbol (sly-read-symbol-name "Documentation for symbol: "))
    ('doc
     (let ((package (sly-current-package))
           (connection (sly-current-connection))
           (symbol-name (plist-get data :symbol-name)))
       (ignore connection)
       (lambda (buffer callback &rest _)
         ;; ‘slynk:describe-symbol’ returns more information than
         ;; ‘slynk:documentation-symbol’.
         (sly-eval-async `(slynk:describe-symbol ,symbol-name)
           (lambda (doc)
             (with-current-buffer buffer
               (insert doc)
               (ghelp-sly--fontify-doc)
               (funcall callback)))
           package))))))

(defun ghelp-sly--fontify-doc ()
  "Fontify the documentation in the current buffer."
  (goto-char (point-min))
  ;; Add blank lines between entries and highlight entry titles.
  (while (re-search-forward "^ +.*:[ \n]" nil t)
    (save-excursion
      (put-text-property (match-beginning 0) (match-end 0)
                         'face '(:weight bold))
      (goto-char (match-beginning 0))
      (insert "\n")))
  ;; Make the link to the source file a clickable button.
  (goto-char (point-min))
  (when (re-search-forward "Source file: \\(.*\\)$" nil t)
    (make-text-button (match-beginning 1) (match-end 1)
                      'action (let ((path (match-string 1)))
                                (lambda (&rest _)
                                  (find-file path)))
                      'follow-link t)))


(provide 'ghelp-sly)

;;; ghelp-sly.el ends here
