;;; ghelp-helpful.el --- Ghelp+Helpful      -*- lexical-binding: t; -*-

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'helpful)
(require 'ghelp-builtin)
(require 'pcase)

(defun ghelp-helpful-backend (command data)
  "Helpful backend."
  (pcase command
    ('symbol (completing-read "Symbol: " obarray
                              (lambda (s)
                                (let ((s (intern-soft s)))
                                  (or (fboundp s)
                                      (boundp s)
                                      (facep s)
                                      (cl--class-p s))))))
    ;; This way refreshing works with buffer-local variables.
    ('doc (with-current-buffer (marker-buffer (plist-get data :marker))
            (let* ((symbol (intern-soft (plist-get data :symbol)))
                   (callable-doc (ghelp-helpful-callable symbol))
                   (variable-doc (ghelp-helpful-variable symbol))
                   (entry-list (list
                                (when callable-doc
                                  (list (format "%s (callable)" symbol)
                                        callable-doc))
                                (when variable-doc
                                  (list (format "%s (variable)" symbol)
                                        variable-doc))
                                (ghelp-face-describe-symbol symbol)
                                (ghelp-cl-type-describe-symbol symbol))))
              (remove nil entry-list))))))

(defun ghelp-helpful-key (key-sequence)
  "Describe KEY-SEQUENCE."
  (interactive
   (list (read-key-sequence "Press key: ")))
  (let ((sym (key-binding key-sequence)))
    (pcase sym
      ('nil (user-error "No command is bound to %s"
                        (key-description key-sequence)))
      ((pred commandp) (ghelp-describe-1
                        'no-prompt `(:symbol ,sym :mode emacs-lisp-mode
                                             :marker ,(point-marker))))
      (_ (user-error "%s is bound to %s which is not a command"
                     (key-description key-sequence)
                     sym)))))

(defun ghelp-helpful-callable (symbol)
  (when (fboundp symbol)
    (let ((buf (helpful--buffer symbol t)))
      (with-current-buffer buf
        (helpful-update)
        ;; insert an ending line
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "\n"))
        (prog1 (buffer-string)
          (kill-buffer buf))))))

(defun ghelp-helpful-variable (symbol)
  ;; For some reason ‘helpful-update’ jumps to the definition
  ;; of the variable.
  (save-excursion
    (when (helpful--variable-p symbol)
      (let ((buf (helpful--buffer symbol nil)))
        (with-current-buffer buf
          (helpful-update)
          ;; insert an ending newline
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert "\n"))
          (prog1 (let ((yank-excluded-properties nil))
                   (buffer-string))
            (kill-buffer buf)))))))

;;; Advices

(defun ghelp-helpful--describe-advice (oldfn button)
  "Describe the symbol that this BUTTON represents."
  (if (derived-mode-p 'ghelp-page-mode)
      (let* ((data (ghelp-get-page-data)))
        (setq data (plist-put data :symbol (button-get button 'symbol)))
        (setq data (plist-put data :marker (point-marker)))
        (ghelp-describe-1 'no-prompt data))
    (funcall oldfn button)))

(defun ghelp-helpful--update-advice (oldfn)
  (if (derived-mode-p 'ghelp-page-mode)
      (ghelp-refresh)
    (funcall oldfn)))

(advice-add 'helpful-update :around #'ghelp-helpful--update-advice)
(advice-add 'helpful--describe :around #'ghelp-helpful--describe-advice)
(advice-add 'helpful--describe-exactly
            :around #'ghelp-helpful--describe-advice)

(provide 'ghelp-helpful)

;;; ghelp-helpful.el ends here
