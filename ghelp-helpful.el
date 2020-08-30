;;; ghelp-helpful.el --- Ghelp+Helpful      -*- lexical-binding: t; -*-

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This file contains the Helpful.el backend for Ghelp. I copied code
;; from helpful.el and modified them to work with Ghelp.

;;; Code:
;;

(require 'helpful)
(require 'ghelp-builtin)
(require 'pcase)

(defun ghelp-helpful-backend (command data)
  "Help backend.
COMMAND and DATA are described in the Commentary of ghelp.el.
FUNCTION-BACKEND returns the documentation of the symbol as a
function, VARIABLE-BACKEND returns the documentation of the
symbol as a variable, other backends in BACKEND-LIST returns the
documentation of the symbol as other things."
  (ghelp-help-backend-1
   command data #'ghelp-helpful-callable #'ghelp-helpful-variable
   #'ghelp-help--face #'ghelp-help-cl-type))

(defun ghelp-helpful-key (key-sequence)
  "Describe KEY-SEQUENCE."
  (interactive
   (list (read-key-sequence "Press key: ")))
  (let ((def (key-binding key-sequence))
        (key-name (key-description key-sequence)))
    (pcase def
      ('nil (user-error "No command is bound to %s"
                        (key-description key-sequence)))
      ((pred commandp)
       (if (or (stringp def) (vectorp def))
           ;; DEF is a keyboard macro.
           (ghelp-describe-1
            'no-prompt `(:symbol ,key-name :mode emacs-lisp-mode
                                 :marker ,(point-marker)
                                 :kmacro ,def))
         ;; DEF is a symbol for a function.
         (ghelp-describe-1
          'no-prompt `(:symbol ,def :mode emacs-lisp-mode
                               :marker ,(point-marker)))))
      (_ (user-error "%s is bound to %s which is not a command"
                     (key-description key-sequence)
                     def)))))

(defun ghelp-helpful-callable (symbol original-buffer)
  "Return documentation for SYMBOL as a function.
ORIGINAL-BUFFER is the buffer where user requested for documentation."
  (with-current-buffer original-buffer
    (when (or (and (symbolp symbol) (fboundp symbol))
              (vectorp symbol) (stringp symbol))
      (let ((buf (helpful--buffer symbol t)))
        (with-current-buffer buf
          (helpful-update)
          ;; insert an ending line
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert "\n"))
          (prog1 (list (format
                        "%s (%s)" symbol
                        (if (symbolp symbol)
                            (if (functionp (symbol-function symbol))
                                "function" "macro")
                          "keyboard macro"))
                       (buffer-string))
            (kill-buffer buf)))))))

(defun ghelp-helpful-variable (symbol original-buffer)
  "Return documentation for SYMBOL as a variable.
ORIGINAL-BUFFER is the buffer where user requested for documentation."
  (with-current-buffer original-buffer
    (when (helpful--variable-p symbol)
      (let ((buf (helpful--buffer symbol nil)))
        (with-current-buffer buf
          (helpful-update)
          ;; insert an ending newline
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert "\n"))
          (prog1 (let ((yank-excluded-properties nil))
                   (list (format "%s (variable)" symbol)
                         (buffer-string)))
            (kill-buffer buf)))))))

;;; Advices

(defun ghelp-helpful--describe-advice (oldfn button)
  "Describe the symbol that this BUTTON represents.
OLDFN can be `helpful--describe' or `helpful--describe-exactly'."
  (if (derived-mode-p 'ghelp-page-mode)
      (let* ((data (ghelp-get-page-data)))
        (setq data (plist-put data :symbol (button-get button 'symbol)))
        (setq data (plist-put data :marker (point-marker)))
        (ghelp-describe-1 'no-prompt data))
    (funcall oldfn button)))

(defun ghelp-helpful--update-advice (oldfn)
  "Refresh ghelp page after OLDFN.
OLDFN is `helpful-update'."
  (if (derived-mode-p 'ghelp-page-mode)
      (ghelp-refresh)
    (funcall oldfn)))

(advice-add 'helpful-update :around #'ghelp-helpful--update-advice)
(advice-add 'helpful--describe :around #'ghelp-helpful--describe-advice)
(advice-add 'helpful--describe-exactly
            :around #'ghelp-helpful--describe-advice)

(provide 'ghelp-helpful)

;;; ghelp-helpful.el ends here
