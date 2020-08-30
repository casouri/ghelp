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
  "Helpful backend.
COMMAND and DATA are described in the Commentary of ghelp.el."
  (pcase command
    ('symbol
     (pcase (plist-get data :category)
       ('function (completing-read
                   "Function: "
                   #'help--symbol-completion-table
                   (lambda (fn)
                     (or (fboundp fn)
                         (get fn 'function-documentation)))
                   t))
       ('variable (completing-read
                   "Variable: "
                   #'help--symbol-completion-table
                   (lambda (var)
                     (or (get var 'variable-documentation)
                         (and (boundp var) (not (keywordp var)))))
                   t))
       (_ (completing-read "Symbol: " obarray
                           (lambda (s)
                             (let ((s (intern-soft s)))
                               (or (fboundp s)
                                   (boundp s)
                                   (facep s)
                                   (cl--class-p s))))
                           t))))
    ('doc
     ;; This way refreshing works with buffer-local variables.
     (with-current-buffer (marker-buffer (plist-get data :marker))
       (if-let ((kmacro (plist-get data :kmacro)))
           ;; Describe a keyboard macro.
           (let ((macro-name (plist-get data :symbol)))
             (list (list
                    macro-name
                    (format "%s is a keyboard macro that expands to %s"
                            macro-name
                            (key-description kmacro)))))
         ;; Describe a symbol.
         (let ((symbol (intern-soft (plist-get data :symbol))))
           ;; But wait, symbol could be a keymap, which helpful
           ;; doesn’t support yet.
           (if (keymapp (symbol-function symbol))
               (format "%s is a sparse keymap. Meaning it is used as a prefix key" symbol)
             ;; Normal symbol.
             (let* ((symbol (intern-soft (plist-get data :symbol)))
                    (callable-doc (ghelp-helpful-callable symbol))
                    (variable-doc (ghelp-helpful-variable symbol))
                    (entry-list
                     (list
                      (when callable-doc
                        (list (format "%s (callable)" symbol)
                              callable-doc))
                      (when variable-doc
                        (list (format "%s (variable)" symbol)
                              variable-doc))
                      (ghelp-face-describe-symbol symbol)
                      (ghelp-cl-type-describe-symbol symbol))))
               (remove nil entry-list)))))))))

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

(defun ghelp-helpful-callable (symbol)
  "Return documentation fof SYMBOL as a function."
  (when (or (and (symbolp symbol) (fboundp symbol))
            (vectorp symbol) (stringp symbol))
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
  "Return documentation for SYMBOL as a variable."
  ;; For some reason ‘helpful-update’ jumps to the definition
  ;; of the variable, so we wrap a `save-excursion'.
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
