;;; ghelp-helpful.el --- Ghelp+Helpful      -*- lexical-binding: t; -*-

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'helpful)

;;; New code
;;
;; The BUFFER that is passed around to ‘ghelp-helpful--buffer’
;; could be nil

(defun ghelp-helpful-describe-symbol (symbol buffer point)
  (let* ((buffer (or buffer (and point (marker-buffer point))))
         (symbol (intern-soft symbol))
         (callable-doc (ghelp-helpful-callable symbol buffer))
         (variable-doc (ghelp-helpful-variable symbol buffer)))
    (remove
     nil
     (list
      (when callable-doc (list (format "%s (callable)" symbol)
                               callable-doc))
      (when variable-doc (list (format "%s (variable)" symbol)
                               variable-doc))))))

(defun ghelp-helpful-callable (symbol buffer)
  (when (fboundp symbol)
    (let ((buf (ghelp-helpful--buffer symbol t buffer)))
      (with-current-buffer buf
        ;; use our hacked ‘helpful--button’ to insert
        ;; hacked describe buttons
        (cl-letf (((symbol-function 'helpful--button)
                   #'ghelp-helpful--button))
          (helpful-update))
        ;; insert an ending newline
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "\n"))
        (prog1 (buffer-string)
          (kill-buffer buf))))))

(defun ghelp-helpful-variable (symbol buffer)
  (when (helpful--variable-p symbol)
    (let ((buf (ghelp-helpful--buffer symbol nil buffer)))
      (with-current-buffer buf
        ;; use our hacked ‘helpful--button’ to insert
        ;; hacked describe buttons
        (cl-letf (((symbol-function 'helpful--button)
                   #'ghelp-helpful--button))
          (helpful-update))
        ;; insert an ending newline
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "\n"))
        (prog1 (let ((yank-excluded-properties nil))
                 (buffer-string))
          (kill-buffer buf))))))

;;; Modified helpful.el code

;; hack this function to insert our hacked buttons
(defun ghelp-helpful--button (text type &rest properties)
  ;; `make-text-button' mutates our string to add properties. Copy
  ;; TEXT to prevent mutating our arguments, and to support 'pure'
  ;; strings, which are read-only.
  (setq text (substring-no-properties text))
  (apply #'make-text-button
         text nil
         :type (cond ((eq type 'helpful-describe-button)
                      'ghelp-helpful-describe-button)
                     ((eq type 'helpful-describe-exactly-button)
                      'ghelp-helpful-describe-exactly-button)
                     (t type))
         properties))

;; hacked buttons invoke ghelp functions instead of helpful ones

(define-button-type 'ghelp-helpful-describe-button
  'action #'ghelp-helpful--describe
  'symbol nil
  'follow-link t
  'help-echo "Describe this symbol")

(defun ghelp-helpful--describe (button)
  "Describe the symbol that this BUTTON represents."
  (let* ((sym (button-get button 'symbol))
         (doc-v (ghelp-helpful-variable sym nil))
         (doc-f (ghelp-helpful-callable sym nil))
         (ev (when doc-v (make-ghelp-entry
                          :name (format "%s (variable)" sym)
                          :text doc-v)))
         (ef (when doc-f (make-ghelp-entry
                          :name (format "%s (callable)" sym)
                          :text doc-f)))
         (entry-list (remove nil (list ev ef))))
    (ghelp--show-page sym ghelp-page--mode nil nil
                      entry-list (selected-window))))

(define-button-type 'ghelp-helpful-describe-exactly-button
  'action #'ghelp-helpful--describe-exactly
  'symbol nil
  'callable-p nil
  'follow-link t
  'help-echo "Describe this symbol")

(defun helpful--describe-exactly (button)
  "Describe the symbol that this BUTTON represents.
This differs from `helpful--describe' because here we know
whether the symbol represents a variable or a callable."
  (let ((sym (button-get button 'symbol))
        (callable-p (button-get button 'callable-p))
        (doc-v (ghelp-helpful-variable sym nil))
        (doc-f (ghelp-helpful-callable sym nil))
        (ev (when doc-v (make-ghelp-entry
                         :name (format "%s (variable)" sym)
                         :text doc-v)))
        (ef (when doc-f (make-ghelp-entry
                         :name (format "%s (callable)" sym)
                         :text doc-f)))
        (entry-list (list (if callable-p ef ev))))
    (ghelp--show-page sym ghelp-page--mode nil nil
                      entry-list (selected-window))))

(defun ghelp-helpful--buffer (symbol callable-p buffer)
  ;; I added the BUFFER parameter - Yuan
  "Return a buffer to show help for SYMBOL in."
  (let* ((current-buffer buffer)
         (buf-name
          (format "*helpful %s*"
                  (if (symbolp symbol)
                      (format "%s: %s"
                              (helpful--kind-name symbol callable-p)
                              symbol)
                    "lambda")))
         (buf (get-buffer buf-name)))
    (unless buf
      ;; If we need to create the buffer, ensure we don't exceed
      ;; `helpful-max-buffers' by killing the least recently used.
      (when (numberp helpful-max-buffers)
        (let* ((buffers (buffer-list))
               (helpful-bufs (--filter (with-current-buffer it
                                         (eq major-mode 'helpful-mode))
                                       buffers))
               ;; `buffer-list' seems to be ordered by most recently
               ;; visited first, so keep those.
               (excess-buffers (-drop (1- helpful-max-buffers) helpful-bufs)))
          ;; Kill buffers so we have one buffer less than the maximum
          ;; before we create a new one.
          (-each excess-buffers #'kill-buffer)))

      (setq buf (get-buffer-create buf-name)))

    ;; Initialise the buffer with the symbol and associated data.
    (with-current-buffer buf
      (helpful-mode)
      (setq helpful--sym symbol)
      (setq helpful--callable-p callable-p)
      (setq helpful--start-buffer current-buffer)
      (setq helpful--associated-buffer current-buffer)
      (if (helpful--primitive-p symbol callable-p)
          (setq-local comment-start "//")
        (setq-local comment-start ";")))
    buf))

(provide 'ghelp-helpful)

;;; ghelp-helpful.el ends here
