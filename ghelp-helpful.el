;;; ghelp-helpful.el --- Ghelp+Helpful      -*- lexical-binding: t; -*-

;; Author (partially): Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'helpful)

;;; New code

(defun ghelp-helpful-describe-symbol (symbol point)
  (let* ((buffer (marker-buffer point))
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
        (helpful-update)
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
        (helpful-update)
        ;; insert an ending newline
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "\n"))
        (prog1 (buffer-string)
          (kill-buffer buf))))))

;;; Modified helpful.el code



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
