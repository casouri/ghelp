;;; ghelp-test.el --- Ghelp tests      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'ert)
(require 'ghelp)

;; TODO: Update the test.
;; (ert-deftest ert-test-history ()
;;   "Create a history, push, jump and find."
;;   (let* ((modes '(A B C D))
;;          (symbols '(a b c d))
;;          (buffers (cl-loop for idx from 0 to 3
;;                            collect (generate-new-buffer "test")))
;;          (nodes (cl-loop
;;                  for idx from 0 to 3
;;                  for node = (make-ghelp-history-node
;;                              :mode (nth idx modes)
;;                              :symbol-name (nth idx symbols)
;;                              :buffer (nth idx buffers))
;;                  collect node))
;;          (history (make-ghelp-history :nodes nodes))
;;          buf)
;;     ;; a b c *d
;;     (ghelp-history--goto history 'd)
;;     (should (eq 3 (ghelp-history-point history)))

;;     ;; a b c *d
;;     (setq buf (ghelp-history--find history 'a))
;;     (should (equal buf (nth 0 buffers)))

;;     ;; *b a c d
;;     (setq buf (ghelp-history--find-and-move history 'b))
;;     (should (eq 0 (ghelp-history-point history)))
;;     (should (eq buf (nth 1 buffers)))

;;     ;; b *a c d
;;     (ghelp-history--forward history 2)
;;     (ghelp-history--back history 1)
;;     (should (eq 1 (ghelp-history-point history)))

;;     ;; clean up buffers
;;     (dolist (node nodes)
;;       (kill-buffer (ghelp-history-node-buffer node)))))

(provide 'ghelp-test)

;;; ghelp-test.el ends here
