;;; ghelp-face.el --- Ghelp+Face      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(defun ghelp-face-describe-symbol (symbol buffer point)
  "Display the properties of face FACE on FRAME.
Interactively, FACE defaults to the faces of the character after point
and FRAME defaults to the selected frame.

If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame."
  (let* ((face (intern-soft symbol))
         (attrs '((:family . "Family")
		  (:foundry . "Foundry")
		  (:width . "Width")
		  (:height . "Height")
		  (:weight . "Weight")
		  (:slant . "Slant")
		  (:foreground . "Foreground")
		  (:distant-foreground . "DistantForeground")
		  (:background . "Background")
		  (:underline . "Underline")
		  (:overline . "Overline")
		  (:strike-through . "Strike-through")
		  (:box . "Box")
		  (:inverse-video . "Inverse")
		  (:stipple . "Stipple")
		  (:font . "Font")
		  (:fontset . "Fontset")
		  (:inherit . "Inherit")))
         (max-width (apply #'max (mapcar #'(lambda (x) (length (cdr x)))
                                         attrs)))
         (frame nil))
    (when (facep face)
      (unless face
        (setq face 'default))
      (if (not (listp face))
          (setq face (list face)))
      (with-temp-buffer
        (let ((standard-output (current-buffer)))
          (dolist (f face (buffer-string))
            (if (stringp f) (setq f (intern f)))
            ;; We may get called for anonymous faces (i.e., faces
            ;; expressed using prop-value plists).  Those can't be
            ;; usefully customized, so ignore them.
            (when (symbolp f)
              (insert "Face: " (symbol-name f))
              (if (not (facep f))
                  (insert "   undefined face.\n")
                (let ((customize-label "customize this face")
                      file-name)
                  (insert (concat " (" (propertize "sample" 'font-lock-face f) ")"))
                  (princ (concat " (" customize-label ")\n"))
                  ;; FIXME not sure how much of this belongs here, and
                  ;; how much in `face-documentation'.  The latter is
                  ;; not used much, but needs to return nil for
                  ;; undocumented faces.
                  (let ((alias (get f 'face-alias))
                        (face f)
                        obsolete)
                    (when alias
                      (setq face alias)
                      (insert
                       (format-message
                        "\n  %s is an alias for the face `%s'.\n%s"
                        f alias
                        (if (setq obsolete (get f 'obsolete-face))
                            (format-message
                             "  This face is obsolete%s; use `%s' instead.\n"
                             (if (stringp obsolete)
                                 (format " since %s" obsolete)
                               "")
                             alias)
                          ""))))
                    (insert "\nDocumentation:\n"
                            (substitute-command-keys
                             (or (face-documentation face)
                                 "Not documented as a face."))
                            "\n\n"))
                  (with-current-buffer standard-output
                    (save-excursion
                      (re-search-backward
                       (concat "\\(" customize-label "\\)") nil t)
                      (help-xref-button 1 'help-customize-face f)))
                  (setq file-name (find-lisp-object-file-name f 'defface))
                  (when file-name
                    (princ (substitute-command-keys "Defined in `"))
                    (princ (file-name-nondirectory file-name))
                    (princ (substitute-command-keys "'"))
                    ;; Make a hyperlink to the library.
                    (save-excursion
                      (re-search-backward
                       (substitute-command-keys "`\\([^`']+\\)'") nil t)
                      (help-xref-button 1 'help-face-def f file-name))
                    (princ ".")
                    (terpri)
                    (terpri))
                  (dolist (a attrs)
                    (let ((attr (face-attribute f (car a) frame)))
                      (insert (make-string (- max-width (length (cdr a))) ?\s)
                              (cdr a) ": " (format "%s" attr))
                      (if (and (eq (car a) :inherit)
                               (not (eq attr 'unspecified)))
                          ;; Make a hyperlink to the parent face.
                          (save-excursion
                            (re-search-backward ": \\([^:]+\\)" nil t)
                            (help-xref-button 1 'help-face attr)))
                      (insert "\n")))))
              (terpri)))
          (let ((yank-excluded-properties nil))
            (list (list symbol (buffer-string)))))))))

(provide 'ghelp-face)

;;; ghelp-face.el ends here
