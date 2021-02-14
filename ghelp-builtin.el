;;; ghelp-builtin.el --- Ghelp+builtin      -*- lexical-binding: t; -*-

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This file contains the builtin Help backend for Ghelp. I copied
;; code from various builtin functions and modified them to work with
;; Ghelp.

;;; Code:
;;

(require 'help-fns)
(require 'cl-lib)
;; For `cl--describe-class'.
(require 'cl-extra)

;;; Function

(defun ghelp-help-backend (command data)
  "Help backend.
COMMAND and DATA are described in the Commentary of ghelp.el.
FUNCTION-BACKEND returns the documentation of the symbol as a
function, VARIABLE-BACKEND returns the documentation of the
symbol as a variable, other backends in BACKEND-LIST returns the
documentation of the symbol as other things."
  (ghelp-help-backend-1
   command data #'ghelp-help--function #'ghelp-help--variable
   #'ghelp-help--face #'ghelp-help-cl-type))

(defun ghelp-help-backend-1
    (command data function-backend variable-backend &rest backend-list)
  "Help backend.
COMMAND and DATA are described in the Commentary of ghelp.el.
FUNCTION-BACKEND returns the documentation of the symbol as a
function, VARIABLE-BACKEND returns the documentation of the
symbol as a variable, other backends in BACKEND-LIST returns the
documentation of the symbol as other things."
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
     (let ((original-buffer (marker-buffer (plist-get data :marker))))
       (if-let ((kmacro (plist-get data :kmacro)))
           ;; Describe a keyboard macro.
           (let ((macro-name (plist-get data :symbol-name)))
             (list (list
                    macro-name
                    (format "%s is a keyboard macro that expands to %s"
                            macro-name
                            (key-description kmacro)))))
         ;; Describe a symbol.
         (let ((symbol (intern-soft (plist-get data :symbol-name))))
           ;; But wait, symbol could be a keymap, which helpful
           ;; doesn’t support yet.
           (if (keymapp (symbol-function symbol))
               (format "%s is a sparse keymap. Meaning it is used as a prefix key" symbol)
             ;; Normal symbol.
             (let* ((symbol (intern-soft (plist-get data :symbol-name)))
                    (category (plist-get data :category))
                    (func-doc (funcall function-backend symbol
                                       original-buffer))
                    (var-doc (funcall variable-backend symbol
                                      original-buffer))
                    (entry-list
                     ;; If the user only requested for
                     ;; function/variable, only show
                     ;; function/variable.
                     (pcase category
                       ('function (list func-doc))
                       ('variable (list var-doc))
                       (_ (append
                           (list func-doc var-doc)
                           (cl-loop
                            for fn in backend-list
                            collect
                            (funcall fn symbol original-buffer)))))))
               (remove nil entry-list)))))))))

(defun ghelp-help--function (symbol _)
  "Return documentation for SYMBOL.
SYMBOL could be a function, a macro, or a keyboard macro."
  (with-temp-buffer
    (let ((function symbol)
          (standard-output (current-buffer)))
      (when (fboundp symbol)
        (let ((start (point)))
          (prin1 symbol)
          (princ " is ")
          (help-fns-function-description-header function)
          (fill-region-as-paragraph start (point)))
        (terpri) (terpri)
        (pcase-let* ((`(,real-function ,def ,_aliased ,real-def)
                      (help-fns--analyze-function function))
                     (doc-raw (condition-case nil
                                  ;; FIXME: Maybe `documentation' should return nil
                                  ;; for invalid functions i.s.o. signaling an error.
                                  (documentation function t)
                                ;; E.g. an alias for a not yet defined function.
                                ((invalid-function void-function) nil)))
                     (key-bindings-buffer (current-buffer)))

          ;; If the function is autoloaded, and its docstring has
          ;; key substitution constructs, load the library.
          (and (autoloadp real-def) doc-raw
               help-enable-autoload
               (string-match "\\([^\\]=\\|[^=]\\|\\`\\)\\\\[[{<]" doc-raw)
               (autoload-do-load real-def))

          (help-fns--key-bindings function)
          (with-current-buffer standard-output
            (let ((doc (condition-case nil
                           ;; FIXME: Maybe `help-fns--signature' should return `doc'
                           ;; for invalid functions i.s.o. signaling an error.
                           (help-fns--signature
                            function doc-raw
                            (if (subrp def) (indirect-function real-def) real-def)
                            real-function key-bindings-buffer)
                         ;; E.g. an alias for a not yet defined function.
                         ((invalid-function void-function) doc-raw))))
              (help-fns--ensure-empty-line)
              (run-hook-with-args 'help-fns-describe-function-functions function)
              (help-fns--ensure-empty-line)
              (insert (or doc "Not documented.")))
            ;; Avoid asking the user annoying questions if she decides
            ;; to save the help buffer, when her locale's codeset
            ;; isn't UTF-8.
            (unless (memq text-quoting-style '(straight grave))
              (set-buffer-file-coding-system 'utf-8))))
        (list (format "%s (%s)" symbol
                      (if (symbolp symbol)
                          (if (functionp (symbol-function symbol))
                              "function" "macro")
                        "keyboard macro"))
              (buffer-string))))))

(defun ghelp-help--variable (symbol buffer)
  "Return documentation for SYMBOL as a variable.
BUFFER is the original buffer the symbol is in."
  (let (file-name
        (frame (selected-frame))
        (variable symbol))
    (with-temp-buffer
      (if (not (with-current-buffer buffer (boundp variable)))
          nil
        (let ((standard-output (current-buffer))
              val val-start-pos locus)
          ;; Extract the value before setting up the output buffer,
          ;; in case `buffer' *is* the output buffer.
          (with-selected-frame frame
	    (with-current-buffer buffer
	      (setq val (symbol-value variable)
		    locus (variable-binding-locus variable))))
          
          (with-current-buffer buffer
	    (prin1 variable)
	    (setq file-name (find-lisp-object-file-name variable 'defvar))

	    (princ (if file-name
		       (progn
		         (princ (format-message
                                 " is a variable defined in `%s'.\n"
                                 (if (eq file-name 'C-source)
                                     "C source code"
                                   (help-fns-short-filename file-name))))
		         (with-current-buffer standard-output
		           (save-excursion
			     (re-search-backward (substitute-command-keys
                                                  "`\\([^`']+\\)'")
                                                 nil t)
			     (help-xref-button 1 'help-variable-def
					       variable file-name)))))))
          (with-current-buffer standard-output
	    (setq val-start-pos (point))
	    (princ "Its value is")
	    (let ((line-beg (line-beginning-position))
		  (print-rep
		   (let ((rep
			  (let ((print-quoted t)
                                (print-circle t))
			    (cl-prin1-to-string val))))
		     (if (and (symbolp val) (not (booleanp val)))
			 (format-message "`%s'" rep)
		       rep))))
	      (if (< (+ (length print-rep) (point) (- line-beg)) 68)
		  (insert " " print-rep)
	        (terpri)
                (let ((buf (current-buffer)))
                  (with-temp-buffer
                    (lisp-mode-variables nil)
                    (set-syntax-table emacs-lisp-mode-syntax-table)
                    (insert print-rep)
                    (pp-buffer)
                    (let ((pp-buffer (current-buffer)))
                      (with-current-buffer buf
                        (insert-buffer-substring pp-buffer)))))
                ;; Remove trailing newline.
                (and (= (char-before) ?\n) (delete-char -1)))
	      (let* ((sv (get variable 'standard-value))
		     (origval (and (consp sv)
				   (condition-case nil
				       (eval (car sv) t)
				     (error :help-eval-error))))
                     from)
	        (when (and (consp sv)
                           (not (equal origval val))
                           (not (equal origval :help-eval-error)))
		  (princ "\nOriginal value was \n")
		  (setq from (point))
		  (if (and (symbolp origval) (not (booleanp origval)))
		      (let* ((rep (cl-prin1-to-string origval))
			     (print-rep (format-message "`%s'" rep)))
		        (insert print-rep))
		    (cl-prin1 origval))
                  (save-restriction
                    (narrow-to-region from (point))
                    (save-excursion (pp-buffer)))
		  (if (< (point) (+ from 20))
		      (delete-region (1- from) from))))))
          (terpri)
          (when locus
	    (cond
             ((bufferp locus)
              (princ (format "Local in buffer %s; "
                             (buffer-name buffer))))
             ((terminal-live-p locus)
              (princ (format "It is a terminal-local variable; ")))
             (t
              (princ (format "It is local to %S" locus))))
	    (if (not (default-boundp variable))
	        (princ "globally void")
	      (let ((global-val (default-value variable)))
	        (with-current-buffer standard-output
	          (princ "global value is ")
	          (if (eq val global-val)
		      (princ "the same.")
		    (terpri)
		    ;; Fixme: pp can take an age if you happen to
		    ;; ask for a very large expression.  We should
		    ;; probably print it raw once and check it's a
		    ;; sensible size before prettyprinting.  -- fx
		    (let ((from (point)))
                      (cl-prin1 global-val)
                      (save-restriction
                        (narrow-to-region from (point))
                        (save-excursion (pp-buffer)))
		      ;; See previous comment for this function.
		      ;; (help-xref-on-pp from (point))
		      (if (< (point) (+ from 20))
		          (delete-region (1- from) from)))))))
            (terpri))

          ;; If the value is large, move it to the end.
          (with-current-buffer standard-output
	    (when (> (count-lines (point-min) (point-max)) 10)
	      ;; Note that setting the syntax table like below
	      ;; makes forward-sexp move over a `'s' at the end
	      ;; of a symbol.
	      (set-syntax-table emacs-lisp-mode-syntax-table)
	      (goto-char val-start-pos)
	      ;; The line below previously read as
	      ;; (delete-region (point) (progn (end-of-line) (point)))
	      ;; which suppressed display of the buffer local value for
	      ;; large values.
	      (when (looking-at "value is") (replace-match ""))
	      (save-excursion
	        (insert "\n\nValue:")
	        (set (make-local-variable 'help-button-cache)
		     (point-marker)))
	      (insert "value is shown ")
	      (insert-button "below"
			     'action help-button-cache
			     'follow-link t
			     'help-echo "mouse-2, RET: show value")
	      (insert ".\n")))
          (terpri)

          (let* ((alias (condition-case nil
                            (indirect-variable variable)
                          (error variable)))
                 (doc (or (documentation-property
                           variable 'variable-documentation)
                          (documentation-property
                           alias 'variable-documentation))))

            (with-current-buffer standard-output
              (help-fns--ensure-empty-line))
	    (princ "Documentation:\n")
	    (with-current-buffer standard-output
	      (insert (or doc "Not documented as a variable."))))

          (with-current-buffer standard-output
	    ;; Return the text we displayed.
	    (list (format "%s (variable)" variable) (buffer-string))))))))

;;; Face

(defun ghelp-help--face (symbol _)
  "Return the documentation for SYMBOL as a face."
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
            (list (format "%s (face)" symbol) (buffer-string))))))))

;;; cl-class

(defun ghelp-help-cl-type (symbol _)
  "Return the documentation for SYMBOL as a CL type."
  (with-temp-buffer
    (let ((standard-output (current-buffer))
          (symbol (intern-soft symbol)))
      (when-let ((class (cl-find-class symbol)))
        (cl--describe-class symbol class)
        (let ((yank-excluded-properties nil))
          (list (format "%s (type)" symbol) (buffer-string)))))))

(provide 'ghelp-builtin)

;;; ghelp-builtin.el ends here
