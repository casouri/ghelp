;;; ghelp.el --- Generic help      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;;;; Usage

;; Use these functions:

;; - ‘ghelp-describe’
;; - ‘ghelp-describe-at-point’

;;;; Terminology

;; - ghelp   :: this package

;; - page    :: the buffer displaying documentation

;; - backend :: the backend providing documentation. Each major mode
;;              has one backend

;; - entry   :: page is made of entries. Each entry is a self-contained
;; documentation for the symbol. Each symbol can be
;; interpreted in different ways and we present each
;; interpretation’s documentation in a entry.

;; - history :: Each major-mode has it’s own page history

;;;; Page anatomy

;; <entry>
;; <newline>
;; <entry>
;; <newline>
;; ...

;;;; Entry anatomy

;; <title>
;; <text>

;;;; Backends

;; Each major mode has one backend that can be accessed through
;; ‘ghelp-describe’ function. The backend gets the symbol, finds
;; the documentation and returns the symbol and documentation.

;;;; History

;; ‘ghelp-history’ is specialized for ghelp:

;; 1. new nodes are always added to the end of the history
;; 2. when looking for a particular node, you can optionally
;; move that node to the end of the history. See
;; ‘ghelp-history--find-and-move’.
;; 3. pushing into history moves point to the end of the history.

;;;; Code structure

;; ghelp is made of several parts: ghelp-describe, ghelp-history,
;; ghelp-page, ghelp-entry, ghelp-backend. They don’t know the detail of
;; each other and only communicate by “exposed” functions. You can find
;; the “exposed” functions on the beginning of each section.

;;; Code:
;;

(require 'cl-lib)
(require 'pcase)

;;; Global

(defun ghelp-describe-as-in (mode &rest args)
  "Return a describe function that thinks it’s in MODE.
ARGS is passed to ‘describe-function’."
  (let ((ghelp--overwrite-mode mode))
    (ignore ghelp--overwrite-mode)
    (apply #'ghelp-describe args)))

(defun ghelp-describe-as-in-emacs-lisp-mode (&rest args)
  "Describe as if in ‘emacs-lisp-mode’, ARGS is passed to ‘describe-function’."
  (interactive)
  (apply #'ghelp-describe-as-in 'emacs-lisp-mode args))

(defvar ghelp-map (let ((map (make-sparse-keymap)))
                    (define-key map (kbd "C-h") #'ghelp-describe-symbol)
                    (define-key map (kbd "C-p") #'ghelp-describe-at-point)
                    (define-key map "C-r" #'ghelp-resume)
                    (define-key map "h" #'help-command)
                    (define-key map "e" #'ghelp-describe-as-in-emacs-lisp-mode)
                    map)
  "Map for ghelp. Bind this map to some entry key sequence.")

(define-minor-mode ghelp-global-minor-mode
  "Setup ghelp backends."
  :lighter ""
  :global t
  (if (not ghelp-global-minor-mode)
      (setq ghelp-backend-alist nil)
    (when (require 'helpful nil t)
      (require 'ghelp-helpful)
      (ghelp-register-backend 'emacs-lisp-mode #'ghelp-helpful-backend))
    (when (require 'eglot nil t)
      (require 'ghelp-eglot)
      (ghelp-register-backend ghelp-eglot-supported-modes
                              #'ghelp-eglot-backend))))

;;; Etc

(defvar ghelp-mode-share-alist `((lisp-interaction-mode . emacs-lisp-mode))
  "An alist specifing how major modes shares documentations.

An entry like (major-mode1 . major-mode2) makes MAJOR-MODE1
share the history and backends of MAJOR-MODE2.

If there is another entry (major-mode2 . major-mode3), then
both MAJOR-MODE1 and MAJOR-MODE2 shares with MAJOR-MODE3.

The maximum levels you can connect these references depends
on ‘ghelp--max-reference-count’.")

(defvar ghelp--max-reference-count 17
  "Maximum number of levels of reference allowed in ‘ghelp-mode-share-alist’.")

(defun ghelp--resolve-mode (mode)
  "Return the major mode that MODE points to in ‘ghelp-mode-share-alist’.
If MODE doesn’t point to anything, return itself."
  (let (prev-mode)
    ;; if MODE doesn’t point to anything anymore (MODE = nil),
    ;; return it (PREV-MODE)
    (while mode
      (setq prev-mode mode
            mode (alist-get mode ghelp-mode-share-alist)))
    prev-mode))

(defvar-local ghelp--overwrite-mode nil
  "Overwrite ‘major-mode’ with this mode.")

(defun ghelp-get-mode ()
  "Return major mode for use."
  (or ghelp-page--mode
      (ghelp--resolve-mode
       (or ghelp--overwrite-mode
           major-mode))))

;;; Commands

(defun ghelp-close ()
  "Close ghelp buffer."
  (interactive)
  (cl-loop for buffer1 in (window-prev-buffers)
           for buffer = (car buffer1)
           for is-ghelp = (eq 'ghelp-page-mode
                              (buffer-local-value
                               'major-mode buffer))
           if (not is-ghelp)
           do (switch-to-buffer buffer)
           and return nil
           finally (delete-window)))

(defun ghelp-resume ()
  "Resume to last opened page."
  (interactive)
  (let* ((mode (ghelp--resolve-mode (ghelp-get-mode)))
         (history (ghelp--get-history mode))
         (page (ghelp-history--current-page history)))
    (if page
        (let ((win (display-buffer page)))
          (when help-window-select
            (select-window win)))
      (user-error "Can’t find a previous page for mode %s" mode))))

(defun ghelp-refresh ()
  "Refresh current page."
  (interactive)
  (ghelp-describe t ghelp-page--symbol))

(defun ghelp-completing-read (default-symbol &rest args)
  "‘completing-read’ with two improvements.

1. Compose prompt with DEFAULT-SYMBOL (string or nil) as
   “Describe (default DEFAULT-SYMBOL): ”.
2. If gets empty string, return DEFAULT-SYMBOL.

\(fn DEFAULT-SYMBOL COLLECTION &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)"
  (let* ((prompt (format "Describe%s: "
                         (if default-symbol
                             (format " (default %s)" default-symbol)
                           "")))
         (symbol (apply #'completing-read prompt args)))
    (if (equal symbol "") default-symbol symbol)))

(defun ghelp-describe (&optional no-prompt symbol)
  "Describe symbol.

Select PAGE if ‘help-window-select’ is non-nil. If NO-PROMPT
non-nil, no prompt. If SYMBOL is non-nil, describe SYMBOL.
SYMBOL is intended for internal use."
  (interactive)
  (let* ((mode (ghelp-get-mode))
         (backend (ghelp--get-backend mode))
         (window (when (derived-mode-p 'ghelp-page-mode)
                   (selected-window))))
    (pcase-let* ((`(,symbol ,entry-list ,store)
                  (funcall backend no-prompt symbol)))
      (ghelp--show-page symbol entry-list store mode window))))

(defun ghelp-describe-at-point ()
  "Describe symbol at point."
  (interactive)
  (ghelp-describe t))

;;; History
;;
;; Functions:
;;
;; - ‘ghelp--get-history’
;; - ‘ghelp-history--to-candidates’
;; - ‘ghelp-history--goto’
;; - ‘ghelp-history--find-and-move’
;; - ‘ghelp-history--find’
;; - ‘ghelp-history--push’
;; - ‘ghelp-history--forward’
;; - ‘ghelp-history--back’
;; - ‘ghelp-history--nth-page’
;; - ‘ghelp-history--current-page’

(defvar ghelp-history-max-length 50
  "Maximum length of each history.

Ghelp creates a marker for each history page, so maybe don’t set
this to a super large number.")

(defvar ghelp-history-alist nil
  "A list of (major-mode . history).
HISTORY is the history of documentation queries.")

(cl-defstruct ghelp-history
  "History pages.

 - point :: The index of the current node.
 - nodes :: A list of ‘ghelp-history-node’."
  (point 0)
  (nodes nil))

(cl-defstruct ghelp-history-node
  "A node in a ‘ghelp-history’.

 - mode   :: Major mode.
 - symbol :: Symbol that the documentation describes.
 - buffer :: The ghelp buffer containing the documentation."
  mode
  symbol
  buffer)

(defsubst ghelp--within (min val max)
  "Return val within range [MIN, MAX].
If MIN < VAL < MAX, return VAL,
if VAL <= MIN, return MIN,
if VAL >= MAX, return MAX."
  (max min (min max val)))

(defun ghelp-history--nth-page (idx history)
  "Return the IDX’th page of HISTORY."
  (when-let ((node (nth idx (ghelp-history-nodes history))))
    (ghelp-history-node-buffer node)))

(defun ghelp-history--current-page (history)
  "Return the current page of HISTORY."
  (ghelp-history--nth-page (ghelp-history-point history) history))

(defun ghelp-history--back (history count)
  "Go back COUNT steps in HISTORY, return the corresponding node or nil.

If COUNT there aren’t enough nodes to go back through, stop at
the last one. COUNT can be negative."
  (let* ((nodes (ghelp-history-nodes history))
         (point (ghelp-history-point history))
         (len (length nodes))
         (new-idx (ghelp--within 0 (+ point count) (1- len))))
    (setf (ghelp-history-point history) new-idx)
    (ghelp-history-node-buffer
     (ghelp-history--nth-live new-idx history))))

(defun ghelp-history--forward (history count)
  "Go forward COUNT steps in HISTORY, return the node or nil.

If COUNT there aren’t enough nodes to go forward through, stop at
the last one. COUNT can be negative."
  (ghelp-history--back history (- count)))

(defun ghelp-history--push (buffer mode symbol history)
  "Push BUFFER w/ MODE & SYMBOL onto HISTORY and return BUFFER."
  (pcase-let ((nodes (ghelp-history-nodes history)))
    (setf (ghelp-history-nodes history)
          ;; push a new node
          (cons (make-ghelp-history-node
                 :buffer buffer
                 :mode mode
                 :symbol symbol)
                ;; maybe remove the bottom node if too long
                (if (< (length nodes) ghelp-history-max-length)
                    nodes
                  (kill-buffer (ghelp-history-node-buffer (last nodes)))
                  (butlast nodes)))
          (ghelp-history-point history) 0))

  buffer)

(defun ghelp-history--find (history symbol)
  "Return the page (buffer) describing SYMBOL in HISTORY or nil."
  (when-let ((cell (ghelp-history--find-1 history symbol)))
    (car cell)))

(defun ghelp-history--find-and-move (history symbol)
  "Return the page (buffer) describing SYMBOL in HISTORY or nil.

Unlike ‘ghelp-history--find’, move the node to be the latest node."
  (when-let ((cell (ghelp-history--find-1 history symbol t)))
    (car cell)))

(defun ghelp-history--find-1 (history symbol &optional move)
  "Return (page . index) describing SYMBOL in HISTORY or nil.

If MOVE non-nil, move the node to the end of the history."
  (let ((idx (cl-loop for node       in (ghelp-history-nodes history)
                      for idx         = 0 then (1+ idx)
                      for node-symbol = (ghelp-history-node-symbol node)
                      if (equal symbol node-symbol)
                      return idx
                      finally return nil)))
    (when idx (cons (ghelp-history-node-buffer
                     (ghelp-history--nth-live idx history move))
                    idx))))

(defun ghelp-history--nth-live (idx history &optional move)
  "Return the IDX’th node of HISTORY or nil.

If MOVE non-nil, move the node to the end of HISTORY.
If the buffer of that node is dead, remove it and return nil."
  (let* ((nodes (ghelp-history-nodes history))
         (node (nth idx nodes))
         (buf (ghelp-history-node-buffer node))
         ;; nodes minus the node
         (nodes-w/o-node (append (cl-subseq nodes 0 idx)
                                 (nthcdr (1+ idx) nodes))))
    (if (not (buffer-live-p buf))
        (prog1 nil
          (setf (ghelp-history-nodes history)
                nodes-w/o-node))
      ;; node has a live buffer, maybe move it
      (when move
        (setf (ghelp-history-point history) 0
              (ghelp-history-nodes history)
              (cons node nodes-w/o-node)))
      ;; return
      node)))

(defun ghelp-history--goto (history symbol)
  "Go to node w/ SYMBOL in HISTORY and return that node or nil."
  (when-let ((cell (ghelp-history--find-1 history symbol)))
    (setf (ghelp-history-point history) (cdr cell))
    (car cell)))

(defun ghelp--get-history (mode)
  "Return the history of major mode MODE or nil."
  (or (alist-get mode ghelp-history-alist)
      ;; TODO remove workaround
      ;; needed for all version before 27.1
      (let ((h (make-ghelp-history)))
        (setf (alist-get mode ghelp-history-alist) h)
        h)))

(defun ghelp-history--to-candidates (history)
  "Return a list of symbols.

The symbols are those in HISTORY and can be used for
‘completing-read’ as COLLECTION."
  (cl-loop for node in (ghelp-history-nodes history)
           collect (ghelp-history-node-symbol node)))

;;; Entry
;;
;; Functions:
;;
;; - ‘ghelp-entry-fold’
;; - ‘ghelp-entry-unfold’
;; - ‘ghelp-previous-entry’
;; - ‘ghelp-next-entry’
;; - ‘ghelp-toggle-entry’

(defvar-local ghelp--page-entry-list nil
  "A list of documentation entries.
Each entry is a ‘ghelp-entry’.")

(defun ghelp-toggle-entry ()
  "Toggle visibility of entry at point."
  (interactive)
  (if (ghelp-entry-folded)
      (ghelp-entry-unfold)
    (ghelp-entry-fold)))

(defmacro ghelp-entry--with-ov (ov-symbol &rest body)
  "Evaluate BODY with OV-SYMBOL bounded to ghelp overlay at point."
  (declare (indent 1))
  `(let ((,ov-symbol (ghelp--overlay-at-point)))
     (if (not ov)
         (user-error "No entries found at point")
       ,@body)))

(defun ghelp-next-entry ()
  "Go to next entry."
  (interactive)
  ;; state 0: on entry
  ;; state 1: not on entry
  ;; state 2: on entry
  (let ((state (if (ghelp--overlay-at-point) 0 1)))
    (condition-case nil
        (while (< state 2)
          (forward-char)
          (pcase state
            (1 (when (ghelp--overlay-at-point)
                 (setq state (1+ state))))
            (0 (when (not (ghelp--overlay-at-point))
                 (setq state (1+ state))))))
      (end-of-buffer nil))))

(defun ghelp-previous-entry ()
  "Go to beginning of the previous entry."
  (interactive)
  ;; state 0: on entry
  ;; state 1: not on entry
  ;; state 2: on entry
  ;; state 3: not on entry
  (let ((state (if (ghelp--overlay-at-point) 0 1)))
    (condition-case nil
        (while (< state 3)
          (backward-char)
          (pcase state
            (1 (when (ghelp--overlay-at-point)
                 (setq state (1+ state))))
            ((or 0 2) (when (not (ghelp--overlay-at-point))
                        (setq state (1+ state))))))
      (beginning-of-buffer nil))
    (when (eq state 3)
      (forward-char))))

(defun ghelp-entry-unfold ()
  "Unfold OVERLAY."
  (interactive)
  (ghelp-entry--with-ov ov
    (ghelp-entry--unfold ov)))

(defun ghelp-entry-fold ()
  "Fold OVERLAY."
  (interactive)
  (ghelp-entry--with-ov ov
    (ghelp-entry--fold ov)))

(defun ghelp-entry-folded ()
  "Return non-nil if OVERLAY is folded in terms of ghelp entry."
  (ghelp-entry--with-ov ov
    (ghelp-entry--folded ov)))

(defun ghelp-entry--folded (overlay)
  "Return non-nil if OVERLAY is folded in terms of ghelp entry."
  (overlay-get overlay 'display))

(defun ghelp-entry--fold (overlay)
  "Fold OVERLAY."
  (overlay-put overlay 'face 'ghelp-folded-entry)
  (overlay-put overlay 'display (overlay-get
                                 overlay 'ghelp-entry-name)))



(defun ghelp-entry--unfold (overlay)
  "Unfold OVERLAY."
  (overlay-put overlay 'display nil)
  (overlay-put overlay 'face 'ghelp-entry))

;;; Page
;;
;; - ‘ghelp-page-insert-entry’
;; - ‘ghelp-page-clear’
;; - ‘ghelp-get-page-or-create’
;; - ‘ghelp-switch-to-page’
;; - ‘ghelp-forward’
;; - ‘ghelp-back’
;; - ‘ghelp-folded-entry’
;; - ‘ghelp-entry’
;; - ‘ghelp--show-page’
;; - ‘ghelp-page-store-get’
;; - ‘ghelp-page-store-set’
;; - ‘ghelp-page--mode’
;; - ‘ghelp-page--symbol’

;;;;; Modes

(defvar ghelp-page-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'ghelp-toggle-entry)
    (define-key map "q" #'ghelp-close)
    (define-key map "b" #'ghelp-back)
    (define-key map "f" #'ghelp-forward)
    (define-key map "n" #'ghelp-next-entry)
    (define-key map "p" #'ghelp-previous-entry)
    (define-key map "m" #'forward-button)
    (define-key map "[" #'backward-button)
    (define-key map (kbd "SPC") #'scroll-up-command)
    (define-key map (kbd "<backspace>") #'scroll-down-command)
    (define-key map "g" #'ghelp-refresh)
    (define-key map "s" #'ghelp-switch-to-page)
    map))

(define-derived-mode ghelp-page-mode fundamental-mode
  "Ghelp" "Major mode for ghelp pages."
  (setq buffer-read-only t))

;;;;; Variables

(defface ghelp-entry (let ((display t)
                           (atts '(:inherit default)))
                       `((,display . ,atts)))
  "Face for each entry in a documentation."
  :group 'ghelp)

(defface ghelp-folded-entry (let ((display t)
                                  (atts '(:inherit highlight)))
                              `((,display . ,atts)))
  "Face for a folded entry in a documentation."
  :group 'ghelp)

(defface ghelp-entry-title (let ((display t)
                                 (atts '(:inherit (ghelp-entry
                                                   info-title-3))))
                             `((,display . ,atts)))
  "Face for the title of an entry in a documentation."
  :group 'ghelp)

(defvar-local ghelp-page--history nil
  "A pointer point back to the history containing this page.")

(defvar-local ghelp-page--mode nil
  "Mode that was passed to ‘ghelp-show-page’.")

(defvar-local ghelp-page--symbol nil
  "Symbol that was passed to ‘ghelp--show-page’.")

;; (defvar ghelp-entry-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "TAB") #'ghelp-toggle-entry)
;;     map)
;;   "Keymap activated when point is in an entry.")

;; FIXME not working
(defvar ghelp-page--header-line-format
  `(:eval
    (pcase-let (((cl-struct ghelp-history
                            nodes
                            point)
                 ghelp-page--history))
      (concat " "
              ;; [back]
              (unless (eq point (1- (length nodes)))
                (propertize
                 (ghelp--make-button "[back]" #'ghelp-back)
                 face info-header-node
                 mouse-face highlight))
              " "
              ;; [forward]
              (unless (eq point 0)
                (propertize
                 (ghelp--make-button "[forward]" #'ghelp-forward)
                 face info-header-node
                 mouse-face highlight)))))
  "Setup current page’s header line.")

(defvar-local ghelp-page--store nil
  "Store information in page. An alist.")

;;;;; Commands

(defun ghelp-back (&optional count)
  "Go back one or COUNT pages."
  (interactive "p")
  (switch-to-buffer (ghelp-history--back (ghelp-page--history)
                                         (or count 1))))

(defun ghelp-forward (&optional count)
  "Go forward one or COUNT pages."
  (interactive "p")
  (ghelp-back (- count)))

(defun ghelp-switch-to-page ()
  "Switch to a page in history."
  (interactive)
  (let* ((history (ghelp-page--history))
         (symbol (completing-read
                  "Switch to: "
                  (ghelp-history--to-candidates history)
                  nil t))
         (page (ghelp-history--goto history symbol)))
    (switch-to-buffer page)))

;;;;; Functions

(defun ghelp--generate-new-page (mode symbol)
  "Generate a new page for MODE (major mode) and SYMBOL and return it.

!! This function doesn’t set variable ‘ghelp-page--history’. Be aware !!"
  (with-current-buffer (generate-new-buffer
                        (ghelp--page-name-from mode symbol))
    ;; TODO setup buffer
    (ghelp-page-mode)
    (setq ghelp-page--symbol symbol
          ghelp-page--mode mode)
    (current-buffer)))

(defun ghelp-get-page-or-create (mode symbol)
  "Return the page for MODE (major mode) and SYMBOL.
Assume a history is avaliable for MODE, else error.
POINT is the point of the symbol."
  (let* ((history (ghelp--get-history mode))
         ;; find and move the page to the end of history
         (page (ghelp-history--find-and-move history symbol)))
    (or page
        (prog1 (setq page (ghelp--generate-new-page mode symbol))
          (with-current-buffer page
            (setq ghelp-page--history history))
          (ghelp-history--push page mode symbol history)))))

(defun ghelp-page-clear ()
  "Clear PAGE."
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun ghelp-page-insert-entry (entry &optional fold)
  "Insert ENTRY at the end of PAGE (buffer) and return the entry.

If FOLD non-nil, fold the entry after insertion."
  (let ((inhibit-read-only t)
        (name (nth 0 entry))
        (text (nth 1 entry))
        ov beg)
    (save-excursion
      (goto-char (point-max))
      ;; this newline therefore is not included in the overlay
      (insert "\n")
      (backward-char)
      (setq beg (point))
      (insert (propertize (format "%s\n" name) 'face 'ghelp-entry-title)
              text)
      (setq ov (make-overlay beg (point)))
      (overlay-put ov 'ghelp-ov t)
      (overlay-put ov 'ghelp-entry-name name)
      (overlay-put ov 'face 'ghelp-entry)
      ;; FIXME keymap as a symbol doesn’t seem to work
      ;; keymap property blocks text buttons
      ;; (overlay-put ov 'keymap ghelp-entry-map)
      (when fold (ghelp-entry--fold ov))
      entry)))

(defun ghelp-page-insert-entry-list (entry-list &optional fold)
  "Insert entries in ENTRY-LIST one-by-one, FOLD see ‘ghelp-page-insert-entry’."
  (dolist (entry entry-list)
    (ghelp-page-insert-entry entry fold)))

(defun ghelp--overlay-at-point ()
  "Return ghelp overlay at point or nil."
  (catch 'ret
    (let ((overlays (overlays-at (point))))
      (while overlays
        (let ((overlay (car overlays)))
          (when (overlay-get overlay 'ghelp-ov)
            (throw 'ret overlay)))
        (setq overlays (cdr overlays))))))

(defun ghelp--page-name-from (mode symbol)
  "Return the buffer name would be used by ghelp page for MODE and SYMBOL."
  (format " *ghelp %s : %s*" mode symbol))

;; Helpers

(defun ghelp--make-button (text fn)
  "Return a clickable TEXT that invokes FN when clicked by mouse-1."
  (propertize text 'keymap (let ((map (make-sparse-keymap)))
                             (define-key map [down-mouse-1] fn)
                             map)))

(defun ghelp-page--history ()
  "Return non-nil ‘ghelp-page-history’ or error."
  (or ghelp-page--history
      (error "No ‘ghelp-page--history’ found in current buffer")))

(defun ghelp--show-page (symbol entry-list store mode &optional window)
  "Display page for SYMBOL with ENTRY-LIST & STORE in MODE, in WINDOW if non-nil."
  (let ((page (ghelp-get-page-or-create mode symbol)))
    (with-current-buffer page
      (ghelp-page-clear)
      (ghelp-page-insert-entry-list entry-list t)
      (pcase-dolist (`(,key ,value) store)
        (ghelp-page-store-set key value))
      (goto-char
       (point-max))
      (ghelp-previous-entry)
      (ghelp-entry-unfold)
      (setq-local ghelp-page--mode mode)
      (setq-local ghelp-page--symbol symbol))
    (if window
        (window--display-buffer page window 'window)
      (setq window (display-buffer page)))
    (when help-window-select
      (select-window window))))

(defun ghelp-page-store-set (key value)
  "Set KEY VALUE pair in PAGE store."
  (setf (alist-get key ghelp-page--store) value))

(defun ghelp-page-store-get (key)
  "Get KEY from PAGE’s store."
  (alist-get key ghelp-page--store))

;;; Backend
;;
;; Functions:
;;
;; - ‘ghelp--get-backend’

(defvar ghelp-backend-alist ()
  "An alist of (major-mode . backend).")

(defun ghelp--get-backend (mode)
  "Get ghelp backend by MODE."
  (alist-get mode ghelp-backend-alist))

(defun ghelp-register-backend (mode backend-function)
  "Register BACKEND-FUNCTION for each MODE.
MODE can be a major mode symbol or a list of it."
  (cond ((symbolp mode)
         (setf (alist-get mode ghelp-backend-alist) backend-function))
        ((consp mode)
         (dolist (mode1 mode)
           (setf (alist-get mode1 ghelp-backend-alist) backend-function)))
        (t (error "MODE should be either a list or a symbol"))))

;;; Dummy

(defun ghelp-dummy-backend (&optional no-prompt symbol)
  "Demo. No prompt if NO-PROMPT is non-nil.
If SYMBOL non-nil, just describe it, otherwise get a symbol by prompting or guessing."
  (let* ((default-symbol (symbol-at-point))
         (symbol (or symbol
                     ;; get symbol from user, I don’t have to make a prompt though
                     (if no-prompt
                         default-symbol
                       (ghelp-completing-read ; I can also use ‘completing-read’
                        default-symbol
                        '("woome" "veemo" "love" "and" "peace" "many")))))
         ;; just to demo a usage of ‘store’
         (ghelp-page-store-get 'my-var) ; = "cool"
         ;; get documentation
         (entry-list (pcase symbol
                       ;;           title   documentation
                       ("woome" '(("Woome"  "Woome!\n")))
                       ("veemo" '(("Veemo"  "Veemo!\n")))
                       ("love"  '(("Love"   "Love is good.\n")))
                       ("and"   '(("And"    "And is a conjunction.\n")))
                       ("peace" '(("Peace"  "もう大丈夫だ！なぜって？私が来た！\n")))
                       ;; multiple entries
                       ("many"  '(("Many1"  "I’m ONE.\n") ("Many2" "I’m TWO.\n"))))))
    ;; store some value for later use
    (list symbol entry-list `((my-var "cool")))))

(provide 'ghelp)

;;; ghelp.el ends here
