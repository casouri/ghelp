;;; ghelp.el --- Generic help      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This package provides a generic help system similar to Emacs Help.
;; Unlike Emacs Help, ghelp works for more major-modes and is extensible
;; with backends.
;; 
;; *Features*
;; • Unified entry command
;; • Unified UI
;; • Documentation history, you can search in history, go back/forward.
;; 
;; *Currently supported backends*
;; • builtin Help
;; • [helpful]
;; • [eglot]
;; • [geiser]
;; 
;; [☞ Screencasts]
;; 
;; 
;; [helpful] <https://github.com/Wilfred/helpful>
;; 
;; [eglot] <https://github.com/joaotavora/eglot>
;; 
;; [geiser] <https://www.nongnu.org/geiser/>
;; 
;; [☞ Screencasts] <https://github.com/casouri/ghelp#screencasts>
;; 
;; 
;; 1 Install & load
;; ════════════════
;; 
;;   Download the files and add them to load path.
;; 
;;   With `use-package':
;;   ┌────
;;   │ (use-package ghelp)
;;   └────
;;   Without `use-package':
;;   ┌────
;;   │ (require 'ghelp)
;;   └────
;; 
;; 
;; 2 Usage
;; ═══════
;; 
;;   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;    `ghelp-describe'           Describe a symbol in  current major mode  
;;    `gehlp-describe-at-point'  Describe symbol at point (without prompt) 
;;    `ghelp-resume'             Reopen last page                          
;;   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; 
;;   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;    `ghelp-describe-elisp'     Describe a Emacs symbol (like apropos)         
;;    `ghelp-describe-function'  Describe a Elisp function/macro/keyboard macro 
;;    `ghelp-describe-variable'  Describe a Elisp variable                      
;;    `ghelp-describe-key'       Describe a key sequence                        
;;   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; 
;;   Normally `ghelp-describe' shows documentation of the symbol at point,
;;   If you want to query for a symbol (e.g., with completion), type `C-u'
;;   then `ghelp-describe'.
;; 
;; 
;; 3 Enable backends
;; ═════════════════
;; 
;;   Each backend are loaded automatically when you enabled the
;;   corresponding package. For example, when you load `helpful.el', ghelp
;;   automatically loads its helpful backend.
;; 
;; 
;; 4 In ghelp buffer
;; ═════════════════
;; 
;;   A ghelp buffer is called a page. Each page is made of several entries.
;;   Each entry is a self-contained documentation. (For example, you could
;;   have a entry for a symbol as a function and another one for it as a
;;   variable.)
;; 
;;   Commands you can use:
;; 
;;   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;    Key      Command                        
;;   ─────────────────────────────────────────
;;    `?'      show help                      
;;    `f/b'    go forward/backward in history 
;;    `TAB'    next button                    
;;    `S-TAB'  previous button                
;;    `t'      collapse/expand entry          
;;    `g'      refresh page                   
;;    `q'      close page                     
;;    `s'      search/switch to a page        
;;   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; 
;;   For more bindings, type `?' in a ghelp buffer, or type `M-x
;;   ghelp-describe ghelp-page-mode-map RET'.
;; 
;; 
;; 5 Customization
;; ═══════════════
;; 
;;   If you want several major modes to share the same set of history and
;;   backends (like `lisp-interaction-mode' and `emacs-lisp-mode'), add an
;;   entry `(mode1 . mode2)' to `ghelp-mode-share-alist', and `mode1' will
;;   share everything of `mode2'.
;; 
;;   You can customize faces: `ghelp-entry', `ghelp-folded-entry', and
;;   `ghelp-entry-title'.
;; 
;;   Normally if you call `ghelp-describe-function' it selects the backends
;;   to use by the current major-mode. If you want to look up some symbol
;;   with a specific backend, try `(ghelp-describe-with-mode ’prompt
;;   'mode)'. For example, you can bind
;;   ┌────
;;   │ (define-key (kbd "C-h C-e") (lambda () (interactive) (ghelp-describe-with-mode ’force-prompt ’emacs-lisp-mode)))
;;   └────
;;   to look up Emacs Lisp symbols regardless of which major mode you are
;;   currently in.
;; 
;; 
;; 6 Write a backend
;; ═════════════════
;; 
;;   A backend is a function that takes two arguments `COMMAND' and `DATA'.
;; 
;;   If `COMMAND' is `'symbol', return a string representing the symbol
;;   that the user wants documentation for.
;; 
;;   If `COMMAND' is `'doc', return the documentation for `SYMBOL', where
;;   `SYMBOL' is from `DATA':
;;   ┌────
;;   │ (:symbol SYMBOL :marker MARKER)
;;   └────
;;   And `MARKER' is the marker at the point where user invoked
;;   `ghelp-describe'. Returned documentation should be a string ending
;;   with a newline. Return nil if no documentation is found.
;; 
;;   Below is an example backend that gets the symbol and then the
;;   documentation and returns them. It only recognizes “woome”, “veemo”,
;;   “love” and “tank”.
;;   ┌────
;;   │ (defun ghelp-dummy-backend (command data)
;;   │   (pcase command
;;   │     ('symbol (completing-read "Symbol: "
;;   │                               '("woome" "veemo" "love" "tank")))
;;   │     ('doc (pcase (plist-get data :symbol)
;;   │             ("woome" "Woome!!\n")
;;   │             ("veemo" "Veemo!!\n")
;;   │             ("love" "Peace!!\n")
;;   │             ("tank" "TANK! THE! BEST!\n")))))
;;   └────
;;   You can try this out by typing `M-x ghelp-dummy RET'.
;; 
;;   Once you have a backend, register it by
;;   ┌────
;;   │ (ghelp-register-backend 'major-mode #'your-backend-function)
;;   └────
;; 
;; 
;; 7 Advanced backend
;; ══════════════════
;; 
;; 7.1 Returned documentation
;; ──────────────────────────
;; 
;;   Besides a string, the returned documentation could carry more
;;   information.
;; 
;;   First, it can be a list of form `(TITLE BODY)' where `TITLE' is the
;;   title for your documentation, and `BODY' is the body of your
;;   documentation. This way you can use a title other than the symbol
;;   name.
;; 
;;   Second, you can return multiple documentations by returning a list
;;   `((TITLE BODY)...)', where each element is a `(TITLE BODY)' form.
;; 
;; 
;; 7.2 Use buttons in your documentation
;; ─────────────────────────────────────
;; 
;;   You can use buttons in your documentation as long they are text
;;   buttons made by text properties, rather than overlay buttons. After
;;   all your are returning a string, which doesn’t carry overlays.
;; 
;;   However, one problem might arise if the command invoked by your button
;;   needs some information, like the symbol that this documentation page
;;   is describing. You can get that by `(ghelp-get-page-data)', which
;;   returns a plist of form
;;   ┌────
;;   │ (:symbol SYMBOL :mode MODE :marker MARKER)
;;   └────
;;   `SYMBOL' and `MARKER' are the same as before, `MODE' is the major
;;   mode.
;; 
;; 
;; 7.3 Use a phony major mode
;; ──────────────────────────
;; 
;;   Normally each backend is tied to an actual major mode. But if you want
;;   to write a backend that doesn’t associate with any major mode, like a
;;   dictionary, you can use `ghelp-describe-with-mode', and use
;;   `dictionary' as your “major mode”.
;; 
;; 
;; 8 Screencasts
;; ═════════════
;; 
;;   *Eglot*
;; 
;;   <file:./ghelp-eglot-800.gif>
;; 
;;   *Helpful*
;; 
;;   <file:./ghelp-helpful-800.gif>
;;
;; Commentary end

;;; Developer:

;;;; Development plan:

;; - Allow backends to use their own buffers. This also allows backends
;;   to be async.

;;;; Terminology

;; - ghelp   :: This package.
;;
;; - page    :: The buffer displaying documentation.
;;
;; - backend :: The backend providing documentation. Each major mode
;;              has one backend
;;
;; - entry :: Page is made of entries. Each entry is a self-contained
;;            documentation for the symbol. Each symbol can be
;;            interpreted in different ways and we present each
;;            interpretation’s documentation in a entry.
;;
;; - history :: Each major-mode has it’s own page history.

;;;; Page anatomy

;; A page is made of a series of entries. Each entry is made of a
;; title and a documentation body.

;;;; Backends

;; Each major mode has one backend that can be accessed through
;; ‘ghelp-describe’ function.  Multiple major modes could share
;; the same backend (and history).

;;;; History

;; Each major mode has a page history. Though multiple major mode
;; could share the same history. A history has a list of nodes. The list
;; is sorted from newest node to oldest node (so we can remove oldest
;; node when necessary). The nodes themselves constructs a
;; doubly-linked list. This list is sorted in logical order -- every
;; time when ghelp creates a new page, it inserts the page after the
;; last viewed page. For example, suppose this is our history:
;;
;;    A - B - C - D
;;
;; And the last viewed page is C. Now, if the user requested for the
;; documentation for E, E is inserted after C and before D:
;;
;;    A - B - C - E - D
;;

;;;; Code structure

;; Ghelp contains self-contained sub-modules: ghelp-entry, ghelp-page,
;; and ghelp-history. Other code don’t know the detail of them and
;; only communicate with them by “exposed” functions. (At least that’s
;; what I attempt to do.) You can find the “exposed” functions on the
;; beginning of each section.

;;; Code:
;;

(require 'cl-lib)
(require 'pcase)
(require 'seq)

;;; Global

(defun ghelp-describe-elisp ()
  "Describe Emacs symbol."
  (interactive)
  (ghelp-describe-with-mode 'force-prompt 'emacs-lisp-mode))

(defvar ghelp-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-h") #'ghelp-describe)
    (define-key map "h" #'help-command)
    (define-key map "e" #'ghelp-describe-elisp)
    (define-key map "f" #'ghelp-describe-function)
    (define-key map "v" #'ghelp-describe-variable)
    (define-key map "k" #'ghelp-describe-key)
    (define-key map "r" #'ghelp-resume)
    map)
  "Map for ghelp. Bind this map to some entry key sequence.")

;;; Etc

(defvar ghelp-mode-share-alist `((lisp-interaction-mode . emacs-lisp-mode)
                                 ;; Without this setting ghelp can’t
                                 ;; resolve the backend if you call
                                 ;; helpful commands directly (instead
                                 ;; of through ‘ghelp-describe’), e.g,
                                 ;; ‘helpful-key’.
                                 (helpful-mode . emacs-lisp-mode))
  "An alist specifying how major modes shares documentations.

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

(defun ghelp-get-mode ()
  "Return major mode for use."
  (ghelp--resolve-mode
   (if (derived-mode-p 'ghelp-page-mode)
       (plist-get ghelp-page-data :mode)
     major-mode)))

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
         (page (ghelp-history--current-page mode)))
    (if page
        (let ((win (display-buffer page)))
          (when help-window-select
            (select-window win)))
      (user-error "Can’t find a previous page for mode %s" mode))))

(defun ghelp-refresh ()
  "Refresh current page."
  (interactive)
  (if (derived-mode-p 'ghelp-page-mode)
      (ghelp-describe-1 'no-prompt (copy-tree ghelp-page-data))
    (user-error "Not in a ghelp page")))

(defun ghelp-describe (prompt)
  "Describe symbol.
When called interactively, use prefix argument to force prompt.

PROMPT"
  (interactive "p")
  (let ((prompt (if (eq prompt 4) 'force-prompt nil)))
    (ghelp--maybe-update-current-page)
    (ghelp-describe-1 prompt nil)))

(defun ghelp-describe-with-mode (prompt mode)
  "Describe symbol for MODE.

PROMPT can be 'no-prompt, 'force-prompt or nil:

no-prompt     means don’t prompt for symbol;
force-prompt  means always prompt for symbol;
nil           means only prompt when there is no valid symbol at point.

MODE is the major mode of the symbol your want to describe."
  (ghelp--maybe-update-current-page)
  (ghelp-describe-1 prompt `(:mode ,mode)))

(defun ghelp-describe-1 (prompt data)
  "Describe symbol.

PROMPT is the same as in ‘ghelp-describe-with-mode’.

DATA is a plist of form (:symbol SYMBOL :mode MODE :marker MARKER).
SYMBOL is the symbol we want to describe, MODE is the major mode,
MARKER is the marker at where user requested for documentation.

If SYMBOL is nil, we try to guess or prompt for the symbol.
If MODE is nil, we use current buffer’s major mode.
If MARKER is nil, we use the marker at point."
  (interactive "p")
  (let* ((mode (or (plist-get data :mode) (ghelp-get-mode)))
         (symbol (plist-get data :symbol))
         (marker (or (plist-get data :marker) (point-marker)))
         (backend (ghelp--get-backend mode))
         (window (when (derived-mode-p 'ghelp-page-mode)
                   (selected-window)))
         doc)
    (setq data (plist-put data :mode mode))
    (setq data (plist-put data :marker marker))
    (when (not backend)
      (user-error "No backend found for %s" major-mode))
    ;; Get symbol.
    (when (not symbol)
      (setq symbol (let* ((sym (when-let ((sym (symbol-at-point)))
                                 (symbol-name sym))))
                     (pcase prompt
                       ('no-prompt sym)
                       ('force-prompt
                        (funcall backend 'symbol (copy-tree data)))
                       ('nil
                        (or sym (funcall backend 'symbol
                                         (copy-tree data)))))))
      (if (not (stringp symbol))
          (error "Symbol return by the backend is not a string"))
      ;; Still no symbol?
      (when (not symbol)
        (user-error "No symbol at point"))
      (setq data (plist-put data :symbol symbol)))
    ;; Request for documentation.
    (setq doc (funcall backend 'doc (copy-tree data)))
    (when (not doc)
      (user-error "No documentation found for %s" symbol))
    ;; Handle different kinds of doc.
    (cond ((stringp doc)
           (setq doc `((,symbol ,doc))))
          ((stringp (car doc))
           (setq doc (list doc))))
    (ghelp--show-page doc data window)))

(defun ghelp-describe-at-point ()
  "Describe symbol at point."
  (interactive)
  (ghelp--maybe-update-current-page)
  (ghelp-describe 'no-prompt))

(defun ghelp-describe-function ()
  "Describe a function/macro/keyboard macro."
  (interactive)
  (ghelp-describe-1
   'force-prompt '(:mode emacs-lisp-mode :category function)))

(defun ghelp-describe-variable ()
  "Describe a variable."
  (interactive)
  (ghelp-describe-1
   'force-prompt '(:mode emacs-lisp-mode :category variable)))

(defun ghelp-describe-key (key-sequence)
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
                                 :category function
                                 :kmacro ,def))
         ;; DEF is a symbol for a function.
         (ghelp-describe-1
          'no-prompt `(:symbol ,(symbol-name def) :mode emacs-lisp-mode
                               :category function
                               :marker ,(point-marker)))))
      (_ (user-error "%s is bound to %s which is not a command"
                     (key-description key-sequence)
                     def)))))

;;; History
;;
;; Functions:
;; - ‘ghelp-history--of’
;; - ‘ghelp-history--push’
;; - ‘ghelp-history--page-at’
;; - ‘ghelp-history--set-current-page’
;; - ‘ghelp-history--current-page’

;;;; Variables

(defvar ghelp-history-max-length 50
  "Maximum length of each history.")

(defvar ghelp-history-alist nil
  "A list of (major-mode . history).
HISTORY is the history of documentation queries.")

(cl-defstruct ghelp-history
  "History for a major mode.

 - nodes :: A list of ‘ghelp-history-node’.
 - current :: The last viewed node."
  nodes
  current)

(cl-defstruct ghelp-history-node
  "A node in a ‘ghelp-history’.

 - mode   :: Major mode.
 - symbol :: Symbol that the documentation describes.
 - buffer :: The ghelp buffer containing the documentation.
 - prev   :: Previous node.
 - next   :: next node."
  mode
  symbol
  buffer
  prev
  next)

(defun ghelp-history--set-nodes (nodes mode)
  "Set nodes of the history of MODE to NODES."
  (setf (ghelp-history-nodes (alist-get mode ghelp-history-alist))
        nodes))

(defun ghelp-history--set-current (node mode)
  "Set current node of the history of MODE to NODE."
  (setf (ghelp-history-current (alist-get mode ghelp-history-alist))
        node))

;;;; Private

(defun ghelp-history--symbol-node (symbol history)
  "Return the node describing SYMBOL in HISTORY or nil."
  ;; Remember that SYMBOL is string.
  (seq-find (lambda (node) (equal (ghelp-history-node-symbol node) symbol))
            (ghelp-history-nodes history)))

(defun ghelp-history--remove-node (node history)
  "Remove NODE from HISTORY and destroy NODE."
  (let ((buf (ghelp-history-node-buffer node))
        (prev (ghelp-history-node-prev node))
        (next (ghelp-history-node-next node)))
    (when (buffer-live-p (kill-buffer buf)))
    (when prev (setf (ghelp-history-node-next prev) next))
    (when next (setf (ghelp-history-node-prev next) prev)))
  (setf (ghelp-history-nodes history)
        (remq node (ghelp-history-nodes history)))
  ;; We are deleting the current node.
  (when (eq node (ghelp-history-current history))
    (setf (ghelp-history-current history)
          (or (ghelp-history-node-prev node)
              (ghelp-history-node-next node)
              (car (ghelp-history-nodes history))))))

(defun ghelp-history--trim (history)
  "Remove old nodes from HISTORY if it’s too long.
HiSTORY is too long when its length exceeds
‘ghelp-history-max-length’."
  (let ((nodes (ghelp-history-nodes history)))
    (when (> (length nodes) ghelp-history-max-length)
      (dolist (node (seq-subseq nodes ghelp-history-max-length))
        (ghelp-history--remove-node node history)))))

(defun ghelp-history--insert-after (node1 node2 history)
  "Insert NODE1 after NODE2 in HISTORY."
  (push node1 (ghelp-history-nodes history))
  (let ((after-node2 (ghelp-history-node-next node2)))
    (setf (ghelp-history-node-next node2) node1)
    (setf (ghelp-history-node-next node1) after-node2)
    (setf (ghelp-history-node-prev node1) node2)
    (when after-node2
      (setf (ghelp-history-node-prev after-node2) node1))))

(defun ghelp-history--insert-before (node1 node2 history)
  "Insert NODE1 before NODE2 in HISTORY."
  (push node1 (ghelp-history-nodes history))
  (let ((before-node2 (ghelp-history-node-prev node2)))
    (setf (ghelp-history-node-prev node2) node1)
    (setf (ghelp-history-node-prev node1) before-node2)
    (setf (ghelp-history-node-next node1) node2)
    (when before-node2
      (setf (ghelp-history-node-next before-node2) node1))))

(defun ghelp-history--bring-node-to-front (node history)
  "Make NODE the latest node in HISTORY."
  (let ((nodes (ghelp-history-nodes history)))
    (setf (ghelp-history-nodes history)
          (cons node (remq node nodes)))))

;;;; Public

(defun ghelp-history--of (mode)
  "Return history of MODE."
  (or (alist-get mode ghelp-history-alist)
      ;; TODO remove workaround
      ;; needed for all version before 27.1
      (let ((h (make-ghelp-history :nodes nil :current nil)))
        (setf (alist-get mode ghelp-history-alist) h)
        h)))

(defun ghelp-history--push (page symbol mode)
  "Push PAGE for SYMBOL of MODE to the history of MODE."
  (let* ((node (make-ghelp-history-node :buffer page
                                        :symbol symbol
                                        :mode mode))
         (history (ghelp-history--of mode))
         (current (ghelp-history-current history)))
    (if current (ghelp-history--insert-after node current history)
      (setf (ghelp-history-nodes history) (list node)))
    (setf (ghelp-history-current history) node)))

(defun ghelp-history--page-at (where symbol mode)
  "Return the page at POS in MODE.
POS is WHERE SYMBOL
POS can be
    :at SYMBOL      meaning return the page for SYMBOL, or
    :after SYMBOL   meaning return the page after the one for
                    SYMBOL, or
    :before SYMBOL  meaning return the page before the one for
                    SYMBOL.

Return nil if didn’t find the page, or the page is killed."
  (when-let* ((history (ghelp-history--of mode))
              (node (ghelp-history--symbol-node symbol history))
              (real-node (pcase where
                           (:at node)
                           (:after (ghelp-history-node-next node))
                           (:before (ghelp-history-node-prev node))))
              (buffer (ghelp-history-node-buffer real-node)))
    (if (buffer-live-p buffer)
        buffer
      ;; If user tries to go to prev/next page but that page is
      ;; killed, we fix the history behind the scene.
      (ghelp-history--remove-node real-node history)
      nil)))

(defun ghelp-history--set-current-page (where symbol mode)
  "Set the current page of MODE to the page describing SYMBOL.
Specifically, set the current page of the history of MODE.
WHERE is the same as in ‘ghelp-history--page-at’.
If such page doesn’t exist, do nothing and return nil."
  (when-let* ((history (ghelp-history--of mode))
              (node (ghelp-history--symbol-node symbol history))
              (real-node (pcase where
                           (:at node)
                           (:after (ghelp-history-node-next node))
                           (:before (ghelp-history-node-prev node)))))
    (setf (ghelp-history-current history) real-node)))

(defun ghelp-history--current-page (mode)
  "Return the current page for MODE.
If can’t find one, return nil."
  (when-let* ((history (ghelp-history--of mode))
              (node (ghelp-history-current history))
              (page (ghelp-history-node-buffer node)))
    ;; We fix the error behind the scene.
    (if (buffer-live-p page)
        page
      (message "Last viewed page is killed, showing the second last.")
      (ghelp-history--remove-node node history)
      (ghelp-history--current-page mode))))

(defun ghelp-history--symbols (mode)
  "Return a list of symbols (string) that the history for MODE contains."
  (when-let* ((history (ghelp-history--of mode))
              (nodes (ghelp-history-nodes history)))
    (mapcar #'ghelp-history-node-symbol nodes)))

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
;; - ‘ghelp-page-data’
;; - ‘ghelp-get-page-data’

;;;;; Modes

(defvar ghelp-page-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "t" #'ghelp-toggle-entry)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    (define-key map "q" #'ghelp-close)
    (define-key map "b" #'ghelp-back)
    (define-key map "f" #'ghelp-forward)
    (define-key map "d" #'ghelp-next-entry)
    (define-key map "u" #'ghelp-previous-entry)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "m" #'forward-button)
    (define-key map "[" #'backward-button)
    (define-key map (kbd "SPC") #'scroll-up-command)
    (define-key map (kbd "<backspace>") #'scroll-down-command)
    (define-key map "g" #'ghelp-refresh)
    (define-key map "s" #'ghelp-switch-to-page)
    (define-key map "?" #'ghelp-page-show-help)
    map))

(define-derived-mode ghelp-page-mode fundamental-mode
  "Ghelp" "Major mode for ghelp pages."
  (setq buffer-read-only t))

;;;;; Variables

(defvar ghelp-enable-header-line t
  "Whether to display information on the header line.
Changeling this variable doesn’t affect existing ghelp pages.")

(defface ghelp-entry (let ((display t)
                           (atts '(:inherit nil)))
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

(defface ghelp-header-button (let ((display t)
                                   (atts '(:slant normal :weight normal
                                                  :inherit
                                                  info-header-node)))
                               `((,display . ,atts)))
  "Face for back and forward button in header line."
  :group 'ghelp)

(defvar-local ghelp-page-data nil
  "A plist that stores information about the documentation.
The plist includes these values:

    :symbol    A string; the documentation is about this symbol.
    :mode      The major mode; it is used by ‘ghelp--show-page’.
    :marker    The marker at the point where the user requested
               documentation of this symbol.

NOTE: Backends should not use this variable, instead, use
‘ghelp-get-page-data’.")

(defun ghelp-get-page-data ()
  "Return a plist that’s identical to ‘ghelp-page-data’.
The plist contains useful information like symbol and marker."
  (if (derived-mode-p 'ghelp-page-mode)
      (copy-tree ghelp-page-data)
    (error "Not in a ghelp page")))

;; (defvar ghelp-entry-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "TAB") #'ghelp-toggle-entry)
;;     map)
;;   "Keymap activated when point is in an entry.")

(defun ghelp-page--header-line-format ()
  "Return back and forward button for the current page."
  (let ((symbol (plist-get ghelp-page-data :symbol)))
    (concat (propertize "  " 'display '(space :width (10)))
            ;; [back]
            (propertize
             (ghelp--make-button "<<back" #'ghelp-back)
             'face 'ghelp-header-button
             'mouse-face 'highlight)
            (propertize "  " 'display '(space :width (20)))
            symbol
            (propertize "  " 'display '(space :width (20)))
            ;; [forward]
            (propertize
             (ghelp--make-button "forward>>" #'ghelp-forward)
             'face 'ghelp-header-button
             'mouse-face 'highlight))))

;;;;; Commands

(defun ghelp-back ()
  "Go back one page."
  (interactive)
  (let* ((symbol (plist-get ghelp-page-data :symbol))
         (mode (plist-get ghelp-page-data :mode))
         (page (ghelp-history--page-at :before symbol mode)))
    (when page
      (ghelp-history--set-current-page :before symbol mode)
      (switch-to-buffer page))))

(defun ghelp-forward ()
  "Go forward one page."
  (interactive)
  (let* ((symbol (plist-get ghelp-page-data :symbol))
         (mode (plist-get ghelp-page-data :mode))
         (page (ghelp-history--page-at :after symbol mode)))
    (when page
      (ghelp-history--set-current-page :after symbol mode)
      (switch-to-buffer page))))

(defun ghelp-switch-to-page ()
  "Switch to a page in history."
  (interactive)
  (let* ((mode (plist-get ghelp-page-data :mode))
         (symbol (completing-read
                  "Switch to: "
                  (ghelp-history--symbols mode)
                  nil t))
         (page (ghelp-history--page-at :at symbol mode)))
    (switch-to-buffer page)))

(defun ghelp-page-show-help ()
  "Show help for available commands in `ghelp-page-mode'."
  (interactive)
  (message "q       quit            t       toggle entry
g       refresh         s       switch to page

TAB     next button     S-TAB   previous button
f       next page       b       previous page
d       next entry      u       previous entry

m       next button     [       previous button
SPC     scroll down     DEL     scroll up
"))

;;;;; Functions

(defun ghelp--generate-new-page (mode symbol)
  "Generate a new page for MODE (major mode) and SYMBOL and return it."
  (with-current-buffer (generate-new-buffer
                        (ghelp--page-name-from mode symbol))
    (ghelp-page-mode)
    (plist-put ghelp-page-data :symbol symbol)
    (plist-put ghelp-page-data :mode mode)
    (when ghelp-enable-header-line
      (setq header-line-format
            '((:eval (ghelp-page--header-line-format)))))
    (current-buffer)))

(defun ghelp-get-page-or-create (mode symbol)
  "Return the page for MODE (major mode) and SYMBOL.
Assume a history is available for MODE, else error.
POINT is the point of the symbol."
  (let* ((page (ghelp-history--page-at :at symbol mode)))
    (if page
        (ghelp-history--set-current-page :at symbol mode)
      (setq page (ghelp--generate-new-page mode symbol))
      (ghelp-history--push page symbol mode))
    page))

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
  "Insert entries in ENTRY-LIST one-by-one.
For FOLD, see ‘ghelp-page-insert-entry’."
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
  "Return the buffer name used by ghelp page for MODE and SYMBOL."
  (format " *ghelp %s : %s*" mode symbol))

;; Helpers

(defun ghelp--make-button (text fn)
  "Return a clickable TEXT that invokes FN when clicked by mouse-1."
  (propertize text 'keymap (let ((map (make-sparse-keymap)))
                             (define-key map [header-line mouse-1] fn)
                             (define-key map [mode-line mouse-1] fn)
                             (define-key map [mouse-1] fn)
                             map)))

(defun ghelp-page--history ()
  "Return non-nil ‘ghelp-page-history’ or error."
  (ghelp-history--of (plist-get ghelp-page-data :mode)))

(defun ghelp--show-page (entry-list data &optional window)
  "Display page with ENTRY-LIST in WINDOW (if non-nil).
DATA contains useful information like symbol and mode, see
‘ghelp-page-data’ for more."
  (let* ((mode (plist-get data :mode))
         (marker (plist-get data :marker))
         (symbol (plist-get data :symbol))
         (page (ghelp-get-page-or-create mode symbol)))
    (with-current-buffer page
      (ghelp-page-clear)
      (ghelp-page-insert-entry-list entry-list t)
      (goto-char
       (point-max))
      (ghelp-previous-entry)
      (ghelp-entry-unfold)
      (setq ghelp-page-data (plist-put ghelp-page-data :symbol symbol))
      (setq ghelp-page-data (plist-put ghelp-page-data :marker marker))
      (setq ghelp-page-data (plist-put ghelp-page-data :mode mode)))
    (if window
        (window--display-buffer page window 'window)
      (setq window (display-buffer page)))
    (when help-window-select
      (select-window window))))

(defun ghelp--maybe-update-current-page ()
  "Update current page of history.
If user opened a page with ‘swtich-to-buffer’, we have no way to
know and can’t update the current page to it. To make sure the
current page of our history is almost always up to date, call
this function. This function looks at visible ghelp pages and set
them to current in their history."
  (dolist (window (window-list nil 'never))
    (with-current-buffer (window-buffer window)
      (when (derived-mode-p 'ghelp-page-mode)
        (let* ((symbol (plist-get ghelp-page-data :symbol))
               (mode (plist-get ghelp-page-data :mode)))
          (ghelp-history--set-current-page :at symbol mode))))))

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

(defun ghelp-dummy-backend (command data)
  "Demo. Prompt behavior depends on PROMPT.

If COMMAND is 'symbol, return a string representing the symbol
that the user wants documentation for. DATA is a plist of form

    (:marker MARKER)

where MARKER is the marker at the point where user invoked
‘ghelp-describe’.

If COMMAND is 'doc, return the documentation for SYMBOL, where
SYMBOL is from DATA:

    (:symbol SYMBOL :marker MARKER)

Returned documentation is a string ending with a newline.
Return nil if no documentation is found."
  (pcase command
    ('symbol (completing-read "Symbol: "
                              '("woome" "veemo" "love" "tank")))
    ('doc (pcase (plist-get data :symbol)
            ("woome" "Woome!!\n")
            ("veemo" "Veemo!!\n")
            ("love" "Peace!!\n")
            ("tank" "TANK! THE! BEST!\n")))))

(defun ghelp-dummy ()
  "Demonstrate the dummy backend."
  (interactive)
  (ghelp-describe-as-in 'dummy-mode))

(ghelp-register-backend 'dummy-mode #'ghelp-dummy-backend)

;;; Setup

(require 'ghelp-builtin)
(ghelp-register-backend 'emacs-lisp-mode #'ghelp-help-backend)

(with-eval-after-load 'helpful
  (require 'ghelp-helpful)
  (ghelp-register-backend 'emacs-lisp-mode #'ghelp-helpful-backend))

(with-eval-after-load 'eglot
  (require 'ghelp-eglot)
  (ghelp-register-backend ghelp-eglot-supported-modes
                      #'ghelp-eglot-backend))

(with-eval-after-load 'geiser
  (require 'ghelp-geiser)
  (ghelp-register-backend 'scheme-mode #'ghelp-geiser-backend)
  (ghelp-register-backend 'geiser-repl-mode #'ghelp-geiser-backend))


(provide 'ghelp)

;;; ghelp.el ends here
