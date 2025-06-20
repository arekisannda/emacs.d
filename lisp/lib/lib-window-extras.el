;;; lib-window-extras.el --- Additional Windows Commands -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'util-windows)

(defun +toggle-dedicated-window-buffer (&optional window)
  "Toggle window WINDOW's dedication to its current buffer on or off.
WINDOW defaults to the selected window."
  (interactive)
  (let* ((flag (not (window-dedicated-p window))))
    (set-window-dedicated-p window flag)
    (if flag
        (message "Window buffer is now dedicated")
      (message "Window buffer is not dedicated anymore"))
    (force-mode-line-update)
    flag))

(defun +window-set-purpose (&optional prefix)
  "Set/unset window purpose.

With prefix PREFIX \\[universal-argument], remove window purpose.
With double-prefix PREFIX \\[universal-argument], echo window purpose."
  (interactive "p")
  (pcase prefix
    (4 (util/window--unset-purpose))
    (16 (message "window-purpose: %s" (util/window-get-purpose)))
    (_ (util/window--set-purpose))))

(defun +window-with-purpose (&optional purpose noselect)
  "Select window with `window-purpose` value PURPOSE.

If NOSELECT is nil, select and return window."
  (interactive)
  (let* ((purpose-list (mapcar #'symbol-name +window-purpose-list))
         (prompt (format "Select purpose: "))
         (purpose (or purpose (s-trim (completing-read prompt purpose-list (-const t) t)))))
    (if-let ((window (window-with-parameter 'window-purpose
                                            (cond ((stringp purpose) (intern purpose))
                                                  ((symbolp purpose) purpose)))))
        (if noselect window (select-window window)))))


(defcustom +window-init-right-side-window-function  nil
  "Function used to set initial right side-window buffer if one does not exist."
  :type 'function
  :group 'window
  :group 'convenience)

(defcustom +window-init-bottom-side-window-function nil
  "Function used to set initial bottom side-window buffer if one does not exist."
  :type 'function
  :group 'window
  :group 'convenience)

(defun +window-select-right-side-window (slot &optional prefix)
  "Toggle right side-window.

With double-prefix PREFIX \\[universal-argument], delete right side windows."
  (let ((params `((window-side . right)
                  (window-slot . ,slot))))
    (pcase prefix
      (16 (util/window-with-parameters-delete params))
      (4 (util/window-with-parameters-delete '((window-side . right))))
      (_ (util/window-toggle-window params
                                    +window-init-right-side-window-function)))))

(defmacro +window-select-right-side-slot (slot)
  "Define a command for a specific right-side window SLOT."
  `(defun ,(intern (format "+window-select-right-side-window-slot-%s" slot)) (&optional prefix)
     (interactive "p")
     (+window-select-right-side-window ,slot prefix)))

(defun +window-select-bottom-side-window (&optional prefix)
  "Select bottom side-window.

With double-prefix PREFIX \\[universal-argument], delete bottom side-windows."
  (interactive "p")
  (let ((params '((window-popup . below))))
    (pcase prefix
      (4 (util/window-with-parameters-delete params))
      (_ (util/window-toggle-window params
                                    +window-init-bottom-side-window-function)))))

(defcustom +window-kill-non-main-windows-ignore-modes '(treemacs-mode
                                                        vterm-mode)
  "A list of modes to ignore when closing non-main windows."
  :type '(repeat symbol)
  :group 'window
  :group 'convenience)

(defun +window-kill-non-main-windows ()
  "Kill side windows and pop up windows."
  (interactive)
  (walk-windows
   (lambda (window)
     (if (or (window-parameter window 'window-side)
             (window-parameter window 'window-popup))
         (with-selected-window window
           (unless (derived-mode-p +window-kill-non-main-windows-ignore-modes)
             (delete-window window)))))
   'nomini
   nil))

(defun +display-popup-disable-split-window (&optional _window _size _side)
  "Disable `split-window` of popup window."
  (user-error "Cannot split popup window"))

(defun +display-buffer-in-popup-window (buffer &optional alist plist)
  "Display BUFFER in popup window according to ALIST and PLIST."
  (if (plist-get plist :ignore) 'fail
    (let* ((side (plist-get plist :side))
           (size (plist-get plist :size))
           (fixed (plist-get plist :fixed))
           (init-window (selected-window))
           state
           parameters
           window)

      (if side
          (setq parameters `((window-popup . ,side)))
        (user-error "Missing popup window parameters"))

      (cond
       ;; if reuse flag is set and if buffer is visible, reuse the window
       ((and (setq window (get-buffer-window buffer)) (plist-get plist :reuse))
        (select-window window))
       ((setq window (util/window-with-parameters parameters nil t))
        (setq state 'reuse-window))
       ((setq window (split-window (window-main-window) (if size (- size)) side nil))
        (setq state 'create-window))
       (t (user-error "Unable to create popup window")))

      (when (plist-get plist :disable-modeline)
        (set-window-parameter window 'mode-line-format 'none))

      (set-window-parameter window 'no-delete-other-windows t)
      (set-window-parameter window 'window-popup side)
      (set-window-parameter window 'split-window #'+display-popup-disable-split-window)
      (set-window-parameter window 'quit-restore `(window window ,(util/window-get-mru-in-main) ,buffer))
      (set-window-buffer window buffer)
      (set-window-dedicated-p window (plist-get plist :dedicated))

      (with-current-buffer buffer
        (setq-local window-size-fixed fixed))

      (if (plist-get plist :select) window init-window))))

(defun +display-buffer-in-side-window (buffer &optional alist plist)
  "Display BUFFER in side window according to ALIST and PLIST."
  (if (plist-get plist :ignore) 'fail
    (let* ((side (plist-get plist :side))
           (slot (plist-get plist :slot))
           (size (plist-get plist :size))
           (fixed (plist-get plist :fixed))
           (init-window (selected-window))
           parameters
           window)

      (if (and side slot)
          (setq parameters `((window-side . ,side) (window-slot . ,slot)))
        (user-error "Missing side window parameters"))

      (cond
       ;; if reuse flag is set and if buffer is visible, reuse the window
       ((and (setq window (get-buffer-window buffer)) (plist-get plist :reuse))
        (select-window window))
       ((setq window
              (display-buffer-in-side-window
               buffer
               (append alist
                       `((direction . ,(plist-get plist :direction))
                         (side      . ,side)
                         (slot      . ,slot)
                         (inhibit-same-window . t))
                       (cond
                        ((equal fixed 'height) `((window-height . ,size)))
                        ((equal fixed 'width) `((window-width . ,size))))))))
       (t (user-error "Unable to create side window")))

      (when (plist-get plist :disable-modeline)
        (set-window-parameter window 'mode-line-format 'none))
      (if (not (window-parameter window 'quit-restore))
          (set-window-parameter window 'quit-restore `(window window ,init-window ,buffer)))
      ;; (if (or (eq slot 0)
      ;;         (and (eq slot 1 ) (not (window-parameter window 'quit-restore))))
      ;;     (set-window-parameter window 'quit-restore `(window window ,init-window ,buffer)))
      (set-window-parameter window 'no-delete-other-windows t)
      (set-window-buffer window buffer)
      (set-window-dedicated-p window (plist-get plist :dedicated))

      (with-current-buffer buffer
        (setq-local window-size-fixed fixed))

      (if (plist-get plist :select) window init-window))))

(defun +display-buffer-in-purposed-window (buffer &optional alist plist)
  "Display BUFFER in window with specified purpose according to ALIST and PLIST."
  (if (plist-get plist :ignore) 'fail
    (let* ((purpose-list (plist-get plist :purpose))
           (init-window (selected-window))
           window)
      (cond
       ;; if reuse flag is set and if buffer is visible, reuse the window
       ((and (setq window (get-buffer-window buffer)) (plist-get plist :reuse))
        (select-window window))
       ((setq window (get-buffer-window buffer))
        (select-window window))
       ;; return first window with purpose matching an element from `purpose-list`
       ;; if `purpose-list` contains multiple matched elements, return window of the
       ;; matched element from the list.
       ((setq window (cl-some (lambda (purpose) (+window-with-purpose purpose)) purpose-list))
        (set-window-buffer window buffer))
       (t (user-error "Unable to find window with the requested purpose")))

      (if (plist-get plist :select) window init-window))))

(defun +display-buffer-in-mru-main-window (buffer &optional alist plist)
  "Display BUFFER in most recently used window according to ALIST and PLIST."
  (if (plist-get plist :ignore) 'fail
    (let* ((init-window (selected-window))
           window)
      (cond
       ;; if reuse flag is set and if buffer is visible, reuse the window
       ((and (setq window (get-buffer-window buffer)) (plist-get plist :reuse))
        (select-window window))
       ((setq window (util/window-get-mru-in-main))
        (set-window-buffer window buffer))
       (t (user-error "Unable to get main window")))

      (if (plist-get plist :select) window init-window))))

(defun +display-buffer-in-lru-main-window (buffer &optional alist plist)
  "Display BUFFER in least recently used window according to ALIST and PLIST."
  (if (plist-get plist :ignore) 'fail
    (let* ((init-window (selected-window))
           window)
      (cond
       ;; if reuse flag is set and if buffer is visible, reuse the window
       ((and (setq window (get-buffer-window buffer)) (plist-get plist :reuse))
        (select-window window))
       ((setq window (util/window-get-lru-in-main))
        (set-window-buffer window buffer))
       (t (user-error "Unable to get main window")))

      (if (plist-get plist :select) window init-window))))

(defun +window--action-match (buffer-or-name condition plist)
  "Action match function.
When BUFFER-OR-NAME matches CONDITION, PLIST is returned."
  (let* ((buffer (get-buffer buffer-or-name))
         (buffer-major-mode (buffer-local-value 'major-mode buffer))
         (buffer-name (buffer-name buffer))
         (condition-if (plist-get plist :if)))
    (when (or (not condition-if)
              (and condition-if (funcall condition-if)))
      (when (or (and (symbolp condition)
                     (provided-mode-derived-p buffer-major-mode condition))
                (and (stringp condition)
                     (or (string= condition buffer-name)
                         (and (plist-get plist :regexp)
                              (string-match condition buffer-name))))
                (and (consp condition)
                     (or (and (eq (car condition) :custom)
                              (funcall (cadr condition) buffer))
                         (cl-some (lambda (c)(+window--action-match buffer-or-name
                                                                    c
                                                                    plist))
                                  condition))))
        plist))))

(defun +dynamic-display-buffer--match-action (buffer-or-name action-list)
  "Return action from ACTION-LIST for BUFFER-OR-NAME."
  (cl-loop for (condition . plist) in action-list
           when (+window--action-match buffer-or-name condition plist)
           return plist
           finally return nil))

(defun +dynamic-display-buffer (buffer &optional alist plist)
  "DISPLAY BUFFER according to ALIST, PLIST, and the inititial window.

If the inititial window is a side window, display BUFFER using the rules
defined in `:dynamic`.  `:dynamic` is a list of
rules (CONDITION . ACTION-PLIST), and each condition can be a symbol or string.
A symbol is interpreted as a major-mode; a string, the buffer name or
a regular expression if `:regexp` is present in the action plist.

Additional ACTION-PLIST options:

:action and a function name or lambda:

Function with arguments BUFFER-OR-NAME, ALIST, and PLIST.

:mru and t:

Open BUFFER in the most recently used window

:lru and t:

Open BUFFER in the least recently used window

If the inititial window is not a side window, display BUFFER using `:static`"
  (if (plist-get plist :ignore) 'fail
    (let* ((init-window (selected-window))
           window
           rule-plist)
      (cond
       ((or (util/window-popup-p init-window)
            (util/window-side-p init-window))
        (unless (setq rule-plist (+dynamic-display-buffer--match-action
                                  (window-buffer init-window)
                                  (plist-get plist :dynamic)))
          (setq rule-plist (plist-get plist :static))))
       (t (setq rule-plist (plist-get plist :static))))

      (cond
       ((plist-get rule-plist :same)
        (setq window (display-buffer--maybe-same-window buffer alist)))
       ((plist-get rule-plist :mru)
        (setq window (+display-buffer-in-mru-main-window buffer alist rule-plist)))
       ((plist-get rule-plist :lru)
        (setq window (+display-buffer-in-lru-main-window buffer alist rule-plist)))
       ((setq action (plist-get rule-plist :action))
        (setq window (funcall (plist-get rule-plist :action) buffer alist rule-plist)))
       (t 'fail))
      window)))

(defun +window-select-mru-main-window ()
  "Select most recently used main window."
  (interactive)
  (select-window (util/window-get-mru-in-main)))

(defmacro +window-one-window-tab-bar-close-tab (&rest body)
  "Create custom close tab function with BODY."
  `(lambda ()
     (interactive)
     (if (one-window-p)
         (tab-bar-close-tab)
       (progn ,@body))))

(defmacro +window-split-focus-other-window (splitfn)
  "Focus other window after calling SPLITFN."
  `(lambda ()
     (interactive)
     (funcall #',splitfn)
     (other-window 1)))

(defmacro +window-make-frame-with-params (params &rest body)
  "Create new frames with PARAMS and run BODY."
  `(let ((frame (make-frame ,params)))
     (select-frame-set-input-focus frame)
     ,@body))

(defmacro +window-select-frame-with-params (params &rest body)
  "Select frames with PARAMS or create it then run BODY."
  `(let ((frame (cl-find-if
                 (lambda (f)
                   (seq-every-p
                    (lambda (p)
                      (eq (frame-parameter f (car p)) (cdr p)))
                    ,params))
                 (frame-list))))
     (if frame
         (progn
           (select-frame-set-input-focus frame)
           ,@body)
       (+window-make-frame-with-params ,params ,@body))))

(defun +windmove-display-in-direction (dir &optional arg)
  "Display the next buffer in the window at direction DIR.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Create a new window if there is no window in that direction.

By default, select the new window with a displayed buffer.
If `windmove-display-no-select' is `ignore', then allow the next command
to decide what window it selects.  With other non-nil values of
`windmove-display-no-select', this function reselects
a previously selected old window.

If prefix ARG is \\[universal-argument], reselect a previously selected old window.
If `windmove-display-no-select' is non-nil, the meaning of
the prefix argument is reversed and it selects the new window.

When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
  (let ((no-select (xor (consp arg) windmove-display-no-select)))
    (display-buffer-override-next-command
     (lambda (_buffer alist)
       (let* ((type 'window)
              (window (split-window nil nil dir)))
         (balance-windows)
         (cons window type)))
     (lambda (old-window new-window)
       (when (and (not (eq windmove-display-no-select 'ignore))
                  (window-live-p (if no-select old-window new-window)))
         (select-window (if no-select old-window new-window))))
     (format "[display-%s]" dir)
     )))

(defun +windmove-display-in-direction-around (fn dir &optional args)
  "`windmove-display-in-direction' around wrapper.
Use custom `+windmove-display-in-direction' for cardinal DIR;
default to original FN for all others."
  (cond
   ((or (eq dir 'left)
        (eq dir 'up)
        (eq dir 'down)
        (eq dir 'right))
    (apply #'+windmove-display-in-direction dir args))
   (t (apply fn dir args))))

(advice-add #'windmove-display-in-direction :around #'+windmove-display-in-direction-around)

(provide 'lib-window-extras)

;;; lib-window-extras.el ends here
