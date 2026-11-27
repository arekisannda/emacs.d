;;; util-windows.el --- Emacs windows utility functions -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defcustom util/windows-max-width 120
  "Windowx max width in characters."
  :type 'integer)

(defcustom util/windows-min-right-width 110
  "Right side window min width in characters."
  :type 'integer)

(defcustom util/windows-min-left-width 40
  "Left side window min width in characters."
  :type 'integer)

(defcustom util/windows-min-bottom-height 20
  "Bottom side window min height in lines."
  :type 'integer)

(defcustom util/windows-max-bottom-height 30
  "Bottom side window min height in lines."
  :type 'integer)

(defcustom util/windows-disable-shrink nil
  "If non-nil, disable shrinking of windows.")

(defcustom util/windows-side-window-hook '()
  "Called when creating side window.
Function takes two arguments WINDOW and buffer and optional FLAGS."
  :type 'hook)

(defcustom util/windows-popup-window-hook '()
  "Called when creating popup window.
Function takes two arguments WINDOW and BUFFER."
  :type 'hook)

(defcustom util/windows-aux-window-hook '()
  "Called when creating window.
Function takes two arguments WINDOW and buffer and optional FLAGS."
  :type 'hook)

(defvar util/windows-temporary-buffer-name " *split-sentinel*")

(defun utils/window-display-buffer (buffer window type alist)
  (if (version< emacs-version "27")
      (window--display-buffer buffer window type alist
                              display-buffer-mark-dedicated)
    (window--display-buffer buffer window type alist)))

(advice-add 'shrink-window-if-larger-than-buffer
            :before-while (lambda (&rest args) util/windows-disable-shrink))

(defun util/windows-get-mru-in-main (&optional all-frames dedicated not-selected no-other)
  "Get most recently used main window."
  (let (best-window best-time time)
    (dolist (window (window-list-1 nil 'nomini all-frames))
      (setq time (window-use-time window))
      (when (and (or dedicated (not (window-dedicated-p window)))
                 (or (not not-selected) (not (eq window (selected-window))))
                 (or (not no-other) (not (window-parameter window 'no-other-window)))
                 (or (not best-time) (> time best-time))
                 (not (util/windows-side-window-p window))
                 (not (util/windows-popup-window-p window))
                 (not (util/windows-aux-window-p window)))
        (setq best-time time)
        (setq best-window window)))
    best-window))

(defun util/windows-popup-fit-window-to-buffer (window &rest _)
  "Configured  `fit-to-window-buffer' for popup WINDOW."
  (fit-window-to-buffer window 20 1))

(defcustom util/windows-display-buffer-by-condition-switch-function #'ignore
  "Switch function for `util/windows-display-buffer-by-condition'."
  :type 'function
  :group 'window
  :group 'convenience)

(defun util/windows-display-buffer-by-condition (buffer &optional alist plist)
  "DISPLAY BUFFER according to ALIST, PLIST, and the inititial window.

If the inititial window is a side window, display BUFFER using the rules
defined in `:conditions`.  `:conditions` is a list of
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

If the inititial window is not a side window, display BUFFER using `:fallback`"
  (if (plist-get plist :ignore) (user-error "Buffer ignored by rule.")
    (let* ((init-window (window-normalize-window nil))
           window
           rule-plist)
      (unless (setq rule-plist (funcall util/windows-display-buffer-by-condition-switch-function
                                        (window-buffer init-window)
                                        (plist-get plist :conditions)))
        (setq rule-plist (plist-get plist :fallback)))

      (cond
       ((and (plist-get rule-plist :reuse) (setq window (get-buffer-window buffer)))
        (setq window (display-buffer-reuse-window buffer alist)))
       ((plist-get rule-plist :same)
        (setq window (display-buffer-same-window buffer alist)))
       ((plist-get rule-plist :mru)
        (setq window (util/windows-display-buffer-in-mru-main-window buffer alist rule-plist)))
       ((plist-get rule-plist :lru)
        (setq window (util/windows-display-buffer-in-lru-main-window buffer alist rule-plist)))
       ((setq action (plist-get rule-plist :action))
        (setq window (funcall (plist-get rule-plist :action) buffer alist rule-plist)))
       (t 'fail))
      window)))

(defun util/windows-display-buffer-in-mru-main-window (buffer &optional alist plist)
  "Display BUFFER in most recently used window according to ALIST and PLIST."
  (if (plist-get plist :ignore) (user-error "Buffer ignored by rule.")
    (let* ((init-window (window-normalize-window nil))
           window)
      (cond
       ;; if reuse flag is set and if buffer is visible, reuse the window
       ((and (setq window (get-buffer-window buffer)) (plist-get plist :reuse))
        (select-window window))
       ((setq window (util/windows-get-mru-in-main))
        (set-window-buffer window buffer))
       (t (user-error "Unable to get main window")))

      window)))

(defun util/windows-display-buffer-in-lru-main-window (buffer &optional alist plist)
  "Display BUFFER in least recently used window according to ALIST and PLIST."
  (if (plist-get plist :ignore) (user-error "Buffer ignored by rule.")
    (let* ((init-window (window-normalize-window nil))
           window)
      (cond
       ;; if reuse flag is set and if buffer is visible, reuse the window
       ((and (setq window (get-buffer-window buffer)) (plist-get plist :reuse))
        (select-window window))
       ((setq window (util/window-get-lru-in-main))
        (set-window-buffer window buffer))
       (t (user-error "Unable to get main window")))

      window)))

(defun util/windows-display-buffer-in-side-window (buffer &optional alist plist)
  "Display BUFFER in side window according to ALIST and PLIST."
  (if (plist-get plist :ignore) (user-error "Buffer ignored by rule.")
    (let* ((side (plist-get plist :side))
           (slot (plist-get plist :slot))
           (size (plist-get plist :size))
           (fixed (plist-get plist :fixed))
           (window-combination-limit t)
           (init-window (window-normalize-window nil))
           parameters
           window)

      (if (and side slot)
          (setq parameters `((window-side . ,side) (window-slot . ,slot)))
        (user-error "Missing side window parameters"))

      (cond
       ((setq window
              (display-buffer-in-side-window
               buffer
               `(,@alist
                 (side . ,side)
                 (slot . ,slot)
                 (inhibit-same-window . t)
                 ,@(if (and size (functionp size)) (funcall size side)
                     (list (cons 'window-height (and (cl-find side '(bottom top)) size))
                           (cons 'window-width  (and (cl-find side '(right left)) size))))
                 )
               )))
       (t (user-error "Unable to create side window")))

      (with-current-buffer buffer
        (when (plist-get plist :disable-modeline)
          (set-window-parameter window 'mode-line-format 'none))
        (unless (window-parameter window 'quit-restore)
          (set-window-parameter window 'quit-restore `(window window ,init-window ,buffer)))
        (when (plist-get plist :no-other)
          (set-window-parameter window 'no-other-window t))
        (set-window-buffer window buffer)
        (set-window-dedicated-p window (plist-get plist :dedicated))
        (set-window-parameter window 'no-other-window t)
        (set-window-parameter window 'no-delete-other-windows t)

        (when fixed
          (cond
           ((eq fixed 'height)
            (window-preserve-size window nil t))
           ((eq fixed 'width)
            (window-preserve-size window t t))
           )
          (setq-local window-size-fixed fixed)
          )
        )

      (run-hook-with-args 'util/windows-side-window-hook window buffer (plist-get plist :flags))
      window
      )))

(defun util/windows--state-list-stash (frame state-fn)
  "STATE-FN takes 1 argument WINDOW and return a list of (WINDOW-TYPE SLOT WINDOW-STATE)."
  (with-selected-frame frame
    (let (window-states
          windows-to-delete)
      (walk-windows
       (lambda
         (window)
         (when-let* ((state (funcall state-fn window)))
           (add-to-list 'window-states state)
           (add-to-list 'windows-to-delete window)))
       'nomini)

      (dolist (w windows-to-delete)
        (delete-window w))

      window-states)))

(defun util/windows--state-list-restore (frame window-states)
  (with-selected-frame frame
    (dolist (s window-states)
      (pcase-let ((`(,side ,slot ,state) s))
        (let ((sentinel (get-buffer-create util/windows-temporary-buffer-name))
              window)

          (pcase side
            ('popup
             (setq window (util/windows-display-buffer-in-popup-window sentinel nil nil)))
            ((or 'left 'right 'bottom 'top)
             (setq window (display-buffer-in-side-window sentinel `((side . ,side)
                                                                    (slot . ,slot))))
             ))

          (window-state-put state window t)))
      )))

(defun util/windows-split-main-window-below (size frame)
  (with-selected-frame frame
    (let ((size (min util/windows-max-bottom-height
                     (max util/windows-min-bottom-height
                          (if (integerp size) size (floor (* size (frame-height frame))))
                          )))
          (sentinel (get-buffer-create util/windows-temporary-buffer-name))
          (toggle (and (window-with-parameter 'window-side nil frame)))
          (state-fn
           (lambda (window)
             (when (util/windows-side-window-p window)
               (list
                (window-parameter window 'window-side)
                (window-parameter window 'window-slot)
                (window-state-get window t)
                ))))
          side-states
          root-window
          window
          (window-combination-limit t))
      (and toggle
           (setq side-states (util/windows--state-list-stash frame state-fn))
           )
      (setq root-window (frame-root-window frame))
      (setq window (split-window-below (- size) root-window))
      (set-window-buffer window sentinel)
      (set-window-combination-limit (window-parent window) t)
      (and toggle
           (util/windows--state-list-restore frame side-states))
      ;; (balance-windows root-window)
      (get-buffer-window sentinel frame))
    ))

(defun util/windows-select-popup-window ()
  (interactive)
  (when-let* ((window (window-with-parameter 'window-popup 'bottom (selected-frame))))
    (select-window window)))

(defun util/windows-display-buffer-in-popup-window (buffer &optional alist plist)
  (let ((frame (shackle--splittable-frame)))
    (when frame
      (if (plist-get plist :ignore) (user-error "Buffer ignored by rule.")
        (let* ((init-window (window-normalize-window nil))
               (popup-window (window-with-parameter 'window-popup 'bottom frame))
               (size (or (plist-get plist :size) util/windows-min-bottom-height))
               (fixed (plist-get plist :fixed))
               (alist `(,@alist
                        (window-popup        . bottom)
                        (no-other-window     . t)
                        (inhibit-same-window . t)
                        ))
               parameters
               window)

          (cond
           ((window-live-p popup-window)
            (setq window popup-window)
            (utils/window-display-buffer buffer window 'reuse alist))
           (t
            (setq window (util/windows-split-main-window-below size frame))
            (utils/window-display-buffer buffer window 'window alist)))

          (with-current-buffer buffer
            (unless (bound-and-true-p util/windows--popup-configured)
              (setq-local util/windows--popup-configured t))

            (when (plist-get plist :disable-modeline)
              (set-window-parameter window 'mode-line-format 'none))

            (set-window-parameter window 'quit-restore `(window window ,init-window ,buffer))
            (set-window-dedicated-p window (plist-get plist :dedicated))
            (set-window-parameter window 'no-other-window t)
            (set-window-parameter window 'no-delete-other-windows t)
            (set-window-parameter window 'split-window (lambda (&rest _) (error "Cannot split popup window")))
            (set-window-parameter window 'window-popup 'bottom)

            (window-preserve-size window nil t)
            (setq-local window-size-fixed fixed))

          (run-hook-with-args 'util/windows-popup-window-hook window buffer (plist-get plist :flags))
          window
          ))
      )))

(defcustom util/windows-sub-frame-hook '()
  "Called when displaying buffer in a sub frame.
Function takes two arguments WINDOW and BUFFER and optional FLAGS."
  :type 'hook)

(defun util/windows-sub-frame-p (&optional frame)
  "Return non-nil if FRAME is a sub frame created by util-windows."
  (let ((frame (window-normalize-frame frame)))
    (and (frame-parent frame)
         (eq frame (frame-parameter (frame-parent frame) 'util/windows-sub-frame)))))

(defun util/windows--sub-frame-size (size total)
  "Resolve SIZE against TOTAL columns/lines of the parent frame.
A nil SIZE uses TOTAL."
  (max 1 (min total
              (cond
               ((integerp size) size)
               ((floatp size) (floor (* size total)))
               ((functionp size) (funcall size total))
               (t total)))))

(defun util/windows--sub-frame-poshandler-info (child parent-window)
  "Build a `posframe'-compatible poshandler info plist for CHILD."
  (let* ((parent (window-frame parent-window))
         (point (window-point parent-window))
         (mouse (mouse-pixel-position)))
    (list :posframe child
          :posframe-buffer (window-buffer (frame-root-window child))
          :posframe-width (frame-pixel-width child)
          :posframe-height (frame-pixel-height child)
          :parent-frame parent
          :parent-frame-width (frame-pixel-width parent)
          :parent-frame-height (frame-pixel-height parent)
          :parent-window parent-window
          :parent-window-top (window-pixel-top parent-window)
          :parent-window-left (window-pixel-left parent-window)
          :parent-window-width (window-pixel-width parent-window)
          :parent-window-height (window-pixel-height parent-window)
          :parent-window-start (window-start parent-window)
          :parent-window-end (window-end parent-window)
          :position point
          :position-info (posn-at-point point parent-window)
          :font-width (frame-char-width parent)
          :font-height (frame-char-height parent)
          :mode-line-height (window-mode-line-height parent-window)
          :header-line-height (window-header-line-height parent-window)
          :tab-line-height (window-tab-line-height parent-window)
          :minibuffer-height (window-pixel-height (minibuffer-window parent))
          :mouse-x (cadr mouse)
          :mouse-y (cddr mouse)
          :x-pixel-offset 0
          :y-pixel-offset 0)))

(defun util/windows--sub-frame-center (info)
  "Default poshandler: center sub frame in parent frame using INFO."
  (cons (max 0 (/ (- (plist-get info :parent-frame-width)
                     (plist-get info :posframe-width))
                  2))
        (max 0 (/ (- (plist-get info :parent-frame-height)
                     (plist-get info :posframe-height))
                  2))))

(defun util/windows--sub-frame-position (child parent-window position)
  "Move CHILD according to POSITION relative to PARENT-WINDOW."
  (let* ((info (util/windows--sub-frame-poshandler-info child parent-window))
         (pos (pcase position
                (`(,(pred integerp) . ,(pred integerp)) position)
                ((pred functionp) (funcall position info))
                (_ (util/windows--sub-frame-center info)))))
    (set-frame-position child (car pos) (cdr pos))))

(defun util/windows--sub-frame-set-border-color (frame color)
  "Set FRAME border COLOR.
COLOR is a color string or a face whose foreground is used."
  (when-let* ((color (if (facep color)
                         (face-foreground color nil 'default)
                       color)))
    (set-face-background 'child-frame-border color frame)
    (set-face-background 'internal-border color frame)))

(defun util/windows--make-sub-frame (parent width height)
  "Create a WIDTH x HEIGHT sub frame for PARENT."
  (let ((frame (make-frame `((parent-frame             . ,parent)
                             (minibuffer               . ,(minibuffer-window parent))
                             (width                    . ,width)
                             (height                   . ,height)
                             (no-accept-focus          . nil)
                             (no-focus-on-map          . nil)
                             (no-other-frame           . t)
                             (undecorated              . t)
                             (unsplittable             . t)
                             (skip-taskbar             . t)
                             (keep-ratio               . t)
                             (desktop-dont-save        . t)
                             (auto-hide-function       . delete-frame)
                             (child-frame-border-width . 1)
                             (internal-border-width    . 1)
                             (left-fringe              . 8)
                             (right-fringe             . 8)
                             (menu-bar-lines           . 0)
                             (tool-bar-lines           . 0)
                             (tab-bar-lines            . 0)
                             (vertical-scroll-bars     . nil)
                             (horizontal-scroll-bars   . nil)))))
    (set-frame-parameter parent 'util/windows-sub-frame frame)
    frame))

(defun util/windows-display-buffer-in-sub-frame (buffer &optional alist plist)
  "Display BUFFER in a child frame according to ALIST and PLIST."
  (if (plist-get plist :ignore) (user-error "Buffer ignored by rule.")
    (let* ((init-window (window-normalize-window nil))
           (parent (window-frame init-window))
           (return-window init-window)
           width
           height
           child
           window
           new)

      ;; Displaying from inside a sub frame: reuse it, keep original return window.
      (when (util/windows-sub-frame-p parent)
        (setq return-window (frame-parameter parent 'util/windows-sub-frame-return)
              parent (frame-parent parent)))

      (setq width (util/windows--sub-frame-size (plist-get plist :width) (frame-width parent))
            height (util/windows--sub-frame-size (plist-get plist :height) (frame-height parent)))

      (setq child (frame-parameter parent 'util/windows-sub-frame))
      (unless (frame-live-p child)
        (setq child (util/windows--make-sub-frame parent width height)
              new t))
      (setq window (frame-root-window child))

      ;; Window is strongly dedicated; release it so the new buffer can be shown.
      (set-window-dedicated-p window nil)
      (utils/window-display-buffer buffer window (if new 'frame 'reuse) alist)

      (when-let* ((parameters (plist-get plist :parameters)))
        (modify-frame-parameters child parameters))

      (set-frame-size child width height)

      (util/windows--sub-frame-set-border-color child (plist-get plist :border-color))

      (util/windows--sub-frame-position
       child
       (if (window-live-p return-window) return-window (frame-selected-window parent))
       (plist-get plist :position))

      (with-current-buffer buffer
        (when (plist-get plist :disable-modeline)
          (set-window-parameter window 'mode-line-format 'none))
        (set-window-parameter window 'quit-restore `(frame frame ,return-window ,buffer))
        (set-window-parameter window 'split-window (lambda (&rest _) (error "Cannot split sub frame window")))
        (set-window-dedicated-p window t)
        (set-window-prev-buffers window nil)
        (set-window-next-buffers window nil))

      (set-frame-parameter child 'util/windows-sub-frame-return return-window)
      (set-frame-parameter parent 'util/windows-sub-frame-buffer buffer)
      (select-frame-set-input-focus child)
      (select-window window)

      (run-hook-with-args 'util/windows-sub-frame-hook window buffer (plist-get plist :flags))
      window)))

(defun util/windows--sub-frame-restore-focus (frame)
  "Return focus to the originating window when sub FRAME is deleted."
  (when-let* (((util/windows-sub-frame-p frame))
              ((eq frame (selected-frame)))
              (window (frame-parameter frame 'util/windows-sub-frame-return))
              ((window-live-p window)))
    (select-frame-set-input-focus (window-frame window))
    (select-window window)))

(add-hook 'delete-frame-functions #'util/windows--sub-frame-restore-focus)

(defun util/windows-sub-frame-parent-window (&optional window)
  "Return WINDOW, or its parent frame's return window if WINDOW is in a sub frame.
Use this for UI that should be shown relative to the parent frame
\(e.g. posframes) while still acting on the sub frame's buffer."
  (let* ((window (window-normalize-window window))
         (frame (window-frame window)))
    (if (not (util/windows-sub-frame-p frame))
        window
      (let ((parent (frame-parent frame))
            (return (frame-parameter frame 'util/windows-sub-frame-return)))
        (if (and (window-live-p return) (eq (window-frame return) parent))
            return
          (frame-selected-window parent))))))

(defun util/windows--sub-frame-redirect (fn &rest args)
  "Call FN with ARGS from the parent frame when a sub frame is selected.
Keeps the sub frame restricted to its own buffer: the sub frame is
hidden and everything else is displayed through the parent frame's
window management."
  (let ((frame (selected-frame)))
    (when (and (util/windows-sub-frame-p frame)
               (not (minibuffer-window-active-p (selected-window))))
      (util/windows-hide-sub-frame frame))
    (apply fn args)))

(advice-add 'display-buffer :around #'util/windows--sub-frame-redirect)
(advice-add 'switch-to-buffer :around #'util/windows--sub-frame-redirect)

(defun util/windows--sub-frame-posframe-show (fn &rest args)
  "Call `posframe-show' FN with ARGS relative to the parent frame.
`posframe-show' parents the posframe to the selected window's frame;
when that is a sub frame, the posframe would be nested and clipped
inside it.  Selection is restored afterwards, so focus is unchanged."
  (let ((window (selected-window)))
    (if (util/windows-sub-frame-p (window-frame window))
        (with-selected-window (util/windows-sub-frame-parent-window window)
          (apply fn args))
      (apply fn args))))

(advice-add 'posframe-show :around #'util/windows--sub-frame-posframe-show)

(defun util/windows--sub-frame-parent (&optional frame)
  "Return the parent of FRAME if it is a sub frame, otherwise FRAME."
  (let ((frame (window-normalize-frame frame)))
    (if (util/windows-sub-frame-p frame) (frame-parent frame) frame)))

(defun util/windows--get-sub-frame (&optional frame)
  "Return the live sub frame associated with FRAME.
FRAME may be the sub frame itself or its parent."
  (when-let* ((child (frame-parameter (util/windows--sub-frame-parent frame)
                                      'util/windows-sub-frame))
              ((frame-live-p child)))
    child))

(defun util/windows-hide-sub-frame (&optional frame)
  "Delete the sub frame of FRAME.
Its buffer stays tracked; `util/windows-show-sub-frame' redisplays it.
Focus returns to the parent via `util/windows--sub-frame-restore-focus'."
  (interactive)
  (if-let* ((child (util/windows--get-sub-frame frame)))
      (delete-frame child)
    (user-error "No sub frame")))

(defun util/windows-show-sub-frame (&optional frame)
  "Redisplay the tracked sub frame buffer of FRAME.
The buffer goes through `display-buffer', so the shackle rule that
originally placed it recreates the sub frame."
  (interactive)
  (let ((buffer (frame-parameter (util/windows--sub-frame-parent frame)
                                 'util/windows-sub-frame-buffer)))
    (if (buffer-live-p buffer)
        (display-buffer buffer)
      (user-error "No sub frame buffer"))))

(defun util/windows-toggle-sub-frame ()
  "Delete the sub frame if it exists, otherwise redisplay its buffer."
  (interactive)
  (if (util/windows--get-sub-frame)
      (util/windows-hide-sub-frame)
    (util/windows-show-sub-frame)))

(defun util/windows-popup-window-p (window)
  (and (window-parameter window 'window-popup)))

(defun util/windows-side-window-p (window)
  (and (window-parameter window 'window-side)
       (window-parameter window 'window-slot)))

(defun util/windows-aux-window-p (window)
  (and (window-parameter window 'window-aux-other)
       (window-parameter window 'window-aux-id)
       (eq (window-parameter window 'window-aux) 'aux)))

(defun util/windows-aux-source-window-p (window)
  (and (window-parameter window 'window-aux-other)
       (window-parameter window 'window-aux-id)
       (eq (window-parameter window 'window-aux) 'source)))

(defun util/windows-get-aux-window (&optional window)
  (let ((window (window-normalize-window window)))
    (with-selected-window window
      (cond
       ((util/windows-aux-window-p window) window)
       ((util/windows-aux-source-window-p window)
        (window-with-parameter 'window-aux-id (window-parameter window 'window-aux-other)))
       ))
    ))

(defun util/windows-get-aux-other-window (&optional window)
  (let ((window (window-normalize-window window)))
    (with-selected-window window
      (when (or (util/windows-aux-window-p window)
                (util/windows-aux-source-window-p window))
        (window-with-parameter 'window-aux-id (window-parameter window 'window-aux-other))
        ))
    ))

(defun util/windows-kill-aux-window (&optional window)
  (interactive)
  (if-let* ((aux-window (util/windows-get-aux-window window)))
      (when (window-live-p aux-window)
        (delete-window aux-window))
    ))

(defun util/windows-display-buffer-in-aux-source-window (buffer &optional alist plist)
  (let ((init-window (window-normalize-window nil))
        parameters
        window)
    (if (not (util/windows-aux-window-p init-window))
        (user-error "Initial window is not an aux-window.")
      (setq window (window-with-parameter 'window-aux-id (window-parameter window 'window-aux-other)))
      (utils/window-display-buffer buffer window 'reuse alist))))

(defun util/windows--aux-uuid ()
  (let ((rnd (md5 (format "%s%s%s%s%s%s%s"
                          (random)
                          (time-convert nil)
                          (user-uid)
                          (emacs-pid)
                          (user-full-name)
                          user-mail-address
                          (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
            (substring rnd 0 8)
            (substring rnd 8 12)
            (substring rnd 13 16)
            (format "%x"
                    (logior
                     #b10000000
                     (logand
                      #b10111111
                      (string-to-number
                       (substring rnd 16 18) 16))))
            (substring rnd 18 20)
            (substring rnd 20 32))))

(defun util/windows-display-buffer-in-aux-window (buffer &optional alist plist)
  (if-let* ((init-window (window-normalize-window nil))
            (invalid (or (plist-get plist :ignore)
                         (util/windows-side-window-p init-window)
                         (util/windows-popup-window-p init-window)
                         (util/windows-sub-frame-p (window-frame init-window))
                         )))
      (user-error "Window cannot be split for aux-window.")
    (let ((size (plist-get plist :size))
          (fixed (plist-get plist :fixed))
          (alist `(,@alist
                   (no-other-window     . t)
                   (inhibit-same-window . t)
                   ))
          parameters
          window)

      (cond
       ((util/windows-aux-window-p init-window)
        ;; assumed live if init-window is aux window
        (setq window init-window)
        (utils/window-display-buffer buffer window 'window alist))

       ((and (util/windows-aux-source-window-p init-window)
             (setq window (util/windows-get-aux-window init-window))
             (window-live-p window))
        ;; aux window is live
        (utils/window-display-buffer buffer window 'window alist))

       (t
        ;; aux window is not live
        (setq size (max util/windows-min-bottom-height
                        (cond
                         ((functionp size) (funcall size 'bottom))
                         ((floatp size) (floor (* size (window-height init-window t))))
                         ((integerp size) size)
                         )))
        (setq window
              (display-buffer-below-selected
               buffer
               `(,@alist
                 (window-min-height . ,size)
                 (window-height     . ,size)
                 )))

        (let ((aux-source-id (or (window-parameter init-window 'window-aux-id)
                                 (util/windows--aux-uuid)))
              (aux-id (util/windows--aux-uuid)))
          (set-window-prev-buffers window nil)
          (set-window-parameter init-window 'window-aux 'source)
          (set-window-parameter init-window 'window-aux-id aux-source-id)
          (set-window-parameter init-window 'window-aux-other aux-id)
          (set-window-parameter window 'window-aux 'aux)
          (set-window-parameter window 'window-aux-id aux-id)
          (set-window-parameter window 'window-aux-other aux-source-id)
          )))

      (with-current-buffer buffer
        (unless (bound-and-true-p util/windows--aux-configured)
          (setq-local util/windows--aux-configured t))

        (setq-local window-size-fixed fixed)
        (window-preserve-size window nil t)

        (set-window-dedicated-p window t)
        (set-window-parameter window 'no-other-window t)
        (set-window-parameter window 'quit-restore `(window window ,init-window ,buffer))
        (set-window-parameter window 'split-window (lambda (&rest _) (error "Cannot split aux window")))
        )

      (run-hook-with-args 'util/windows-aux-window-hook window buffer (plist-get plist :flags))
      window)
    ))

(defun util/windows--aux-window-cleanup (&optional frame)
  (let (delete-occured)
    (with-selected-frame (window-normalize-frame frame)
      (walk-windows
       (lambda
         (window)
         (with-selected-window window
           (when (util/windows-aux-window-p window)
             (let* ((aux-id (window-parameter window 'window-aux-other))
                    (source-window (window-with-parameter 'window-aux-id aux-id)))
               (unless (window-live-p source-window)
                 (delete-window window)
                 (setq delete-occured t))
               ))
           ))
       'nomini)
      (when delete-occured (balance-windows (frame-root-window frame)))
      )))

(add-hook 'window-configuration-change-hook #'util/windows--aux-window-cleanup)

(defun util/windows--aux-note-buffer (init-window)
  (let* ((uuid (or (window-parameter init-window 'window-aux-id)
                   (util/windows--aux-uuid)))
         (bufname (format "*notes %s*" uuid)))
    (set-window-parameter init-window 'window-aux-id uuid)
    (get-buffer-create bufname)))

(defun util/windows-select-aux-window (&optional arg)
  (interactive "p")
  (let* ((init-window (window-normalize-window nil))
         (aux-splittable-p (and (not (or (util/windows-side-window-p init-window)
                                         (util/windows-popup-window-p init-window)
                                         (util/windows-sub-frame-p (window-frame init-window))
                                         (and (one-window-p)
                                              (eq (window-main-window) (window-parent init-window)))
                                         ))
                                )))
    (unless aux-splittable-p
      (user-error "Not an aux-capable window."))

    (if (util/windows-aux-window-p init-window)
        (setq init-window (util/windows-get-aux-other-window init-window)))

    (pcase arg
      (16 (with-current-buffer (util/windows--aux-note-buffer init-window)
            (org-mode)
            (display-buffer (current-buffer))))
      (4 (with-current-buffer (util/windows--aux-note-buffer init-window)
           (lisp-interaction-mode)
           (display-buffer (current-buffer))))
      (_ (when-let* ((window (util/windows-get-aux-other-window init-window)))
           (select-window window)))
      )
    ))

(defun util/windows-print-window-tree ()
  "Print the window tree of the current frame, including internal windows."
  (interactive)
  (util/windows--print-window-tree-walk (frame-root-window) 0))

(defun util/windows--print-window-tree-walk (win depth)
  "Recursively print WIN at DEPTH, descending into internal windows."
  (let ((indent (make-string (* depth 2) ?\s)))
    (princ (format "%s%s%s\n"
                   indent
                   win
                   (if (window-live-p win)
                       (format " [%s]" (buffer-name (window-buffer win)))
                     "")))
    ;; Descend into children (only internal windows have them).
    (let ((child (window-child win)))
      (while child
        (util/windows--print-window-tree-walk child (1+ depth))
        (setq child (window-next-sibling child))))))

(defun util/windows--quit-popup-window (orig-fn &optional kill window)
  (let* ((win (or window (selected-window)))
         (popup (window-parameter win 'window-popup)))
    (if (not popup)
        (funcall orig-fn kill window)
      ;; Remove current buffer from prev-buffers
      (set-window-prev-buffers
       win
       (seq-filter (lambda (entry)
                     (buffer-live-p (car entry)))
                   (window-prev-buffers win)))
      ;; If no live prev-buffers remain, force delete
      (if (null (window-prev-buffers win))
          (progn
            (when kill (kill-buffer (window-buffer win)))
            (when (window-live-p win) (delete-window win)))
        (funcall orig-fn kill window)))))

(provide 'util-windows)

;;; util-windows.el ends here
