;;; util-windows.el --- Emacs windows utility functions -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defun util/window-popup-fit-window-to-buffer (window &rest _)
  "Configured  `fit-to-window-buffer' for popup WINDOW."
  (fit-window-to-buffer window 20 1))

(defun util/windows-display-buffer-in-side-window (buffer &optional alist plist)
  "Display BUFFER in side window according to ALIST and PLIST."
  (if (plist-get plist :ignore) 'fail
    (let* ((side (plist-get plist :side))
           (slot (plist-get plist :slot))
           (size (plist-get plist :size))
           (fixed (plist-get plist :fixed))
           (init-window (window-normalize-window nil))
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
               `(,@alist
                 (direction             . ,(plist-get plist :direction))
                 (side                  . ,side)
                 (slot                  . ,slot)
                 (inhibit-same-window   . t)
                 (window-height         . ,(and (cl-find side '(bottom top)) size))
                 (window-width          . ,(and (cl-find side '(right left)) size))
                 )
               )))
       (t (user-error "Unable to create side window")))

      (when (plist-get plist :disable-modeline)
        (set-window-parameter window 'mode-line-format 'none))
      (unless (window-parameter window 'quit-restore)
        (set-window-parameter window 'quit-restore `(window window ,init-window ,buffer)))
      (when (plist-get plist :no-other)
        (set-window-parameter window 'no-other-window t))
      (set-window-buffer window buffer)
      (set-window-dedicated-p window (plist-get plist :dedicated))
      (set-window-parameter window 'no-other-window t)
      (window-preserve-size window (not (eq fixed 'height)) t)

      (with-current-buffer buffer
        (setq-local window-size-fixed fixed))

      (if (plist-get plist :select) window init-window))))

(defun util/windows-display-buffer-in-mru-main-window (buffer &optional alist plist)
  "Display BUFFER in most recently used window according to ALIST and PLIST."
  (if (plist-get plist :ignore) 'fail
    (let* ((init-window (window-normalize-window nil))
           window)
      (cond
       ;; if reuse flag is set and if buffer is visible, reuse the window
       ((and (setq window (get-buffer-window buffer)) (plist-get plist :reuse))
        (select-window window))
       ((setq window (windex-get-mru-in-main))
        (set-window-buffer window buffer))
       (t (user-error "Unable to get main window")))

      (if (plist-get plist :select) window init-window))))

(defun util/windows-display-buffer-in-lru-main-window (buffer &optional alist plist)
  "Display BUFFER in least recently used window according to ALIST and PLIST."
  (if (plist-get plist :ignore) 'fail
    (let* ((init-window (window-normalize-window nil))
           window)
      (cond
       ;; if reuse flag is set and if buffer is visible, reuse the window
       ((and (setq window (get-buffer-window buffer)) (plist-get plist :reuse))
        (select-window window))
       ((setq window (util/window-get-lru-in-main))
        (set-window-buffer window buffer))
       (t (user-error "Unable to get main window")))

      (if (plist-get plist :select) window init-window))))

(defun +dynamic-display-buffer--match-action (buffer-or-name action-list)
  "Return action from ACTION-LIST for BUFFER-OR-NAME."
  (cl-loop for (condition . plist) in action-list
           when (shackle--match buffer-or-name condition plist)
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

If the inititial window is not a side window, display BUFFER using `:fallback`"
  (if (plist-get plist :ignore) 'fail
    (let* ((init-window (window-normalize-window nil))
           window
           rule-plist)
      (unless (setq rule-plist (+dynamic-display-buffer--match-action
                                (window-buffer init-window)
                                (plist-get plist :dynamic)))
        (setq rule-plist (plist-get plist :fallback)))

      (cond
       ((plist-get rule-plist :same)
        (setq window (display-buffer-same-window buffer alist)))
       ((and (plist-get rule-plist :reuse) (setq window (get-buffer-window buffer)))
        (setq window (display-buffer-reuse-window buffer alist)))
       ((plist-get rule-plist :mru)
        (setq window (util/windows-display-buffer-in-mru-main-window buffer alist rule-plist)))
       ((plist-get rule-plist :lru)
        (setq window (util/windows-display-buffer-in-lru-main-window buffer alist rule-plist)))
       ((setq action (plist-get rule-plist :action))
        (setq window (funcall (plist-get rule-plist :action) buffer alist rule-plist)))
       (t 'fail))
      window)))

(defun +display-buffer-in-pop-up-window (buffer &optional alist plist)
  (let ((frame (shackle--splittable-frame)))
    (when frame
      (if (plist-get plist :ignore) 'fail
        (let* ((init-window (window-normalize-window nil))
               (alist `(,@alist
                        (window-popup          . bottom)
                        (no-other-window       . t)
                        (dedicated             . t)
                        (window-preserved-size . t)
                        ))
               parameters
               window)
          (with-current-buffer buffer
            (if (get-buffer-window buffer)
                (display-buffer-reuse-window buffer alist)
              (let* ((lines (count-lines (point-min) (point-max)))
                     (window (split-window (frame-root-window frame) (min -20 (max -20 (- lines))))))
                (window--display-buffer buffer window 'window alist)
                (set-window-parameter window 'no-other-window t)
                (window-preserve-size window nil t)
                (if (plist-get plist :select) window init-window)))
            ))
        ))
    ))


(provide 'util-windows)

;;; util-windows.el ends here
