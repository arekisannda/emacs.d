;;; lib-window-extra.el --- Additional Windows Commands -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
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

(defun +windows-set-purpose (&optional arg)
  "Set/unset window purpose.

With prefix ARG \\[universal-argument], remove window purpose.
With double-prefix ARG \\[universal-argument], echo window purpose."
  (interactive "p")
  (pcase arg
    (4 (util/window--unset-purpose))
    (16 (message "window-purpose: %s" (util/window-get-purpose)))
    (_ (util/window--set-purpose))))

(defun +window-with-purpose (&optional purpose noselect)
  "Select window with `window-purpose` value PURPOSE.

If NOSELECT is non-nil, just return window."
  (interactive)
  (let* ((purpose-list (mapcar #'symbol-name +window-purpose-list))
         (prompt (format "Select purpose: "))
         (purpose (or purpose (s-trim (completing-read prompt purpose-list (-const t) t)))))
    (if-let ((window (window-with-parameter 'window-purpose
                                            (cond ((stringp purpose) (intern purpose))
                                                  ((symbolp purpose) purpose)))))
        (if noselect window (select-window window)))))

(defun +toggle-bottom-side-windows (&optional frame)
  "Toggle bottom side window in FRAME."
  (interactive)
  (let* ((frame (window-normalize-frame frame))
         (window--sides-inhibit-check t)
         state)
    (cond
     ((window-with-parameter 'window-side 'bottom frame)
      ;; At least one side window exists.  Remove all side windows after
      ;; saving FRAME's state in its `window-state' parameter.
      (set-frame-parameter
       frame 'window-state (window-state-get (frame-root-window frame)))
      (let ((ignore-window-parameters t))
        (walk-windows
         (lambda (win)
           (when (equal (window-parameter win 'window-side) 'bottom)
             (delete-window win))))))
     ((setq state (frame-parameter nil 'window-state))
      ;; A window state was saved for FRAME.  Restore it and put the
      ;; current root window into its main window.
      (walk-windows
       (lambda (win)
         (when (or (equal (window-parameter win 'window-side) 'left)
                   (equal (window-parameter win 'window-side) 'right)
                   (equal (window-parameter win 'window-side) 'above))
           (delete-window win))))

      (let ((window-combination-resize t)
            (main-state (window-state-get (frame-root-window nil))))
        (window-state-put state (frame-root-window nil) t)
        (window-state-put main-state (window-main-window nil)))
      (window--sides-reverse-frame nil)
      (select-window (util/window-with-parameters '((window-side . bottom)
                                                    (window-slot . 0)))))
     (t
      (error "No side windows state found")))))

(defun +display-buffer-in-side-window (buffer &optional alist plist) ;
  "Select popup window for BUFFER.
ALIST is an association list of action symbols and values.
PLIST is custom shackle parameters list."
  (let* ((side (plist-get plist :align))
         (slot (plist-get plist :slot))
         (height (plist-get plist :height))
         (width (plist-get plist :width))
         (caller-window (selected-window))
         (parameters `((window-side . ,side) (window-slot . ,slot)))
         window)

    (when (plist-get plist :disable-modeline)
      (set-window-parameter window 'mode-line-format))

    (with-current-buffer buffer
      (if-let ((fixed (plist-get plist :fixed)))
          (setq-local window-size-fixed fixed)))

    (cond
     ((setq window (util/window-with-parameters parameters nil))
      (set-window-buffer window buffer))
     (t
      (setq window
            (display-buffer-in-side-window
             buffer
             (append alist
                     `((dedicated                . ,(plist-get plist :dedicated))
                       (direction                . ,(plist-get plist :direction))
                       (side                     . ,side)
                       (slot                     . ,slot))
                     (if height `((window-height . ,height)))
                     (if width `((window-width   . ,width)))))))
     )

    (if (not window)
        (error "Unable to create side-window")
      (set-window-parameter window 'no-delete-other-windows t)

      (if (not (plist-get plist :noselect))
          (select-window window)
        (select-window caller-window)))))

(defun +display-buffer-in-purposed-window (buffer &optional alist plist)
  "Display BUFFER in window with specified purpose.
ALIST is an association list of action symbols and values.
PLIST is custom shackle parameters list."
  (unless (and (plist-get plist :ignore))
    (let* ((purpose-list (plist-get plist :purpose))
           (caller-window (selected-window)))
      (cond
       ((equal (window-parameter caller-window 'window-side) 'bottom)
        (cond
         ((setq window (get-buffer-window buffer))
          (select-window window))

         ((setq window (get-buffer-window buffer))
          (select-window window))

         ((setq window (cl-some (lambda (purpose) (+window-with-purpose purpose t)) purpose-list))
          (set-window-buffer window buffer)
          (select-window window))

         (t (user-error "No window matching items in purpose list"))))

       ((plist-get plist :same)
        (display-buffer-same-window buffer alist))
       ))))

(defun +wm-display-in-mru-main-window (buffer &optional alist plist)
  "Select the most recently used main window for BUFFER.
ALIST is an association list of action symbols and values.
PLIST is custom shackle parameters list."
  )

(provide 'lib-window-extras)

;;; lib-window-extra.el ends here
