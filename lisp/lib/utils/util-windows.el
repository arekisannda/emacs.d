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
  "Bottom side window min width in lines."
  :type 'integer)

(defcustom util/windows-disable-shrink nil
  "If non-nil, disable shrinking of windows.")

(defcustom util/windows-side-window-hook '()
  "Called when creating side window.
Function takes two arguments WINDOW and buffer and optional FLAGS."
  :type 'hook)

(defcustom util/windows-pop-up-window-hook '()
  "Called when creating pop-up window.
Function takes two arguments WINDOW and BUFFER."
  :type 'hook)

(advice-add 'shrink-window-if-larger-than-buffer
            :before-while (lambda (&rest args) util/windows-disable-shrink))

(defun util/windows-popup-fit-window-to-buffer (window &rest _)
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
                 ,@(if (and size (functionp size))
                       (funcall size side)
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
          (window-preserve-size window (not (eq fixed 'height)) t)
          (setq-local window-size-fixed fixed)
          )
        )

      (run-hook-with-args 'util/windows-side-window-hook window buffer (plist-get plist :flags))
      (if (plist-get plist :select) window init-window)
      )))

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
  (if (plist-get plist :ignore) 'fail
    (let* ((init-window (window-normalize-window nil))
           window
           rule-plist)
      (unless (setq rule-plist (funcall util/windows-display-buffer-by-condition-switch-function
                                        (window-buffer init-window)
                                        (plist-get plist :conditions)))
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

(defun util/windows-display-buffer-in-pop-up-window (buffer &optional alist plist)
  (let ((frame (shackle--splittable-frame)))
    (when frame
      (if (plist-get plist :ignore) 'fail
        (let* ((init-window (window-normalize-window nil))
               (alist `((window-popup          . bottom)
                        (no-other-window       . t)
                        (dedicated             . t)
                        (window-preserved-size . t)
                        ,@alist))
               parameters
               window)

          (with-current-buffer buffer
            (face-remap-add-relative 'default `(nil :background ,(doom-color 'bg-alt)))
            (setq-local mode-line-format nil)
            (run-hook-with-args 'util/windows-pop-up-window-hook window buffer))

          (if (get-buffer-window buffer)
              (setq window (display-buffer-reuse-window buffer alist))
            (let* ((lines (count-lines (point-min) (point-max))))
              (setq window (split-window (frame-root-window frame) (min -20 (max -20 (- lines)))))
              (window--display-buffer buffer window 'window alist)
              (set-window-parameter window 'no-other-window t)
              (window-preserve-size window nil t)
              ))

          (set-window-parameter window 'window-popup 'bottom)
          (when (plist-get plist :select) window))
        ))
    ))

(provide 'util-windows)

;;; util-windows.el ends here
