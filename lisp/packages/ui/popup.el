;;; ui/popup.el -*- lexical-binding: t; -*-

(require 'util-windows)

(use-package transient
  :custom
  (transient-show-popup t)
  (transient-display-buffer-action
   '(util/windows-display-buffer-in-pop-up-window
     (dedicated . t)))
  (transient-mode-line-format nil)
  (transient-force-fixed-pitch t))

(use-package posframe
  :custom
  (posframe-inhibit-double-buffering t)
  (posframe-mouse-banish-function #'posframe-mouse-banish-simple)
  :config
  (defun +posframe-show-refresh (buffer &rest _)
    (posframe-refresh buffer))
  (advice-add #'posframe-show :after #'+posframe-show-refresh))

(use-package which-key
  :custom
  (which-key-dont-use-unicode t)
  (which-key-popup-type 'custom)
  (which-key-sort-order 'which-key-description-order)
  (which-key-show-prefix 'echo)
  (which-key-side-window-slot 0)
  (which-key-side-window-location 'bottom)
  (which-key-max-display-columns nil)
  (which-key-side-window-max-width 0)
  (which-key-min-column-description-width 30)
  (which-key-custom-hide-popup-function
   (lambda ()
     (when (buffer-live-p which-key--buffer)
       ;; in case which-key buffer was shown in an existing window, `quit-window'
       ;; will re-show the previous buffer, instead of closing the window
       (quit-windows-on which-key--buffer)
       (when (and which-key-preserve-window-configuration
                  which-key--saved-window-configuration)
         (set-window-configuration which-key--saved-window-configuration)
         (setq which-key--saved-window-configuration nil)))))

  (which-key-custom-show-popup-function #'+which-key--show-buffer-root-window)

  (which-key-custom-popup-max-dimensions-function
   (lambda (&optional _)
     (cons 30 (let ((edges (window-edges (frame-root-window))))
                (- (nth 2 edges) (nth 0 edges)))
           )))
  :config
  (defun +which-key--show-buffer-root-window (&optional _)
    (when (and which-key-preserve-window-configuration
               (not which-key--saved-window-configuration))
      (setq which-key--saved-window-configuration (current-window-configuration)))
    (let* ((alist `((window-width  . #'util/window-popup-fit-window-to-buffer)
                    (window-height . #'util/window-popup-fit-window-to-buffer)
                    (window-popup  . bottom)
                    (dedicated . t))))
      (cond
       ((eq which-key--multiple-locations t)
        (delete-windows-on which-key--buffer)
        (let ((w (split-window (frame-root-window nil) nil nil)))
          (window--display-buffer which-key--buffer w 'window alist)
          (util/window-popup-fit-window-to-buffer w)))
       ((get-buffer-window which-key--buffer)
        (display-buffer-reuse-window which-key--buffer alist))
       (t
        (let ((w (split-window (frame-root-window nil) nil nil)))
          (window--display-buffer which-key--buffer w 'window alist)
          (util/window-popup-fit-window-to-buffer w)))
       )))
  :hook
  (after-init . which-key-mode))
