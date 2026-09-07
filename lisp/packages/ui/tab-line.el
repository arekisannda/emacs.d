;;; tab-line.el -*- lexical-binding: t; -*-

(require 'util-strings)

(use-package tab-line
  :custom
  (tab-line-new-button-show nil)
  (tab-line-tab-name-format-function #'tab-line-tab-name-format-padded)
  (tab-line-tab-name-truncated-max 15)
  :config
  (utils/custom-set-faces
   (tab-line-tab-modified
    ((nil :inherit tab-line-tab :background unspecified)))
   (tab-line
    ((nil :box (:line-width (1 . 4) :style flat-button))))
   (tab-line-tab
    ((nil :height 1.0 :overline ,(doom-color 'fg-alt) :foreground ,(doom-color 'fg-alt))))
   (tab-line-tab-inactive
    ((nil :height 1.0 :background ,(doom-color 'bg-alt))))
   (tab-line-tab-inactive-alternate
    ((nil :inherit tab-line-tab-inactive)))
   (tab-line-tab-current
    ((nil :height 1.0 :overline ,(doom-color 'violet) :foreground ,(doom-color 'fg) :background ,(doom-color 'bg))))
   (tab-line-tab-special
    ((nil :slant italic)))
   (tab-line-tab-modified
    ((nil :underline unspecified :foreground ,(doom-color 'yellow))))
   )

  (defun tab-line-tab-name-format-padded (tab tabs)
    "Default function to use as `tab-line-tab-name-format-function', which see."
    (let* ((buffer-p (bufferp tab))
           (selected-p (if buffer-p
                           (eq tab (window-buffer))
                         (cdr (assq 'selected tab))))
           (name (util/strings-pad-or-truncate
                  (concat " " (if buffer-p
                                  (funcall tab-line-tab-name-function tab tabs)
                                (cdr (assq 'name tab)))) 16))
           (face (if selected-p
                     (if (mode-line-window-selected-p)
                         'tab-line-tab-current
                       'tab-line-tab)
                   'tab-line-tab-inactive)))
      (dolist (fn tab-line-tab-face-functions)
        (setf face (funcall fn tab tabs face buffer-p selected-p)))
      (apply 'propertize
             (concat (propertize (string-replace "%" "%%" name) ;; (bug#57848)
                                 'face face
                                 'keymap tab-line-tab-map
                                 'help-echo (if selected-p "Current tab"
                                              "Click to select tab")
                                 ;; Don't turn mouse-1 into mouse-2 (bug#49247)
                                 'follow-link 'ignore)
                     (let ((close (or (and (or buffer-p (assq 'buffer tab)
                                               (assq 'close tab))
                                           tab-line-close-button-show
                                           (not (eq tab-line-close-button-show
                                                    (if selected-p 'non-selected
                                                      'selected)))
                                           tab-line-close-button)
                                      "")))
                       (setq close (copy-sequence close))
                       ;; Don't overwrite the icon face
                       (add-face-text-property 0 (length close) face t close)
                       close))
             `(
               tab ,tab
               ,@(if selected-p '(selected t))
               mouse-face tab-line-highlight))))

  (defcustom tab-line-confirm-kill-window t
    "Enable `tab-line' kill window confirmation."
    :type 'boolean)

  (defun tab-line-new-state ()
    (let ((buffer (get-buffer-create "*new*")))
      (set-buffer-major-mode buffer)
      (set-window-buffer nil buffer)
      (tab-line-close-other-tabs)
      ))

  (defun tab-line-close-tab-kill-window (&optional arg)
    (interactive "p")
    (let* ((window (selected-window)))
      (if (> (length (tab-line-tabs-window-buffers)) 1)
          (bury-buffer)
        (pcase arg
          (4 (tab-line-new-state))
          (_ (when (or (not tab-line-confirm-kill-window) (yes-or-no-p "Kill window?"))
               (delete-window window)
               (ignore-errors (balance-windows))))
          ))
      (force-mode-line-update)))

  (defun tab-line-kill-tab-kill-window (&optional arg)
    (interactive "p")
    (let* ((window (selected-window)))
      (if (> (length (tab-line-tabs-window-buffers)) 1)
          (kill-current-buffer)
        (pcase arg
          (4 (let ((buffer (current-buffer)))
               (tab-line-new-state)
               (kill-buffer buffer)))
          (_ (when (or (not tab-line-confirm-kill-window) (yes-or-no-p "Kill window?"))
               (kill-buffer-and-window)
               (ignore-errors (balance-windows))))
          ))
      (force-mode-line-update)))

  (defun tab-line-close-other-tabs (&optional window)
    (interactive)
    (let* ((window (window-normalize-window window t)))
      (set-window-prev-buffers window nil)
      (set-window-next-buffers window nil)
      (force-mode-line-update)
      (if (called-interactively-p)
          (message "Cleared other tabs."))))

  (defmacro tab-line-window-for-each (frame &rest body)
    (declare (indent 1) (debug (form body)))
    `(let ((frame ,frame))

       (unless (or (minibufferp)
                   (frame-parent frame)
                   (one-window-p 'nomini frame))
         (walk-windows
          (lambda (window) (with-selected-window window ,@body))
          'nomini)
         )))

  (defun tab-line-main-window-setup (&optional frame)
    (tab-line-window-for-each frame
      (let ((wframe (window-frame)))
        (if (or (window-parameter window 'window-side)
                (window-parameter window 'window-popup)
                )
            (tab-line-mode -1)
          (tab-line-mode 1))
        )))

  (defun tab-line-main-window-teardown (&optional frame)
    (tab-line-window-for-each frame
      (let ((wframe (window-frame)))
        (unless (or (window-parameter window 'window-side)
                    (window-parameter window 'window-popup)
                    )
          (tab-line-mode -1))
        )))

  (define-minor-mode tab-line-main-window-mode
    "Toggle display of tab line in main window only."
    :global t
    :lighter nil
    (when tab-line-popup-window-mode (tab-line-popup-window-mode -1))
    (if tab-line-main-window-mode
        (progn
          (add-hook 'window-configuration-change-hook #'tab-line-main-window-setup)
          (add-hook 'window-buffer-change-functions #'tab-line-main-window-setup)
          (tab-line-main-window-setup)
          (force-mode-line-update t))
      (remove-hook 'window-configuration-change-hook #'tab-line-main-window-setup)
      (remove-hook 'window-buffer-change-functions #'tab-line-main-window-setup)
      (tab-line-main-window-teardown)
      (force-mode-line-update t)))

  (defun tab-line-popup-window-setup (&optional frame)
    (tab-line-window-for-each frame
      (let ((wframe (window-frame)))
        (tab-line-mode (if (window-parameter window 'window-popup) 1 -1))
        )))

  (defun tab-line-popup-window-teardown (&optional frame)
    (tab-line-window-for-each frame
      (tab-line-mode -1)))

  (define-minor-mode tab-line-popup-window-mode
    "Toggle display of tab line in popup window only."
    :global t
    :lighter nil
    (when tab-line-main-window-mode (tab-line-main-window-mode -1))
    (if tab-line-popup-window-mode
        (progn
          (add-hook 'window-configuration-change-hook #'tab-line-popup-window-setup)
          (add-hook 'window-buffer-change-functions #'tab-line-popup-window-setup)
          (tab-line-popup-window-setup)
          (force-mode-line-update t))
      (remove-hook 'window-configuration-change-hook #'tab-line-popup-window-setup)
      (remove-hook 'window-buffer-change-functions #'tab-line-popup-window-setup)
      (tab-line-popup-window-teardown)
      (force-mode-line-update t)))

  :hook
  (window-setup . tab-line-popup-window-mode))
