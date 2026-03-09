;;; ui/modeline.el -*- lexical-binding: t; -*-

(use-package doom-modeline
  :custom-face
  (mode-line
   ((nil :background ,(doom-color 'bg))))
  (mode-line-active
   ((nil :background ,(doom-color 'bg))))
  (mode-line-inactive
   ((nil :background ,(doom-color 'bg))))
  (doom-modeline-bar
   ((nil :foreground ,(doom-color 'bg-alt)
         :background ,(doom-color 'bg-alt))))
  (doom-modeline-bar-inactive
   ((nil :foreground ,(doom-color 'bg-alt)
         :background ,(doom-color 'bg-alt))))
  (doom-modeline-evil-insert-state
   ((nil :weight bold
         :foreground ,(doom-color 'bg-alt)
         :background ,(doom-color 'green))))
  (doom-modeline-evil-normal-state
   ((nil :weight bold
         :foreground ,(doom-color 'bg-alt)
         :background ,(doom-color 'blue))))
  (doom-modeline-evil-visual-state
   ((nil :weight bold
         :foreground ,(doom-color 'bg-alt)
         :background ,(doom-color 'yellow))))
  (doom-modeline-evil-replace-state
   ((nil :weight bold
         :foreground ,(doom-color 'bg-alt)
         :background ,(doom-color 'red))))
  (doom-modeline-evil-motion-state
   ((nil :weight bold
         :foreground ,(doom-color 'bg-alt)
         :background ,(doom-color 'magenta))))
  (doom-modeline-evil-operator-state
   ((nil :weight bold
         :foreground ,(doom-color 'bg-alt)
         :background ,(doom-color 'orange))))
  (doom-modeline-evil-emacs-state
   ((nil :weight bold
         :foreground ,(doom-color 'bg-alt)
         :background ,(doom-color 'violet))))
  (doom-modeline-evil-user-state
   ((nil :weight bold
         :foreground ,(doom-color 'bg-alt)
         :background ,(doom-color 'dark-blue))))
  :custom
  (doom-modeline-bar-width 2)
  (doom-modeline-height 15)
  (doom-modeline-buffer-file-name-style 'truncate-upto-root)
  (doom-modeline-buffer-file-state-icon nil)
  (doom-modeline-buffer-modification-icon nil)
  (doom-modeline-window-width-limit nil)
  (doom-modeline-env-version nil)
  (mode-line-right-align-edge 'right-fringe)
  :config
  (doom-modeline-def-segment evil
    "Display evil mode states."
    (when (bound-and-true-p evil-mode)
      (let ((face (cond
                   ((evil-normal-state-p) 'doom-modeline-evil-normal-state)
                   ((evil-emacs-state-p) 'doom-modeline-evil-emacs-state)
                   ((evil-insert-state-p) 'doom-modeline-evil-insert-state)
                   ((evil-motion-state-p) 'doom-modeline-evil-motion-state)
                   ((evil-operator-state-p) 'doom-modeline-evil-operator-state)
                   ((evil-replace-state-p) 'doom-modeline-evil-replace-state)
                   ((evil-visual-state-p) 'doom-modeline-evil-visual-state)
                   (t 'doom-modeline-evil-user-state))))
        (propertize
         (propertize " " 'display `(space :width 1))
         'face (doom-modeline-face face)
         'help-echo (evil-state-property evil-state :name t)))))

  (doom-modeline-def-segment buffer-info-extra
    `(" "
      mode-line-mule-info
      mode-line-modified
      mode-line-client
      mode-line-remote))

  (doom-modeline-def-segment space
    (propertize " " 'display `(space :width 1)))

  (doom-modeline-def-segment minibuffer-depth
    (let* ((depth (minibuffer-depth)))
      (when (> depth 0)
        (format "[%d]" depth))))

  (doom-modeline-def-segment dedicated
    (if (window-dedicated-p)
        (propertize
         " "
         'face (doom-modeline-face 'warning))
      "  "))

  (doom-modeline-def-segment purpose
    (if-let* ((purpose (window-parameter (selected-window) 'window-purpose)))
        (propertize
         (format "[%s]" (symbol-name purpose))
         'face (doom-modeline-face 'success))))

  (doom-modeline-def-modeline
    '+default-modeline
    '(evil buffer-info-extra buffer-info dedicated remote-host purpose)
    '(misc-info minibuffer-depth selection-info lsp repl check buffer-position))

  (defun +doom-modeline-set ()
    (doom-modeline-set-modeline '+default-modeline 'default))

  (setq doom-modeline-mode-alist nil)

  (defun +mode-line-setup (window buffer &optional flags)
    (when (member 'disable-mode-line flags)
      (with-current-buffer buffer
        (setq-local mode-line-format nil)
        )))

  :hook
  (util/windows-side-window . +mode-line-setup)
  (util/windows-pop-up-window . +mode-line-setup)
  (doom-modeline-mode . +doom-modeline-set)
  (doom-modeline-mode . column-number-mode)
  (after-init . doom-modeline-mode))
