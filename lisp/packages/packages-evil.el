;;; packages-evil.el --- Emacs Evil-mode Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package evil :after disable-mouse
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-default-state 'normal
        evil-want-keybinding nil
        evil-undo-system 'undo-fu
        evil-want-minibuffer nil)
  :config
  (evil-mode 1)
  (mapc #'disable-mouse-in-keymap
        (list evil-motion-state-map
              evil-normal-state-map
              evil-visual-state-map
              evil-insert-state-map))

  (dolist (mode '(vterm-mode
                  ranger-mode
                  elpaca-ui-mode
                  message-mode
                  special-mode
                  dap-ui-breakpoints-ui-list-mode
                  eglot-list-connections-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil-collection :after evil
  :init
  (setq evil-collection-mode-list
        '(dashboard
          info
          dired
          ibuffer
          magit
          edebug
          org
          org-roam
          ediff))
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter :after evil)

(use-package evil-args :after evil)

(use-package evil-snipe :after evil
  :init
  (setq evil-snipe-enable-highlight t)
  :config
  (evil-snipe-override-mode +1))

(use-package evil-easymotion :after evil)

(use-package evil-matchit :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-lion :after evil
  :init
  (setq evil-lion-squeeze-spaces t
        evil-lion-left-align-key nil
        evil-lion-right-align-key nil)
  :config
  (evil-lion-mode 1))

(use-package evil-mc :after evil
  :init
  (setq evil-mc-cursors-map (make-sparse-keymap)
        evil-mc-key-map (make-sparse-keymap))
  :config
  (global-evil-mc-mode t))

(use-package embrace
  :init
  (setq embrace-show-help-p t))

(provide 'packages-evil)

;;; packages-evil.el ends here
