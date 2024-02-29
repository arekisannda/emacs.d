;;; packages-evil.el --- Emacs Evil-mode Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package evil :after undo-fu
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-default-state 'normal)
  (evil-want-keybinding nil)
  (evil-undo-system 'undo-fu)
  (evil-want-minibuffer nil)
  :hook
  (elpaca-after-init . evil-mode))

(use-package evil-collection :after evil
  :custom
  (evil-collection-mode-list '(dashboard
                               info
                               dired
                               ibuffer
                               magit
                               edebug
                               org
                               org-roam
                               ediff))
  :diminish evil-collection-unimpaired-mode
  :hook
  (evil-mode . evil-collection-init))

(use-package evil-nerd-commenter :after evil)

(use-package evil-args :after evil)

(use-package evil-snipe :after evil
  :custom
  (evil-snipe-enable-highlight t)
  :hook
  (evil-mode . evil-snipe-override-mode))

(use-package evil-easymotion :after evil)

(use-package evil-matchit :after evil
  :hook
  (evil-mode . global-evil-matchit-mode))

(use-package evil-lion :after evil
  :custom
  (evil-lion-squeeze-spaces t)
  (evil-lion-left-align-key nil)
  (evil-lion-right-align-key nil)
  :hook
  (evil-mode . evil-lion-mode))

(use-package evil-mc :after evil
  :init
  (setq evil-mc-cursors-map (make-sparse-keymap)
        evil-mc-key-map (make-sparse-keymap))
  :hook
  (evil-mode . global-evil-mc-mode))

(use-package embrace
  :init
  (setq embrace-show-help-p t))

(provide 'packages-evil)

;;; packages-evil.el ends here
