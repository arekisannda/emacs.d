;;; packages/evil --- summary:
;;; Emacs evil-mode packages and configurations
;;; commentary:

;;; code:

(use-package evil
  :after general
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-default-state 'normal
	evil-want-C-u-jump nil
	evil-want-C-u-scroll t
	evil-want-keybinding nil
	evil-undo-system 'undo-fu
	evil-want-minibuffer nil)
  :config
  (general-evil-setup t)
  (evil-mode 1))

(use-package undo-fu
  :ensure t
  :after diminish)

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-mode-list
	'(dashboard
	  info
	  dired
	  ibuffer
	  magit
	  ediff))
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :ensure t)

(use-package evil-args
  :ensure t)

(use-package evil-snipe
  :after evil
  :init
  (setq evil-snipe-enable-highlight t)
  :config
  (evil-snipe-override-mode +1))

(use-package evil-easymotion
  :after evil)

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-lion
  :ensure t
  :init
  (setq evil-lion-squeeze-spaces t
	evil-lion-left-align-key nil
	evil-lion-right-align-key nil)
  :config
  (evil-lion-mode 1))

(provide 'packages-evil)
;;; evil.el ends here
