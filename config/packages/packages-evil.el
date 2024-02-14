;;; packages-evil.el --- Emacs Evil-mode Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-default-state 'normal
        evil-want-keybinding nil
        evil-undo-system 'undo-fu
        evil-want-minibuffer nil))

(use-package evil-collection :ensure t :after evil)

(use-package evil-nerd-commenter :ensure t :after evil)

(use-package evil-args :ensure t :after evil)

(use-package evil-snipe
  :ensure t
  :after evil
  :init
  (setq evil-snipe-enable-highlight t))

(use-package evil-easymotion :ensure t :after evil)

(use-package evil-matchit :ensure t :after evil)

(use-package evil-lion
  :ensure t
  :after evil
  :init
  (setq evil-lion-squeeze-spaces t
	evil-lion-left-align-key nil
	evil-lion-right-align-key nil))

(use-package evil-mc
  :ensure t
  :after evil
  :init
  (setq evil-mc-cursors-map (make-sparse-keymap)
        evil-mc-key-map (make-sparse-keymap)))

(use-package embrace
  :ensure t
  :init
  (setq embrace-show-help-p t))

(provide 'packages-evil)

;;; packages-evil.el ends here
