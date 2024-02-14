;;; packages-keybindings.el --- Keybinding Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package general :ensure t)

(use-package hydra
  :ensure t
  :init (setq-default hydra-key-doc-function nil))

(provide 'packages-keybindings)

;;; packages-keybindings.el ends here
