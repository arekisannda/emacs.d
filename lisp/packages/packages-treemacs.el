;;; packages-treemacs.el --- Treemacs Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package treemacs)

(use-package treemacs-nerd-icons :after treemacs)

(use-package treemacs-evil :after treemacs)

(use-package treemacs-persp :after (treemacs persp-mode))

(use-package treemacs-magit :after (treemacs magit))

(use-package lsp-treemacs :after (treemacs lsp-mode))

(use-package emacs :after (treemacs treemacs-nerd-icons)
  :ensure nil
  :config
  (require 'treemacs-persp)
  (treemacs-load-theme "nerd-icons")
  (treemacs-set-scope-type 'Perspectives))

(provide 'packages-treemacs)

;;; packages-treemacs.el ends here
