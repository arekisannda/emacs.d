;;; packages-treemacs.el --- Treemacs Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package treemacs :ensure t)

(use-package treemacs-nerd-icons :ensure t :after treemacs)

(use-package treemacs-evil :ensure t :after treemacs)

(use-package treemacs-persp :ensure t :after (treemacs persp-mode))

(use-package treemacs-magit :ensure t :after (treemacs magit))

(use-package lsp-treemacs :ensure t :after (treemacs lsp-mode))

(provide 'packages-treemacs)

;;; packages-treemacs.el ends here
