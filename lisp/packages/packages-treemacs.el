;;; packages-treemacs.el --- Treemacs Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package treemacs)

(use-package treemacs-nerd-icons :after treemacs)

(use-package treemacs-evil :after treemacs)

(use-package treemacs-persp :disabled :after (treemacs persp-mode) :defer)

(use-package treemacs-perspective :after (treemacs perspective) :defer)

(use-package treemacs-magit :after (treemacs magit))

(use-package emacs :after (treemacs treemacs-nerd-icons)
  :ensure nil
  :hook
  (window-setup . (lambda ()
                    (require 'treemacs-themes)
                    (require 'treemacs-scope)
                    (cond ((featurep 'persp-mode) (require 'treemacs-persp))
                          ((featurep 'perspective) (require 'treemacs-perspective)))
                    (treemacs-load-theme "nerd-icons")
                    (treemacs-set-scope-type 'Perspectives))))

(provide 'packages-treemacs)

;;; packages-treemacs.el ends here
