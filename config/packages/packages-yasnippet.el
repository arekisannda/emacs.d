;;; packages-yasnippet.el --- YASnippet Packages  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'elpaca)

(use-package yasnippet
  :ensure t
  :init
  (setq-default yas-snippet-dirs '("~/.config/emacs-snippets"))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets :ensure t :after yasnippet)




(provide 'packages-yasnippet)

;;; packages-yasnippet.el ends here
