;;; packages-yasnippet.el --- YASnippet Package Configurations  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package yasnippet
  :init
  (setq-default yas-snippet-dirs
                `(,(expand-file-name "emacs-snippets" configs/user-config-dir)))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets :after yasnippet)

(provide 'packages-yasnippet)

;;; packages-yasnippet.el ends here
