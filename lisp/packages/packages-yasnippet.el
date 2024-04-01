;;; packages-yasnippet.el --- YASnippet Package Configurations  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package yasnippet
  :hook
  (elpaca-after-init . yas-global-mode))

(use-package yasnippet-snippets :after yasnippet)

(use-package emacs
  :ensure nil
  :preface
  (defun +yasnippet-add-dirs ()
    (let ((dir (expand-file-name "emacs-snippets" +user-config-dir)))
      (unless (member dir yas-snippet-dirs)
        (add-to-list 'yas-snippet-dirs dir)
        (yas--load-snippet-dirs))))
  :hook
  (elpaca-after-init . +yasnippet-add-dirs))

(provide 'packages-yasnippet)

;;; packages-yasnippet.el ends here
