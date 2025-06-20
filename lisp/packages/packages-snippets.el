;;; packages-snippets.el --- Snippet/Template Packages Configurations  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package yasnippet
  :custom-face
  (yas-field-highlight-face
   ((nil :inherit region)))
  :custom
  (yas-indent-line 'fixed)
  (yas-keymap-disable-hook
   (lambda () (and (frame-live-p corfu--frame)
                   (frame-visible-p corfu--frame))))
  :hook
  (elpaca-after-init . yas-global-mode)
  :diminish yas-minor-mode)

(use-package yasnippet-snippets :after yasnippet
  :config
  (let ((dir (expand-file-name "snippets" user-emacs-directory)))
    (unless (member dir yas-snippet-dirs)
      (add-to-list 'yas-snippet-dirs dir)
      (yas--load-snippet-dirs))))

(provide 'packages-snippets)

;;; packages-snippets.el ends here
