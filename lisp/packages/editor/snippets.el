;;; editor/snippets.el -*- lexical-binding: t; -*-

(use-package yasnippet
  :custom
  (yas-indent-line 'fixed)
  (yas-keymap-disable-hook
   (lambda () (and (frame-live-p corfu--frame)
                   (frame-visible-p corfu--frame))))
  :config
  (utils/custom-set-faces
   (yas-field-highlight-face
    ((nil :inherit region)))
   )
  :diminish yas-minor-mode)

(use-package yasnippet-snippets :after yasnippet
  :config
  (let ((dir (expand-file-name "snippets" user-emacs-directory)))
    (unless (member dir yas-snippet-dirs)
      (add-to-list 'yas-snippet-dirs dir)
      (yas--load-snippet-dirs))))

(use-package yasnippet-capf :after yasnippet
  :custom
  (yasnippet-capf-lookup-by 'key))
