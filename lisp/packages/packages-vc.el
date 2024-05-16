;;; packages-vc.el --- Emacs Magit/VC Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-helpers)

(use-package magit
  :ensure t
  :custom
  (magit-repository-directories
   (list `(,(expand-file-name "~/Code/") . 1))))

(use-package diff-hl :after magit
  :ensure (:type git :host github :repo "arekisannda/diff-hl" :branch "master")
  :custom
  (diff-hl-show-staged-changes nil)
  (diff-hl-side-margin-width 3)
  (diff-hl-flydiff-delay 0.1)
  :init
  (setq diff-hl-show-hunk-map (make-sparse-keymap)
        diff-hl-inline-popup-transient-mode-map (make-sparse-keymap))
  :hook
  (global-diff-hl-mode . diff-hl-flydiff-mode)
  (elpaca-after-init . global-diff-hl-mode)
  (global-diff-hl-mode . diff-hl-margin-mode)
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package forge :after (magit transient))

(provide 'packages-vc)

;;; packages-vc.el ends here
