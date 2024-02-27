;;; packages-interface.el --- Emacs Interface Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package nerd-icons :ensure t)

(use-package mixed-pitch :ensure t)

(use-package ranger :ensure t)

(use-package rainbow-mode :ensure t)

(use-package rainbow-delimiters :ensure t)

(use-package telephone-line :ensure t)

(use-package ace-window :ensure t
  :init
  (setq-default aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
                aw-dispatch-when-more-than 0))

(use-package diff-hl :ensure t
  :elpaca (:type git :host github :repo "arekisannda/diff-hl" :branch "master")
  :init
  ;; clear diff-hl default keybinds
  (setq diff-hl-show-staged-changes nil)
  (setq diff-hl-show-hunk-map (make-sparse-keymap))
  (setq diff-hl-inline-popup-transient-mode-map (make-sparse-keymap))
  (setq diff-hl-side-margin-width 3)
  (setq diff-hl-flydiff-delay 0.1))

(use-package dashboard :ensure t :after nerd-icons
  :init
  (setq dashboard-icon-type 'nerd-icons))

(provide 'packages-interface)

;;; packages-interface.el ends here
