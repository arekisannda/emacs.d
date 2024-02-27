;;; packages-interface.el --- Emacs Interface Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package nerd-icons :ensure t)

(use-package mixed-pitch :ensure t :disabled)

(use-package ranger :ensure t
  :config
  (ranger-override-dired-mode 1))

(use-package rainbow-mode :ensure t
  :config
  (setq-default rainbow-r-colors-alist '())
  (setq-default rainbow-html-colors-alist '()))

(use-package rainbow-delimiters :ensure t)

(use-package telephone-line :ensure t :after popper
  :config
  (require 'telephone-line-utils)
  (telephone-line-defsegment* packages/telephone-line-popper-tag-segment ()
    (if popper-popup-status "󰊠" nil))

  (setq telephone-line-lhs
        '((evil   . (packages/telephone-line-popper-tag-segment
                     telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-projectile-segment
                     telephone-line-buffer-segment))))

  (telephone-line-mode 1))

(use-package ace-window :ensure t :after posframe
  :init
  (setq-default aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
                aw-dispatch-when-more-than 0)
  :config
  (util/if-daemon-run-after-make-frame (ace-window-posframe-mode 1)))

(use-package dashboard :ensure t :after nerd-icons
  :init
  (setq dashboard-icon-type 'nerd-icons
        dashboard-startup-banner (expand-file-name "assets/logo.png" user-emacs-directory)
        dashboard-banner-logo-title nil
        dashboard-projects-backend 'project-el
        dashboard-center-content t
        dashboard-show-shortcuts t
        dashboard-display-icons-p t
        dashboard-set-heading-icons t
        dashboard-set-footer nil
        dashboard-set-file-icons t
        dashboard-items '((agenda . 8)  (bookmarks . 8) (projects . 16) (recents . 32))
        initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (dashboard-setup-startup-hook)

  (mapc #'disable-mouse-in-keymap
        (list dashboard-mode-map)))

(provide 'packages-interface)

;;; packages-interface.el ends here
