;;; packages/editor --- summary:
;;; Emacs editor related packages and configurations
;;; commentary:

;;; code:

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-banner-logo-title nil
	dashboard-startup-banner nil
	dashboard-center-content t
	dashboard-show-shortcuts t
	dashboard-set-footer nil)
  :config
  ;;(add-to-list 'evil-emacs-state-modes )
  (dashboard-setup-startup-hook))

;; additional icons
(use-package all-the-icons
  :if (display-graphic-p))

;; window nvaigation
(use-package ace-window
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; editor enhancements
(use-package rainbow-mode)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; packages for disabling some emacs features
(use-package disable-mouse
  :ensure t
  :hook (prog-mode . disable-mouse-mode))

(use-package telephone-line
  :config
  (telephone-line-mode 1))

(provide 'packages-editor)
;;; editor.el ends here
