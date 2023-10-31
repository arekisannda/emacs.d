;;; base --- summary:
;;; Emacs base configurations
;;; manage fonts and configure Emacs GUI features
;;; commentary:

;;; code:

;; disable features
(setq-default inhibit-startup-screen t)
(setq-default confirm-kill-processes nil)
(setq-default auto-save-default nil)
(setq-default make-backup-files nil)
(setq-default create-lockfiles nil)

(global-eldoc-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tab-bar-mode 1)
(visual-line-mode -1)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; theme configurations
;; (use-package sonokai-theme
;;   :ensure t
;;   :elpaca(:type git :host github :repo "arekisannda/sonokai-emacs")
;;   :config (load-theme 'sonokai t))

;; for testing theme changes
(load-file "~/Code/sonokai-emacs/sonokai-theme.el")
(load-theme 'sonokai t)

;; pos-tip tooltip configurations
(setq-default pos-tip-internal-border-width 5)
(setq-default pos-tip-border-width 0)

;; line-number configurations
(setq-default display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; fringe configurations
(fringe-mode 5)
(setq-default fringe-styles 'default)
(setq-default fringe-indicator-alist nil)

;; font/line configurations
(defvar settings/default-font-size 80)
(defvar settings/tab-font-size 100)

(setq-default line-spacing 0)
(setq-default truncate-lines t)

;; tab-bar configurations
(setq-default tab-bar-close-button-show nil)
(setq-default tab-bar-new-button-show nil)
(setq-default tab-bar-auto-width-max '(150 20))
(setq-default tab-bar-auto-width-min '(20 2))

;; font configuration
(add-to-list 'default-frame-alist '(font . "FiraMono Nerd Font Mono-8"))

(defun config/set-custom-faces ()
  "Config method to set faces on frame create."
  (set-face-attribute 'default nil
                      :font "FiraMono Nerd Font Mono"
                      :height settings/default-font-size
                      :weight 'normal)
  (set-face-attribute 'fixed-pitch nil
                      :font "FiraMono Nerd Font Mono"
                      :height settings/default-font-size
                      :weight 'normal)
  (set-face-attribute 'variable-pitch nil
                      :font "Fira Sans"
                      :height settings/default-font-size
                      :weight 'normal)
  (set-face-attribute 'font-lock-comment-face nil
                      :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil
                      :slant 'italic)

  (set-face-attribute 'tab-bar nil
		              :font "FiraMono Nerd Font Mono"
		              :height settings/tab-font-size
		              :weight 'bold)
  (set-face-attribute 'tab-bar-tab nil
                      :font "FiraMono Nerd Font Mono"
                      :height settings/tab-font-size
                      :weight 'bold
                      :box '(:line-width (5 . 5) :style flat-button)
                      :underline `(:inherit tab-bar-tab :style line :position 0))
  (set-face-attribute 'tab-bar-tab-inactive nil
		              :font "FiraMono Nerd Font Mono"
		              :height settings/tab-font-size
		              :weight 'light
                      :box '(:line-width (5 . 5) :style flat-button)
                      :underline `(:inherit tab-bar-tab :style line :position 0)))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame) (with-selected-frame frame (config/set-custom-faces))))
  (config/set-custom-faces))

(provide 'config-base)
;;; base.el ends here
