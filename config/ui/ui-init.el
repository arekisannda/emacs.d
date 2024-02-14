;;; ui-init.el --- Emacs UI Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'simple)

(require 'packages-init)
(require 'ui-fonts)
(require 'ui-tooltip)
(require 'ui-tab-bar)
(require 'ui-mode-line)

(setq-default ui/config--logo-path (expand-file-name "logo.png" user-emacs-directory))

(defun ui/config--dashboard-setup ()
  "Set up dashboard configuations."
  (setq dashboard-startup-banner ui/config--logo-path
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

  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (dashboard-setup-startup-hook))

(defun ui/config--feature-disable ()
  "Disable features."
  (setq-default truncate-lines t)
  (setq-default line-spacing 0)
  (setq-default indent-tabs-mode nil)
  (setq-default visual-line-mode nil)

  (tool-bar-mode -1)
  (tooltip-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (global-eldoc-mode -1)
  (electric-pair-mode -1)
  (disable-mouse-global-mode 1)

  (mapc #'disable-mouse-in-keymap
        (list evil-motion-state-map
              evil-normal-state-map
              evil-visual-state-map
              evil-insert-state-map
              dashboard-mode-map)))

(defun ui/config--theme-setup ()
  "Set up theme configurations."
  (load-theme 'sonokai t))

(defun ui/config--tab-bar-setup ()
  "Set up tab-bar configurations."
  (ui/tab-bar--persp-mode-setup)
  (ui/tab-bar-project-bar-mode)

  (setq-default tab-bar-close-button-show nil)
  (setq-default tab-bar-new-button-show nil)
  (setq-default tab-bar-auto-width-max '(150 20))
  (setq-default tab-bar-auto-width-min '(20 2)))

(defun ui/config--diminish-setup ()
  "Set up Diminish modes."
  (dolist (mode '(evil-collection-unimpaired-mode
                  auto-revert-mode
                  projectile-mode
                  disable-mouse-mode
                  company-mode))
    (diminish mode)))

(defun ui/config--font-setup ()
  "Set up fonts configurations."
  (util/dedup-add-to-list
   'default-frame-alist
   `(font . ,(concat ui/fonts-fixed-pitch-face
                     (format "-%d" (/ ui/fonts-fixed-pitch-size 10)))))
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame) (with-selected-frame frame (ui/fonts--custom-faces))))
    (ui/fonts--custom-faces)))

(defun ui/config--tooltip-setup ()
  "Set up tooltip configurations."
  (global-flycheck-mode 1)

  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame) (with-selected-frame frame (ui/tooltip--load-deferred-configurations))))
    (ui/tooltip--load-deferred-configurations)))

(defun ui/config--fringe-setup ()
  "Set up fringe configurations."
  (setq-default fringe-styles 'default)
  (setq-default fringe-indicator-alist nil)

  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (unless (display-graphic-p)
                      (diff-hl-margin-mode 1))))))

  (diff-hl-flydiff-mode 1)
  (global-diff-hl-mode nil)
  (fringe-mode 5))

(defun ui/config--treemacs-setup ()
  "Set up Treemacs configurations."
  (require 'treemacs-persp)
  (treemacs-load-theme "nerd-icons")
  (treemacs-set-scope-type 'Perspectives))

(defun ui/config--minibuffer-vertico-setup ()
  "Set up vertico configuratins."
  ;; (setq which-key-posframe-poshandler 'posframe-poshandler-window-bottom-left-corner)
  ;; (which-key-posframe-mode 1)
  (setq-default which-key-sort-order 'which-key-description-order)
  (which-key-mode 1)

  (vertico-mode 1)
  (vertico-multiform-mode 1)
  (savehist-mode 1)
  (marginalia-mode 1)

  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(defun ui/config--telephone-line-setup ()
  "Set up `telephone-line` configurations."
  (setq telephone-line-lhs
	'((evil   . (ui/mode-line--popper-tag-segment
		     telephone-line-evil-tag-segment))
	  (accent . (telephone-line-vc-segment
		     telephone-line-erc-modified-channels-segment
		     telephone-line-process-segment))
	  (nil    . (telephone-line-projectile-segment
		     telephone-line-buffer-segment))))

  (telephone-line-mode 1))

(defun ui/config-setup ()
  "Set up Emacs UI configurations."
  (ui/config--feature-disable)
  (ui/config--theme-setup)
  (ui/config--font-setup)
  (ui/config--tab-bar-setup)
  (ui/config--tooltip-setup)
  (ui/config--minibuffer-vertico-setup)
  (ui/config--diminish-setup)
  (ui/config--dashboard-setup)
  (ui/config--treemacs-setup)
  (ui/config--fringe-setup)
  (ui/config--telephone-line-setup)

  (ranger-override-dired-mode 1))

(ui/config-setup)

(provide 'ui-init)

;;; ui-init.el ends here
