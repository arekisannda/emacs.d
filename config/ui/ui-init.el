;;; ui-init.el --- Emacs UI Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'simple)

(require 'packages-init)
(require 'ui-fonts)
(require 'ui-tab-bar)
(require 'ui-tooltip)
(require 'ui-mode-line)
(require 'ui-extra)
(require 'ui-base-editor)
(require 'ui-code-editor)
(require 'ui-minibuffer)
(require 'ui-completion)

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
  (let ((theme 'sonokai))
    (load-theme theme t)
    (ui/colors-load-theme theme)))

(defun ui/config-setup ()
  "Set up Emacs UI configurations."
  (ui/config--feature-disable)

  (ui/base-editor-setup)
  (ui/code-editor-setup)
  (ui/minibuffer-setup)
  (ui/completion-setup)

  (ui/config--theme-setup)
  (ui/fonts-setup)
  (ui/tab-bar-setup)
  (ui/tooltip-setup)
  (ui/mode-line-setup)
  (ui/extra-setup))

(ui/config-setup)

(provide 'ui-init)

;;; ui-init.el ends here
