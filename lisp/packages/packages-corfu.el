;;; packages-corfu.el --- Corfu Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package corfu :demand t
  :init
  (setq corfu-map (make-sparse-keymap)
        corfu-popupinfo-map (make-sparse-keymap))
  :custom
  (corfu-cycle nil)
  (corfu-auto nil)
  (corfu-on-exact-match 'show)
  (corfu-popupinfo-delay (cons nil 0.5))
  (corfu-min-width 40)
  (corfu-max-width 100)
  (corfu-left-margin-width 1.0)
  (corfu-right-margin-width 1.0)
  (corfu-scroll-margin 2)
  (corfu-popupinfo-delay (cons nil 0.5))
  (corfu-bar-width 0.5)
  (completion-cycle-threshold nil)
  (tab-always-indent nil)
  :hook
  (elpaca-after-init . global-corfu-mode)
  (elpaca-after-init . corfu-popupinfo-mode))

(use-package emacs :after corfu
  :ensure nil
  :preface
  (defun +corfu-minibuffer-completion-setup ()
    "Setup to run for minibuffer mode."
    (shut-up
      (when (local-variable-p 'completion-at-point-functions)
        (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                    corfu-auto nil
                    corfu-cycle nil
                    corfu-popupinfo-delay (cons nil 0.5)
                    corfu-min-width 40))
      (setq-local completion-cycle-threshold nil)
      (setq-local tab-always-indent nil)

      (corfu-mode 1)
      (corfu-candidate-overlay-mode 1)))
  :hook
  (minibuffer-setup . +corfu-minibuffer-completion-setup))

(use-package nerd-icons-corfu :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package corfu-terminal :disabled)

(use-package corfu-candidate-overlay :after corfu
  :commands (corfu-candidate-overlay))

(use-package cape
  :config
  (require 'cape-char)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package yasnippet-capf :after yasnippet
  :custom
  (yasnippet-capf-lookup-by 'key))

(provide 'packages-corfu)

;;; packages-corfu.el ends here
