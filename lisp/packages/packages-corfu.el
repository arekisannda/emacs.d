;;; packages-corfu.el --- Corfu Completion Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package corfu :ensure t :demand t
  :init
  (setq corfu-map (make-sparse-keymap)
        corfu-popupinfo-map (make-sparse-keymap))
  :config
  (setq-default corfu-cycle nil
                corfu-auto nil
                corfu-on-exact-match 'show
                corfu-popupinfo-delay (cons nil 0.5)
                corfu-min-width 40
                corfu-max-width 100
                corfu-left-margin-width 1.0
                corfu-right-margin-width 1.0
                corfu-scroll-margin 2
                corfu-bar-width 0.5)
  (setq-default completion-cycle-threshold nil)
  (setq-default tab-always-indent nil)

  (defun packages/corfu-minibuffer-completion-setup ()
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


  (add-hook 'minibuffer-setup-hook #'packages/corfu-minibuffer-completion-setup)

  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1))

(use-package nerd-icons-corfu :ensure t :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package corfu-terminal :disabled)

(use-package corfu-candidate-overlay :ensure t :after corfu)

(use-package cape :ensure t
  :config
  (require 'cape-char)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package company-math :ensure t :after latex)

(use-package company-reftex :ensure t :after latex)

(use-package company-auctex :ensure t :after latex)

(use-package yasnippet-capf :ensure t :after yasnippet
  :config
  (setq yasnippet-capf-lookup-by 'key))

(provide 'packages-corfu)

;;; packages-corfu.el ends here
