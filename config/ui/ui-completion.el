;;; ui-completion.el --- Emacs Input Completion Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'packages-init)

(defun ui/completion-company-setup ()
  "Set up company configurations."
  (setq company-manual-begin t
        company-idle-delay nil
        company-tooltip-minimum-width 40
        company-tooltip-align-annotations t)

  (add-hook 'after-init-hook #'global-company-mode)
  (company-flx-mode +1)
  (company-auctex-init))

(defun ui/completion-corfu-setup ()
  "Set up corfu configurations."
  (setq-default corfu-cycle nil
                corfu-auto nil
                corfu-popupinfo-delay (cons nil 0.5)
                corfu-min-width 40)
  (setq completion-cycle-threshold nil)
  (setq tab-always-indent 'complete)

  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1))

(defun ui/completion-setup ()
  "Set up completion configurations."
  (ui/completion-corfu-setup))

(provide 'ui-completion)

;;; ui-completion.el ends here
