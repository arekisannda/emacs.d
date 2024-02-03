;;; editor-completion.el --- Emacs Input Completion Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'packages-init)

(defun editor/completion--company-setup ()
  "Set up company configurations."
  (setq company-manual-begin t
        company-idle-delay nil
        company-tooltip-minimum-width 40
        company-tooltip-align-annotations t)

  (add-hook 'after-init-hook #'global-company-mode)
  (company-flx-mode +1)
  (company-auctex-init))

(defun editor/completion-setup ()
  "Set up completion configurations."
  (editor/completion--company-setup))

(editor/completion-setup)

(provide 'editor-completion)

;;; editor-completion.el ends here
