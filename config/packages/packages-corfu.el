;;; packages-corfu.el --- Corfu Completion Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package corfu :ensure t :demand t
  :init
  (setq corfu-map (make-sparse-keymap)
        corfu-popupinfo-map (make-sparse-keymap)))

(use-package nerd-icons-corfu :ensure t :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package corfu-terminal :disabled)

(use-package corfu-candidate-overlay :ensure t :after corfu)

(use-package cape :ensure t)

(use-package company-math :ensure t :after latex)

(use-package company-reftex :ensure t :after latex)

(use-package company-auctex :ensure t :after latex)

(use-package yasnippet-capf :ensure t :after yasnippet)

(elpaca-wait)

(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer."
  (when (local-variable-p 'completion-at-point-functions)
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-keyword)

(provide 'packages-corfu)

;;; packages-corfu.el ends here
