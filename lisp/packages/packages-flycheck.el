;;; packages-flycheck.el --- Flycheck Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-helpers)

(use-package flycheck
  :init
  (setq flycheck-indication-mode nil
        flycheck-mode-line nil
        flycheck-display-errors-delay 3600.0
        flycheck-display-errors-function 'ignore
        flycheck-check-syntax-automatically '(save))
  :config
  (global-flycheck-mode))

(use-package flycheck-posframe :after flycheck
  :config
  (util/if-daemon-run-after-make-frame (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)))

(provide 'packages-flycheck)

;;; packages-flycheck.el ends here
