;;; packages-flycheck.el --- Flycheck Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'elpaca)

(use-package flycheck
  :ensure t
  :init
  (setq flycheck-indication-mode nil
        flycheck-mode-line nil
        flycheck-display-errors-delay 3600.0
        flycheck-display-errors-function 'ignore
        flycheck-check-syntax-automatically '(save)))
(use-package flycheck-posframe :ensure t :after flycheck)
(use-package flycheck-popup-tip :ensure t :after flycheck)


(provide 'packages-flycheck)

;;; packages-flycheck.el ends here
