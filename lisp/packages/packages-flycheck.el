;;; packages-flycheck.el --- Flycheck Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-helpers)

(use-package flycheck
  :custom
  (flycheck-indication-mode nil)
  (flycheck-mode-line nil)
  (flycheck-display-errors-delay 3600.0)
  (flycheck-display-errors-function 'ignore)
  (flycheck-check-syntax-automatically '(save))
  :hook
  (elpaca-after-init . global-flycheck-mode))

(use-package flycheck-posframe :after (flycheck easy-color-faces)
  :custom
  (flycheck-posframe-border-width 1)
  :custom-face
  (flycheck-posframe-border-face
   ((t (:foreground ,easy-color-white
                    :background unspecified))))
  :hook
  (elpaca-after-init . flycheck-posframe-configure-pretty-defaults)
  (flycheck-mode . flycheck-posframe-mode))

(provide 'packages-flycheck)

;;; packages-flycheck.el ends here
