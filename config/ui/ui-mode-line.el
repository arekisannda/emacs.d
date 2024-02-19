;;; ui-mode-line.el --- Emacs Mode-Line Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'telephone-line-utils)
(require 'popper)

(telephone-line-defsegment* ui/mode-line--popper-tag-segment ()
  (if popper-popup-status "󰊠" nil))

(defun ui/mode-line-setup ()
  "Set up `telephone-line` configurations."
  (dolist (mode '(evil-collection-unimpaired-mode
                  auto-revert-mode
                  projectile-mode
                  disable-mouse-mode
                  company-mode))
    (diminish mode))

  (setq telephone-line-lhs
        '((evil   . (ui/mode-line--popper-tag-segment
                     telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-projectile-segment
                     telephone-line-buffer-segment))))

  (telephone-line-mode 1))

(provide 'ui-mode-line)

;;; ui-mode-line.el ends here
