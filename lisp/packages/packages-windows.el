;;; packages-windows.el --- Window Management Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package shackle
  :custom
  (shackle-default-rule '(:same t)))

(use-package popper :after shackle :disabled
  :preface
  (defvar +popper-derived-mode-alist '())

  (defun +popper-derived-mode-p (buf)
    "Return mode if BUF is derived from mode in `+popper-derived-mode-alist`."
    (with-current-buffer buf
      (if (cl-some #'derived-mode-p +popper-derived-mode-alist) t nil)))

  (setq +popper-derived-mode-alist
        '(messages-buffer-mode
          xref--xref-buffer-mode
          help-mode
          compilation-mode
          backtrace-mode
          lisp-interaction-mode
          Custom-mode
          vterm-mode
          grep-mode
          diff-mode
          elpaca-ui-mode
          elpaca-info-mode
          tabulated-list-mode))
  :custom
  (popper-reference-buffers '("\\*eldoc.*\\*$"
                              "\\*lsp-help\\*$"
                              "\\*compilation\\*$"
                              "\\*Man.*\\*$"
                              "\\*Flycheck.*\\*$"
                              "\\*Ediff Control.*\\*$"
                              "\\*evil-marks\\*$"
                              "^magit.*$"
                              "\\*Async Shell Command\\*$"
                              "\\*EGLOT.*\\*$"
                              "\\*diff-hl\\*$"
                              +popper-derived-mode-p))
  (popper-window-height 30)
  (popper-mode-line "")
  (popper-display-control nil)
  (popper-group-function #'(lambda () (cond ((featurep 'persp-mode) (safe-persp-name (get-current-persp)))
                                            ((featurep 'perspective) (persp-name (persp-curr))))))
  :hook
  (elpaca-after-init . popper-mode))

(use-package emacs :after telephone-line :disabled
  :ensure nil
  :config
  (telephone-line-defsegment* +telepohone-line-popper-tag-segment ()
    (if popper-popup-status "󰊠" nil))

  (setq telephone-line-lhs
        '((evil   . (+telepohone-line-popper-tag-segment
                     telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-projectile-segment
                     telephone-line-buffer-segment)))))

(use-package window-purpose)

(provide 'packages-windows)

;;; packages-windows.el ends here
