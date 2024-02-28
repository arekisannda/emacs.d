;;; packages-windows.el --- Window Management Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package shackle
  :config
  (setq shackle-default-rule '(:same t)))

(use-package popper :after (shackle persp-mode)
  :config
  (defvar management/window--popper-derived-mode-alist '())

  (defun packages/poppper--derived-mode-p (buf)
    "Return mode if BUF is derived from mode in `management/window--popper-derived-mode-alist`."
    (with-current-buffer buf
      (if (cl-some #'derived-mode-p management/window--popper-derived-mode-alist) t nil)))

  (setq management/window--popper-derived-mode-alist
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

  (setq popper-reference-buffers
        '("\\*lsp-help\\*$"
          "\\*compilation\\*$"
          "\\*Man.*\\*$"
          "\\*Flycheck.*\\*$"
          "\\*Ediff Control.*\\*$"
          "\\*evil-marks\\*$"
          "^magit.*$"
          "\\*Async Shell Command\\*$"
          "\\*EGLOT.*\\*$"
          "\\*diff-hl\\*$"
          packages/poppper--derived-mode-p))

  (setq popper-window-height 30
        popper-mode-line ""
        popper-display-control nil
        popper-group-function #'(lambda () (safe-persp-name (get-current-persp))))

  (popper-mode +1))

(provide 'packages-windows)

;;; packages-windows.el ends here
