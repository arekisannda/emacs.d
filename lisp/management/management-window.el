;;; management-window.el --- Emacs Window Management Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'winner)
(require 'packages-init)

(defvar management/window--popper-derived-mode-alist '())

(defun management/window--popper-derived-mode-p (buf)
  "Return mode if BUF is derived from mode in `management/window--popper-derived-mode-alist`."
  (with-current-buffer buf
    (if (cl-some #'derived-mode-p management/window--popper-derived-mode-alist) t nil)))

(defun management/windows--shackle-popper-setup ()
  "Set up shackle/popper configurations."
  (setq shackle-default-rule '(:same t))

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
          management/window--popper-derived-mode-p))

  (setq popper-window-height 30
        popper-mode-line ""
        popper-display-control nil
        popper-group-function #'(lambda () (safe-persp-name (get-current-persp))))

  (popper-mode +1))

(defun management/windows-setup ()
  "Set up window configurations."
  (setq window-resize-pixelwise t)
  (setq frame-resize-pixelwise t)
  ;; (setq display-buffer-base-action nil)

  (management/windows--shackle-popper-setup)
  (winner-mode 1))

(management/windows-setup)

(provide 'management-window)

;;; management-window.el ends here
