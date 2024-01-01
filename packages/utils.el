;;; utils.el --- Emacs utlity packages and configurations -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package shut-up)
(use-package no-littering)

;; keybinding utils
(use-package diminish
  :ensure t)
(elpaca-wait)

(use-package general)
(use-package hydra
  :init
  (setq hydra-key-doc-function nil))

;; dir navigation utils
(use-package ranger
  :config
  (ranger-override-dired-mode t))

;; version control utils
(use-package magit)
(use-package forge
  :after magit
  :init)

(defun config/set-vc-diff-hl-mode ()
  "Config method to set diff-hl mode on frame create."
  (unless (display-graphic-p)
      (diff-hl-margin-mode 1)))

(use-package diff-hl
  :ensure t
  :elpaca(:type git :host github :repo "arekisannda/diff-hl")
  :init
  ;; clear diff-hl default keybinds
  (setq diff-hl-show-hunk-map (make-sparse-keymap))
  (setq diff-hl-inline-popup-transient-mode-map (make-sparse-keymap))
  (setq diff-hl-side-margin-width 3)
  (setq diff-hl-flydiff-delay 0.1)
  :config
  (if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame) (with-selected-frame frame (config/set-vc-diff-hl-mode)))))
  (diff-hl-flydiff-mode 1)
  (global-diff-hl-mode nil))

;; diff tool configurations
;; (defun ediff-setup-windows-custom (buffer-A buffer-B buffer-C control-buffer))
(setq ediff-setup-windows-function 'ediff-setup-windows-merge)

;; editor utils
(use-package embrace
  :init
  (setq embrace-show-help-p t))

(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dirs '("~/.config/emacs-snippets"))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

;; transient buffer management utils
(use-package popper
  :init
  ;; set list of ephemeral buffers
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "\\*Backtrace\\*"
          "\\*Customize.*\\*$"
          "\\*lsp-help\\*"
          "\\*vterm\\*"
          "\\*compilation\\*"
          "\\*scratch\\*"
          "\\*info.*\\*"
          "\\*Man.*\\*$"
          "\\*elpaca-manager*\\*"
          "\\*elpaca-logs*\\*"
          "\\*Flycheck.*\\*$"
          "\\*Ediff Control.*\\*$"
          "\\*evil-marks\\*$"
          "^magit.*$"
          "\\*Async Shell Command\\*"
          "\\*EGLOT.*\\*$"
          "\\*Bookmark List\\*"
          "\\*Buffer List\\*"
          "Output\\*$"
          treemacs-mode
          dap-server-log-mode
          dap-ui-sessions-mode
          dap-ui-breakpoints-ui-list-mode
          compilation-mode
          help-mode))
  (setq popper-window-height 30
        popper-mode-line "")
  :config
  (popper-mode +1))
;; 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

(provide 'packages-utils)

;;; utils.el ends here
