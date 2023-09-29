;;; packages/utils --- summary:
;;; Emacs utility packages and configurations
;;; commentary:

;;; code:

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

(use-package diff-hl
  :ensure t
  :elpaca(:type git :host github :repo "arekisannda/diff-hl")
  :init
  ;; clear diff-hl default keybinds
  (setq diff-hl-show-hunk-map (make-sparse-keymap))
  (setq diff-hl-inline-popup-transient-mode-map (make-sparse-keymap))
  :config
  (if (display-graphic-p)
      (diff-hl-margin-mode 1))
  (global-diff-hl-mode 1))

;; diff tool configurations
;; (defun ediff-setup-windows-custom (buffer-A buffer-B buffer-C control-buffer))
(setq ediff-setup-windows-function 'ediff-setup-windows-merge)

;; editor utils
(use-package embrace
  :init
  (setq embrace-show-help-p nil))

;; transient buffer management utils
(use-package popper
  :init
  ;; set list of ephemeral buffers
  (setq popper-reference-buffers
	    '("\\*Messages\\*"
	      "\\*Warnings\\*"
	      "\\*Customize.*\\*$"
	      "\\*scratch\\*"
	      "\\*info.*\\*"
	      "\\*Man.*\\*$"
	      "\\*elpaca-.*\\*"
	      "\\*Flycheck.*\\*$"
	      "\\*Ediff Control.*\\*$"
	      "\\*evil-marks\\*$"
	      "^magit.*$"
	      "\\*Asyn*c Shell Command\\*"
	      "\\*EGLOT.*\\*$"
	      "Output\\*$"
	      compilation-!mode
	      help-mode))
  (setq popper-window-height 30
	    popper-mode-line "")
  :config
  (popper-mode +1))
;; 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

(provide 'packages-utils)
;;; utils.el ends here
