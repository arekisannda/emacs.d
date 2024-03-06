;;; packages-eglot.el --- Eglot Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-helpers)

(use-package seq
  :ensure (seq))

(use-package jsonrpc
  :ensure (jsonrpc))

(use-package eldoc
  :ensure (eldoc)
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  :init
  (setq eldoc-display-functions '(eldoc-display-in-buffer))
  :hook
  (elpaca-after-init . (lambda () (global-eldoc-mode -1))))

(use-package eglot
  :ensure (eglot)
  :custom
  (fset #'jsonrpc--log-event #'ignore)
  (eglot-events-buffer-size 0)
  (eglot-sync-connect nil)
  (eglot-connect-timeout nil)
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 3)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider
                                       :inlayHintProvider))
  :hook
  (eglot-managed-mode . (lambda () (eldoc-mode -1))))

(use-package eglot-booster
  :ensure (:type git :host github :repo "jdtsmith/eglot-booster")
  :hook
  (elpaca-after-init . eglot-booster-mode))

(use-package flycheck-eglot
  :hook
  (eglot-managed-mode . flycheck-eglot-mode))

(provide 'packages-eglot)

;;; packages-eglot.el ends here
