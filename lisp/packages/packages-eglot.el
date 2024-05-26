;;; packages-eglot.el --- Eglot Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-helpers)

(use-package emacs
  :ensure nil
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eglot-events-buffer-size 0)
  (eglot-sync-connect nil)
  (eglot-connect-timeout nil)
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 3)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider
                                       :inlayHintProvider))
  :hook
  (eglot-managed-mode . (lambda ()(eldoc-mode -1))))

(use-package eglot-booster
  :ensure (:type git :host github :repo "jdtsmith/eglot-booster")
  :hook
  (elpaca-after-init . eglot-booster-mode)
  :custom
  (eglot-events-buffer-size 0)
  :config
  (fset #'jsonrpc--log-event #'ignore))

(use-package flycheck-eglot
  :hook
  (eglot-managed-mode . flycheck-eglot-mode))

(use-package consult-eglot :after eglot)

(provide 'packages-eglot)

;;; packages-eglot.el ends here
