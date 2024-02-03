;;; packages-perspective.el --- Emacs Pespective Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'elpaca)

;; (use-package perspective
;;   :ensure t)

(use-package persp-mode
  :ensure t
  :init
  (setq-default wg-morph-on nil)
  (setq-default persp-set-last-persp-for-new-frames nil
                ;; persp-init-frame-behaviour nil
                ;; persp-init-new-frame-behaviour-override nil
                ;; persp-interactive-init-frame-behaviour-override nil
                ;; persp-emacsclient-init-frame-behaviour-override nil
                persp-remove-buffers-from-nil-persp-behaviour nil
                persp-autokill-buffer-on-remove 'kill
                persp-kill-foreign-buffer-behaviour 'kill
                persp-nil-name "system"
                persp-inhibit-switch-for '("system")))

(provide 'packages-perspective)

;;; packages-perspective.el ends here
