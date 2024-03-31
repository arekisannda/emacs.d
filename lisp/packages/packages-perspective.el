;;; packages-perspective.el --- Pespective Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-helpers)

(use-package persp-mode :disabled
  :custom
  ;; (wg-morph-on nil)
  ;; (persp-init-frame-behaviour nil)
  ;; (persp-init-new-frame-behaviour-override nil)
  ;; (persp-interactive-init-frame-behaviour-override nil)
  ;; (persp-emacsclient-init-frame-behaviour-override nil)
  (persp-set-last-persp-for-new-frames nil)
  (persp-remove-buffers-from-nil-persp-behaviour nil)
  (persp-autokill-buffer-on-remove 'kill)
  (persp-kill-foreign-buffer-behaviour 'kill)
  (persp-nil-name "main")
  :hook
  (server-after-make-frame . (lambda () (when (and (daemonp) (not persp-mode))
                                          (persp-mode 1)
                                          (ext-tab-bar-persp-mode-setup))))
  (elpaca-after-init . (lambda () (when (not (or (daemonp) persp-mode))
                                    (persp-mode 1)
                                    (ext-tab-bar-persp-mode-setup)))))

(use-package perspective
  :custom
  (persp-show-modestring nil)
  (persp-mode-prefix-key nil)
  (persp-switch-wrap nil)
  (persp-state-default-file (expand-file-name "var/perspective/persp-auto-save" user-emacs-directory))
  :hook
  (kill-emacs . persp-state-save)
  (emacs-startup . (lambda ()
                     (let ((perspective-dir (expand-file-name "var/perspective/" user-emacs-directory)))
                       (when (not (file-exists-p perspective-dir))
                         (make-directory perspective-dir)))
                     (when (file-exists-p persp-state-default-file)
                       (persp-state-load persp-state-default-file))
                     (persp-switch "main")))
  (persp-created . dashboard-open)
  (server-after-make-frame . (lambda () (when (and (daemonp) (not persp-mode))
                                          (persp-mode 1)
                                          (ext-tab-bar-persp-mode-setup))))
  (elpaca-after-init . (lambda () (when (not (or (daemonp) persp-mode))
                                    (persp-mode 1)
                                    (ext-tab-bar-persp-mode-setup)))))

(provide 'packages-perspective)

;;; packages-perspective.el ends here
