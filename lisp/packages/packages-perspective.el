;;; packages-perspective.el --- Pespective Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-helpers)

(use-package persp-mode
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
  :preface
  (defun +persp-mode-tab-bar-setup ()
    "Add `persp-mode` hooks to save and restore tab-bar configurations."
    (require 'tab-bar)
    (add-hook 'persp-before-deactivate-functions
              #'(lambda (_) ; save tab-bar data
                  (when (get-current-persp)
                    (set-persp-parameter 'tab-bar-tabs (tab-bar-tabs)))))

    (add-hook 'persp-activated-functions
              #'(lambda (_) ; load tab-bar data
                  (tab-bar-tabs-set (persp-parameter 'tab-bar-tabs))
                  (tab-bar--update-tab-bar-lines t)))

    (add-hook 'persp-before-save-state-to-file-functions
              #'(lambda (&rest _) ; save tab-bar data
                  (when (get-current-persp)
                    (set-persp-parameter
                     'tab-bar-tabs
                     (frameset-filter-tabs (tab-bar-tabs) nil nil t))))))
  :hook
  (server-after-make-frame . (lambda () (when (and (daemonp) (not persp-mode))) (persp-mode 1)))
  (window-setup . (lambda () (when (not (or (daemonp) persp-mode)) (persp-mode 1))))
  (persp-mode . +persp-mode-tab-bar-setup))

(provide 'packages-perspective)

;;; packages-perspective.el ends here
