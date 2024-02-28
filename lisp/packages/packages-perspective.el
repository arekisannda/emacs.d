;;; packages-perspective.el --- Emacs Pespective Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package persp-mode
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
                persp-nil-name "main")
  :config
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook
                #'(lambda () (unless persp-mode (persp-mode 1))))
    (add-hook 'window-setup-hook
              #'(lambda () (unless persp-mode (persp-mode 1)))))


  (defun packages/perspective--tab-bar-setup ()
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


  (add-hook 'persp-mode-hook #'packages/perspective--tab-bar-setup))

(provide 'packages-perspective)

;;; packages-perspective.el ends here
