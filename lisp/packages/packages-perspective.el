;;; packages-perspective.el --- Pespective Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-helpers)

(use-package treemacs-persp :after (treemacs persp-mode) :defer :disabled)

(use-package persp-mode :after treemacs :disabled
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
  (persp-auto-save-num-of-backups 0)

  (consult-buffer-sources
   (append '(persp-consult-source)
           consult-buffer-sources))

  :preface
  (defun +perspective-init ()
    (require 'treemacs-persp)
    (treemacs-set-scope-type 'Perspectives)
    (delete-other-windows)
    (dashboard-open))
  :init
  (require 'persist)
  (persist-defvar persp-mode-created-hash (make-hash-table :test 'equal) nil)

  (add-hook 'persp-created-functions
            (lambda (&optional persp hash)
              (puthash (safe-persp-name persp) t persp-mode-created-hash)))

  (add-hook 'persp-activated-functions
            (lambda (&optional _)
              (if-let* ((persp (safe-persp-name (get-current-persp)))
                        (after-created (gethash persp persp-mode-created-hash)))
                  (progn
                    (persp-remove-buffer (current-buffer))
                    (delete-other-windows)
                    (dashboard-open)
                    (remhash persp persp-mode-created-hash)))))

  (util/if-daemon-run-after-make-frame-else-add-hook
   (progn
     (persp-mode 1)
     (ext-tab-bar-persp-mode-setup)
     (+perspective-init))
   'emacs-startup-hook))

(use-package treemacs-perspective :after (treemacs perspective) :defer)

(use-package perspective :after treemacs
  :custom
  (persp-show-modestring nil)
  (persp-mode-prefix-key nil)
  (persp-switch-wrap nil)
  (persp-state-default-file
   (if debug-on-error
       (expand-file-name "debug" +persp-state-default-directory)
     (expand-file-name "default" +persp-state-default-directory)))
  :preface
  (aio-defun +perspective-save ()
    (interactive)
    (dolist (persp (persp-names))
      (with-perspective
          persp
        (if-let ((treemacs-buffer (treemacs-get-local-buffer)))
            (kill-buffer treemacs-buffer))))
    (persp-state-save persp-state-default-file))

  (aio-defun +perspective-save-async ()
    (interactive)
    (aio-await (+perspective-save)))

  (defun +perspective-init ()
    (require 'treemacs-perspective)
    (treemacs-set-scope-type 'Perspectives)
    (when (not (file-exists-p +persp-state-default-directory))
      (make-directory +persp-state-default-directory))
    (aio-with-async
      (when (and (file-regular-p persp-state-default-file)
                 (not debug-on-error))
        (persp-state-load persp-state-default-file))))

  :hook
  (persp-state-before-save . (lambda ()
                               (ext-tab-bar-persp-mode-update)
                               (persist-save 'ext-tab-bar-persp-mode-hash)))
  (kill-emacs . +perspective-save)
  (persp-created . dashboard-open)
  :init
  (util/if-daemon-run-after-make-frame-else-add-hook
   (progn
     (persp-mode 1)
     (ext-tab-bar-persp-mode-setup)
     (+perspective-init))
   'emacs-startup-hook)
  (run-with-timer (* 30 60) t #'+perspective-save-async)
  :config
  (defun +persp--state-frame-data ()
    (cl-loop for frame in (frame-list)
             if (and (not (equal "initial_terminal" (terminal-name frame)))
                     (frame-parameter frame 'persp--hash)) ; XXX: filter non-perspective-enabled frames
             collect (with-selected-frame frame
                       (let ((persps-in-frame (make-hash-table :test 'equal))
                             (persp-names-in-order (persp-names)))
                         (cl-loop for persp in persp-names-in-order do
                                  (unless (persp-killed-p (gethash persp (perspectives-hash)))
                                    (with-perspective persp
                                      (let* ((buffers
                                              (cl-loop for buffer in (persp-current-buffers)
                                                       if (persp--state-interesting-buffer-p buffer)
                                                       collect (buffer-name buffer)))
                                             (windows
                                              (cl-loop for entry in (window-state-get (frame-root-window) t)
                                                       collect (persp--state-window-state-massage entry persp buffers))))
                                        (puthash persp
                                                 (make-persp--state-single
                                                  :buffers buffers
                                                  :windows windows)
                                                 persps-in-frame)))))
                         (make-persp--state-frame-v2
                          :persps persps-in-frame
                          :order persp-names-in-order
                          :merge-list (frame-parameter nil 'persp-merge-list))))))
  (advice-add #'persp--state-frame-data :override #'+persp--state-frame-data)

  (defun +persp-init-state (&optional _)
    (persp-switch "main")
    (delete-other-windows)
    (dashboard-open))
  (advice-add #'persp-state-load :after #'+persp-init-state))

(provide 'packages-perspective)

;;; packages-perspective.el ends here
