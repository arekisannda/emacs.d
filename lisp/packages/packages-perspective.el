;;; packages-perspective.el --- Pespective Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-helpers)

(use-package treemacs-perspective :after (treemacs perspective) :defer)
(use-package perspective :after treemacs
  :custom
  (persp-show-modestring nil)
  (persp-mode-prefix-key nil)
  (persp-switch-wrap nil)
  (persp-state-default-file
   (expand-file-name
    (cond (debug-on-error "debug")
          ((daemonp) "server")
          (t "default"))
    +persp-state-default-directory))
  :init
  (defvar +perspective-save-mutex (make-mutex))

  (defun +perspective-save (&optional terminated)
    (interactive)
    (with-mutex +perspective-save-mutex
      (dolist (persp (persp-names))
        (with-perspective persp
          (when terminated
            (walk-windows
             (lambda (window)
               (if (window-parameter window 'window-side)
                   (delete-window window)))))
          (ext-tab-bar-persp-mode-update)
          (persist-save 'ext-tab-bar-persp-mode-hash)))
      (persp-state-save persp-state-default-file)))

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

  (defun +perspective-terminated-save ()
    (+perspective-save t))

  :hook
  (kill-emacs . +perspective-terminated-save)
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
  (defun +persp-switch-to-scratch-buffer ()
    (interactive)
    (display-buffer (persp-get-scratch-buffer)))
  (advice-add #'persp-switch-to-scratch-buffer :override #'+persp-switch-to-scratch-buffer)

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
