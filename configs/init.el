;;; init.el --- Emacs base configurations -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defvar configs-directory (expand-file-name "configs" user-emacs-directory))

(load-file (expand-file-name "gui.el" configs-directory))
(load-file (expand-file-name "ispell.el" configs-directory))

(setq-default confirm-nonexistent-file-or-buffer nil)
(setq-default inhibit-startup-screen t)
(setq-default confirm-kill-processes nil)
(setq-default auto-save-default nil)
(setq-default make-backup-files nil)
(setq-default create-lockfiles nil)

;; (setq project-vc-merge-submodules t)
(setq project-vc-extra-root-markers '(".project.el" ".projectile" ".project"))
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default message-log-max 2000)
(kill-buffer "*Messages*")

(provide 'config-init)

;;; init.el ends here
