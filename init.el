;; init.el  -*- lexical-binding: t; -*-

;; (profiler-start 'cpu+mem)

;; (add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defun +load (path &rest args)
  (condition-case err
      (let ((path (expand-file-name path user-emacs-directory))
            (noerror (nth 0 args))
            (nomessage (or (nth 1 args) 'nomessage))
            (nosuffix (nth 2 args))
            (must-suffix (nth 3 args)))
        (if (file-directory-p path)
            (dolist (file (directory-files-recursively path "\\.el$"))
              (load file noerror nomessage nosuffix must-suffix))
          (if (string-suffix-p ".org" path t)
              (org-babel-load-file path)
            (load path noerror nomessage nosuffix must-suffix))))
    ((error err)
     (message "Load error: %S" err))))

(defmacro +on (hook &rest body)
  (declare (indent 1))
  `(add-hook ',(intern (concat (util/function-name hook) "-hook")) (lambda () ,@body)))

(defun +recursive-load-path (path)
  "Recursively load sub-directories in PATH."
  (let ((path (expand-file-name path user-emacs-directory)))
    (when (file-accessible-directory-p path)
      (add-to-list 'load-path path)
      (dolist (subdir (mapcar #'file-name-directory (directory-files-recursively path "\\.el$")))
        (when (file-directory-p subdir)
          (add-to-list 'load-path subdir))))))

(dolist (path '("lisp/lib")) (+recursive-load-path path))

;; prevent FOUC
(use-package doom-themes
  :config
  (load-theme 'doom-monokai-pro t))

;; enable configurations
(setq custom-file (expand-file-name "custom.el.gpg" user-emacs-directory))

(condition-case err
    (load custom-file 'noerror)
  (error
   (require 'notifications)
   (notifications-notify
    :title "Init Failed"
    :body  "Unable to load custom file"
    :urgency 'critical)
   (kill-emacs)
   ))

(+load "lisp/packages/emacs")
(+load "lisp/packages/ui")
(+load "lisp/packages/editor")
(+load "lisp/packages/completion")
(+load "lisp/packages/input")
(+load "lisp/packages/org")
(+load "lisp/packages/tools")
(+load "lisp/packages/lang")

(+on window-setup
  (defvar packages/emacs-gc-cons-threshold (* 1024 1024 100))
  (setq-default gc-cons-threshold packages/emacs-gc-cons-threshold)
  (setq-default read-process-output-max (* 4 1024 1024))
  (setq message-log-max 2000)
  (with-current-buffer (messages-buffer) (messages-buffer-mode))
  (setenv "EDITOR" "emacsclient -q -r")

  (pcase (getenv "XDG_CURRENT_DESKTOP")
    ("sway" (+load "lisp/scripts/swaywm")))

  (+load "keybinds.org"))

(put 'narrow-to-region 'disabled nil)
