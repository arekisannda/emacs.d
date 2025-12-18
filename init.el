;; init.el  -*- lexical-binding: t; -*-

;; (profiler-start 'cpu+mem)

;; (add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defun +load (path &rest args)
  (condition-case err
      (let ((path (expand-file-name path user-emacs-directory)))
        (if (file-directory-p path)
            (dolist (file (directory-files-recursively path "\\.el$"))
              (apply #'load file args))
          (apply #'load path args)))
    (error nil)))

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
(setq custom-file "custom.el.gpg")

(+load custom-file 'noerror)
(+load "lisp/packages/emacs")
(+load "lisp/packages/ui")
(+load "lisp/packages/editor")
(+load "lisp/packages/completion")
(+load "lisp/packages/input")
(+load "lisp/packages/lang")
(+load "lisp/packages/org")
(+load "lisp/packages/tools")

(put 'narrow-to-region 'disabled nil)
