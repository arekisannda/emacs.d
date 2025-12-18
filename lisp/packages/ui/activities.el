;;; ui/activites.el -*- lexical-binding: t; -*-

(defcustom +activities-save-all-skip '()
  "List of functions to skip `activities-save-all'."
  :type '(set (function :tag "functions")))

(use-package activities
  :custom
  (activities-name-prefix "@")
  (activities-always-persist t)
  (activities-anti-save-predicates
   '(active-minibuffer-window
     activities--backtrace-visible-p))

  (activities-window-persistent-parameters
   (list (cons 'header-line-format 'writable)
         (cons 'mode-line-format 'writable)
         (cons 'tab-line-format 'writable)
         (cons 'no-other-window 'writable)
         (cons 'no-delete-other-windows 'writable)
         (cons 'window-preserved-size 'writable)
         (cons 'window-side 'writable)
         (cons 'window-slot 'writable)
         (cons 'window-popup 'writable)
         (cons 'window-purpose 'writable)))

  (activities-mode-idle-frequency (if init-file-debug most-positive-fixnum 30))
  (+activities-save-all-skip
   '((lambda() (when (fboundp 'treemacs-is-treemacs-window-selected?) (treemacs-is-treemacs-window-selected?)))))
  (activities-bookmark-store nil)
  :init
  (when init-file-debug
    (advice-add #'activities-save-all :override #'ignore)
    (advice-add #'activities-save :override #'ignore)
    (advice-add #'+activities-save-all-around :override #'ignore))

  (unless init-file-debug
    (defun +activities-save-all-around (fn &rest args)
      (unless (run-hook-with-args-until-success '+activities-save-all-skip)
        (apply fn args)))

    (advice-add #'activities-save-all :around #'+activities-save-all-around))

  (defun +activities-suspend-eglot (activity)
    (activities-with activity
      (let* ((project-name (activities--project-name)))
        (+eglot--shutdown-project project-name))))

  (advice-add #'activities-suspend :before #'+activities-suspend-eglot)

  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)
  :hook
  (after-init . activities-mode)
  (activities-mode . activities-tabs-mode))

(with-eval-after-load 'activities
  (defun +activities-new-project ()
    "Create new activity with project."
    (interactive)
    (+create-new-tab)
    (condition-case err
        (progn
          (let ((default-directory "~/"))
            (call-interactively #'project-switch-project)
            (treemacs)
            (activities-define (project-name (project-current)))
            (call-interactively #'activities-revert)
            ))
      ((error quit)
       (tab-bar-close-tab))))
  )
