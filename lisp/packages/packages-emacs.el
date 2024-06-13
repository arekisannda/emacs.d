;;; packages-emacs.el --- Emacs Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-helpers)

(use-package emacs
  :after (posframe which-key-posframe vertico-posframe transient-posframe easy-color-faces)
  :ensure nil
  :custom
  (vertico-count 20)
  (vertico-posframe-width nil)
  (vertico-posframe-min-width 120)
  (vertico-posframe-min-height nil)
  :config
  (defun +emacs-setup-posframe-parameters ()
    (setq vertico-posframe-width 120)
    (setq vertico-posframe-min-width 120)
    (setq vertico-posframe-min-height nil)
    (setq vertico-posframe-parameters
          `((max-width . 200)
            (max-height . 20)
            (left-fringe . nil)
            (right-fringe . nil)
            (background-color . ,(easy-color-darken (face-background 'default) 5))))

    (setq transient-posframe-parameters
          `((left-fringe . nil)
            (right-fringe . nil)
            (background-color . ,(easy-color-darken (face-background 'default) 5))))

    (setq which-key-posframe-parameters
          `((min-width . ,(frame-pixel-width))
            (min-height . 20)
            (left-fringe . nil)
            (right-fringe . nil)
            (background-color . ,(easy-color-darken (face-background 'default) 5))))
    )

  (util/if-daemon-run-after-make-frame-else-add-hook
   (+emacs-setup-posframe-parameters)
   'window-setup-hook))

(use-package emacs
  :ensure nil
  ;;:diminish auto-revert-mode
  :preface
  (defun +emacs-tuning-configurations ()
    ;; performance tuning
    (defvar packages/emacs-gc-cons-threshold (* 1024 1024 100))
    (setq-default gc-cons-threshold packages/emacs-gc-cons-threshold)
    (setq-default read-process-output-max (* 1024 1024)))

  (defun +emacs-configurations ()
    (setq-default window-resize-pixelwise t)
    (setq-default frame-resize-pixelwise t)
    (setq-default hscroll-step 5)
    (setq-default scroll-step 5)
    (setq-default tab-width 4)
    (setq-default tab-bar-separator "")
    (setq-default fringe-indicator-alist nil)
    (fringe-mode nil))

  (defcustom +emacs-read-only-prefixes-list
    (list (expand-file-name elpaca-directory)
          (expand-file-name package-user-dir)
          "/usr/share/emacs/")
    "List of read-only file prefixes."
    :group 'convenience
    :type '(list :element-type string))

  (defun +emacs-set-read-only-by-prefix ()
    "Enable `read-only-mode` if buffer includes one of `+emacs-read-only-prefixes-list`."
    (when (and buffer-file-name
               (cl-loop for prefix in +emacs-read-only-prefixes-list
                        thereis (string-prefix-p prefix buffer-file-name)))
      (read-only-mode 1)
      (evil-motion-state t)))

  (defun +emacs-set-visual-line-mode ()
    "Setup to run for non `prog-mode` major modes."
    (setq truncate-lines nil)
    (visual-line-mode 1)
    (visual-fill-column-mode 1))

  (defun +emacs-minibuffer-setup ()
    (setq gc-cons-threshold most-positive-fixnum))

  (defun +emacs-minibuffer-exit ()
    (setq gc-cons-threshold packages/emacs-gc-cons-threshold))

  (defun +emacs-create-directory-on-save ()
    (when buffer-file-name
      (let ((dir (file-name-directory buffer-file-name)))
        (when (and (not (file-exists-p dir))
                   (y-or-n-p (format "Directory %s does not exist; Create it?" dir)))
          (make-directory dir t)))))

  :hook
  (find-file . +emacs-set-read-only-by-prefix)
  (help-mode . +emacs-set-visual-line-mode)
  (elpaca-after-init . (lambda () (load custom-file 'noerror)))
  (minibuffer-setup . +emacs-minibuffer-setup)
  (minibuffer-exit . +emacs-minibuffer-exit)
  (window-setup . +emacs-tuning-configurations)
  (emacs-startup . +emacs-configurations)
  (before-save . +emacs-create-directory-on-save))

(provide 'packages-emacs)

;;; packages-emacs.el ends here
