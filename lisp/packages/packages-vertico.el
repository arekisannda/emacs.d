;;; packages-vertico.el --- Vertico Completion Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'keymap)
(require 'cl-seq)
(require 'util-helpers)

(use-package which-key
  :init
  (setq-default which-key-sort-order 'which-key-description-order)
  :config
  (which-key-mode 1))

(use-package which-key-posframe :after which-key :disabled
  :config
  (setq which-key-posframe-poshandler 'posframe-poshandler-window-bottom-left-corner)
  (which-key-posframe-mode 1))

(use-package vertico
  :init
  (setq savehist-file (expand-file-name "var/savehist" user-emacs-directory))
  :config

  (defun packages/vertico--sort-directories-first (files)
    "Sort FILES by directories first."
    (setq files (vertico-sort-history-length-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  (setq vertico-multiform-commands
        `((find-file (vertico-sort-override-function . vertico-sort-alpha))
          (project-switch-project (vertico-sort-override-function . packages/vertico--sort-directories-first))
          (project-kill-buffers (vertico-sort-override-function . packages/vertico--sort-directories-first))
          (project-find-file (vertico-sort-override-function . packages/vertico--sort-directories-first))
          (project-find-dir (vertico-sort-override-function . packages/vertico--sort-directories-first))
          (project-forget-project (vertico-sort-override-function . packages/vertico--sort-directories-first))
          (project-forget-project-under (vertico-sort-override-function . packages/vertico--sort-directories-first))
          (describe-symbol (vertico-sort-override-function . vertico-sort-alpha))))

  (setq vertico-multiform-categories
        `((file (vertico-sort-override-function . packages/vertico--sort-directories-first))
          (consult-grep buffer (vertico-buffer-display-action . (display-buffer-same-window)))))

  (vertico-mode 1)
  (vertico-multiform-mode 1)
  (savehist-mode 1))

(use-package orderless :after vertico
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia :after vertico
  :config
  (marginalia-mode 1))

(use-package consult :after vertico
  :init
  (setq consult-narrow-key "<"
        register-preview-delay 0.5
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Disable preview for consult-grep commands
  (consult-customize consult-ripgrep consult-git-grep consult-grep :preview-key nil)

  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode))

(use-package embark)

(use-package embark-consult :after (embark consult)
  :config
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(use-package consult-dir :after consult)

(use-package affe :after consult)

(use-package emacs :after (vertico embark ace-window)
  :ensure nil
  :config
  (defvar packages/vertico-embark-prompter-map (make-sparse-keymap)
    "Embark completion read prompter map.")

  (eval-when-compile
    (defmacro packages/vertico--embark-ace-action (fn)
      `(defun ,(intern (concat "packages/vertico--embark-ace-" (symbol-name fn))) ()
         (interactive)
         (with-demoted-errors "%s"
           (require 'ace-window)
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))

  (defun packages/vertico--embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (defun packages/vertico-embark-act-with-completing-read (&optional args)
    "Display embark actions in the minibuffer.
Passes on ARGS to `embark-act`"
    (interactive "P")
    (let* ((embark-prompter 'embark-completing-read-prompter)
           (act (propertize "Act" 'face 'highlight))
           (embark-indicator (lambda (_keymap targets) nil)))
      (embark-act args)))

  (advice-add 'embark-completing-read-prompter
              :around (util/with-minibuffer-keymap
                       packages/vertico-embark-prompter-map))

  (packages/vertico--embark-ace-action find-file)
  (packages/vertico--embark-ace-action switch-to-buffer)
  (packages/vertico--embark-ace-action bookmark-jump))

(use-package emacs :after (vertico which-key)
  :ensure nil
  :config
  (setq embark-indicators
        '(packages/vertico--embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun packages/vertico--embark-hide-which-key-indicator (fn &rest args)
    "Hide the `which-key` indicator after using the embark prompter.
Executes FN with ARGS."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'packages/vertico--embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'packages/vertico--embark-hide-which-key-indicator))

(use-package emacs :after vertico
  :ensure nil
  :config
  (defun consult--orderless-regexp-compiler (input type &rest _config)
    (setq input (orderless-pattern-compiler input))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input t str))))

  ;; OPTION 1: Activate globally for all consult-grep/ripgrep/find/...
  ;; (setq consult--regexp-compiler #'consult--orderless-regexp-compiler)

  ;; OPTION 2: Activate only for some commands, e.g., consult-ripgrep!
  (defun consult--with-orderless (&rest args)
    (minibuffer-with-setup-hook
        (lambda ()
          (setq-local consult--regexp-compiler #'consult--orderless-regexp-compiler))
      (apply args)))
  (advice-add #'consult-ripgrep :around #'consult--with-orderless)

  (keymap-substitute project-prefix-map #'project-find-regexp #'consult-ripgrep)
  (cl-nsubstitute-if
   '(consult-ripgrep "Find regexp")
   (pcase-lambda (`(,cmd _)) (eq cmd #'project-find-regexp))
   project-switch-commands))

(provide 'packages-vertico)

;;; packages-vertico.el ends here
