;;; packages-tools.el --- Emacs Tool Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package treemacs
  :custom
  (treemacs-user-mode-line-format nil)
  (treemacs-is-never-other-window t)
  (treemacs-display-in-side-window t)
  (treemacs-position 'left)
  (treemacs-width 40)
  (treemacs-RET-actions-config '((root-node-open . treemacs-toggle-node)
                                 (root-node-closed . treemacs-toggle-node)
                                 (dir-node-open . treemacs-toggle-node)
                                 (dir-node-closed . treemacs-toggle-node)
                                 (file-node-open . treemacs-visit-node-in-most-recently-used-window)
                                 (file-node-closed . treemacs-visit-node-in-most-recently-used-window)
                                 (tag-node-open . treemacs-toggle-node-prefer-tag-visit)
                                 (tag-node-closed . treemacs-toggle-node-prefer-tag-visit)
                                 (tag-node . treemacs-visit-node-in-most-recently-used-window)))
  (treemacs-collapse-dirs 0)
  (treemacs-sorting 'alphabetic-numeric-asc)
  :custom-face
  (treemacs-window-background-face
   ((nil :background ,(doom-color 'bg-alt))))
  (treemacs-hl-line-face
   ((nil :background ,(doom-color 'bg))))
  (treemacs-peek-mode-indicator-face
   ((nil :background ,(doom-color 'green))))
  :preface
  (defun +treemacs--setup-peek-buffer (path)
    "Setup the peek buffer and window for PATH."
    (let* ((inhibit-message t)
           (file-buffer (get-file-buffer path))
           (next-window (next-window (selected-window)))
           (window (if file-buffer next-window
                     next-window)))
      (save-selected-window
        (select-window window)
        (unless treemacs--pre-peek-state
          (setf treemacs--pre-peek-state (list window (window-buffer window))))
        (if file-buffer
            (switch-to-buffer file-buffer :norecord)
          (find-file-existing path)
          (add-to-list 'treemacs--peeked-buffers (current-buffer))))))

  (advice-add #'treemacs--setup-peek-buffer :override #'+treemacs--setup-peek-buffer)

  (defun +treemacs--clean-workspaces ()
    "Find top-level headings that are not 'Default' or don't match the pattern 'Tab :' and cut them."
    (interactive)
    (require 'treemacs)
    (mapc (lambda (workspace)
            (when (and (not (string= workspace "Default"))
                       (not (string-match-p "^Tab @" workspace)))
              (treemacs-do-remove-workspace workspace nil)))
          (mapcar #'treemacs-workspace->name treemacs--workspaces)))
  :config
  (defun +treemacs--popup-window-override ()
    "Pop up a side window and buffer for treemacs."
    (let ((buf (treemacs-get-local-buffer-create)))
      (display-buffer buf
                      `(,(if treemacs-display-in-side-window
                             'display-buffer-in-side-window
                           'display-buffer-in-direction)
                        . (;; for buffer in direction
                           (direction . ,treemacs-position)
                           (window . root)
                           ;; for side windows
                           (slot . 0)
                           (side . ,treemacs-position)
                           ;; general-purpose settings
                           (window-width . ,treemacs-width)
                           (dedicated . t))))
      (select-window (get-buffer-window buf))))

  (advice-add #'treemacs--popup-window :override #'+treemacs--popup-window-override)

  (defun +treemacs--select-window-guard (orig-fn &rest r)
    (when (not (seq-some
                (lambda (p) (funcall p))
                '((lambda () (frame-parameter nil '+side-frame)))))
      (apply orig-fn r)))

  (advice-add #'treemacs-select-window :around #'+treemacs--select-window-guard)

  (defun +treemacs--setup ()
    (treemacs-filewatch-mode 1)
    (treemacs-fringe-indicator-mode 'only-when-focused)
    (setq mode-line-format nil))
  :hook
  (kill-emacs . +treemacs--clean-workspaces)
  (treemacs-mode . +treemacs--setup))

(use-package treemacs-nerd-icons :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-evil :after (treemacs evil))

(use-package treemacs-magit :after (treemacs magit))

(use-package treemacs-tab-bar :after treemacs
  :config (treemacs-set-scope-type 'Tabs))

(use-package magit
  :custom
  (magit-commit-diff-inhibit-same-window t)
  (magit-save-repository-buffers 'dontask)
  (magit-commit-show-diff nil)
  (magit-branch-direct-configure nil)
  (magit-refresh-status-buffer nil)
  :custom-face
  (magit-header-line
   ((nil :weight bold
         :foreground ,(doom-color 'fg)
         :background ,(doom-color 'bg-alt)
         :box (:line-width (1 . 1) :color ,(doom-color 'bg-alt) :style nil))))
  :config
  (defun +magit-repolist-setup-override (columns)
    (unless magit-repository-directories
      (user-error "You need to customize `magit-repository-directories' %s"
                  "before you can list repositories"))
    (with-current-buffer (get-buffer-create "*Magit Repositories*")
      (magit-repolist-mode)
      (setq-local magit-repolist-columns columns)
      (magit-repolist-setup-1)
      (magit-repolist-refresh)
      (pop-to-buffer (current-buffer))))

  (advice-add #'magit-repolist-setup :override #'+magit-repolist-setup-override))

(use-package forge :after (magit transient))

(use-package diff-hl :after magit
  :custom
  (diff-hl-show-staged-changes nil)
  (diff-hl-flydiff-delay 0.1)
  :init
  (setq diff-hl-show-hunk-map (make-sparse-keymap)
        diff-hl-inline-popup-transient-mode-map (make-sparse-keymap))
  :hook
  (window-setup . diff-hl-flydiff-mode)
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package writeroom-mode
  :custom
  (writeroom-fullscreen-effect 'maximized)
  (writeroom-restore-window-config t)
  (writeroom-mode-line t)
  (writeroom-width 100))

(use-package vterm)

(defun +vterm-run-command (command &rest args)
  "Open vterm and run COMMAND.
The optional ARGS are keyword arguments."
  (interactive "sEnter command: ")
  (let* ((buffer-name (or (plist-get args :title) "*vterm*"))
         (buffer (vterm buffer-name)))
    (vterm-send-string (format "exec %s" command))
    (vterm-send-return)
    buffer))

(use-package multi-vterm :after vterm
  :init
  (defun +vterm-custom-multi-vterm ()
    "Create new vterm buffer."
    (interactive)
    (let* ((vterm-buffer (multi-vterm-get-buffer)))
      (setq multi-vterm-buffer-list (nconc multi-vterm-buffer-list (list vterm-buffer)))
      (set-buffer vterm-buffer)
      (multi-vterm-internal)
      (switch-to-buffer-other-window vterm-buffer))))

(use-package project
  :custom
  (project-vc-extra-root-markers '(".dir-locals.el"))
  (project-vc-include-untracked t)
  (project-vc-merge-submodules nil))

(use-package ibuffer-project
  :preface
  (defun +ibuffer-list ()
    (interactive)
    (ibuffer nil nil nil t nil nil nil))
  :hook
  (ibuffer . (lambda ()
               (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
               (unless (eq ibuffer-sorting-mode 'project-file-relative)
                 (ibuffer-do-sort-by-project-file-relative)))))


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

(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  :config
  (gptel-api-key-from-auth-source))

(use-package consult
  :custom
  (consult-preview-key nil)
  (consult-narrow-key "<")
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  ;; Disable preview for consult-grep commands
  (consult-customize
   consult-ripgrep
   consult-git-grep
   consult-buffer
   consult-project-buffer
   consult-grep
   consult-recent-file
   :preview-key nil)
  :hook
  (completion-list-mode . consult-preview-at-point-mode))

(use-package consult-dir :after consult)

(use-package affe
  :custom
  (affe-find-command "rg --color=never --no-ignore --files --hidden --glob=!.git/*")
  (affe-regexp-compiler #'affe-orderless-regexp-compiler)
  :init
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (cdr (orderless-compile input)))
    (cons input (apply-partially #'orderless--highlight input t)))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize
   affe-grep
   :preview-key nil))

(use-package embark
  :custom
  (embark-indicators '(+vertico-embark-which-key-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  :custom-face
  (embark-selected
   ((nil :inherit unspecified
         :foreground ,(doom-color 'magenta))))
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  :config
  (defvar +vertico-embark-prompter-map (make-sparse-keymap)
    "Embark completion read prompter map.")

  (defmacro +vertico-make-embark-ace-action (fn)
    `(defun ,(intern (concat "+vertico-embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "%s"
         (require 'ace-window)
         (let ((aw-dispatch-always t))
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn))))))

  (defun +vertico-embark-which-key-indicator ()
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

  (defun +vertico-embark-act-with-completing-read (&optional args)
    "Display embark actions in the minibuffer.
Passes on ARGS to `embark-act`"
    (interactive "P")
    (let* ((embark-prompter 'embark-completing-read-prompter)
           (act (propertize "Act" 'face 'highlight))
           (embark-indicator (lambda (_keymap targets) nil)))
      (embark-act args)))

  (advice-add 'embark-completing-read-prompter
              :around (util/with-minibuffer-keymap
                       +vertico-embark-prompter-map))

  (+vertico-make-embark-ace-action find-file)
  (+vertico-make-embark-ace-action affe-find)
  (+vertico-make-embark-ace-action switch-to-buffer)
  (+vertico-make-embark-ace-action bookmark-jump)
  (defun +vertico-embark-hide-which-key-indicator (fn &rest args)
    "Hide the `which-key` indicator after using the embark prompter.
Executes FN with ARGS."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'+vertico-embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'+vertico-embark-hide-which-key-indicator))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless partial-completion basic))
  ;; (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides nil)
  (completion-ignore-case t)
  (orderless-smart-case nil)
  ;; (completion-category-overrides '((file (styles basic partial-completion))))
  :init
  (setq completion-category-defaults nil)
  (defun +consult-orderless-regexp-compiler (input type &rest _config)
    (setq input (cdr (orderless-compile input)))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input t str))))

  ;; OPTION 1: Activate globally for all consult-grep/ripgrep/find/...
  ;; (setq consult--regexp-compiler #'+consult-orderless-regexp-compiler)

  ;; OPTION 2: Activate only for some commands, e.g., consult-ripgrep!
  (defun +consult-with-orderless (&rest args)
    (minibuffer-with-setup-hook
        (lambda ()
          (setq-local consult--regexp-compiler #'+consult-orderless-regexp-compiler))
      (apply args)))
  (advice-add #'consult-ripgrep :around #'+consult-with-orderless)

  (keymap-substitute project-prefix-map #'project-find-regexp #'consult-ripgrep)
  (cl-nsubstitute-if
   '(consult-ripgrep "Find regexp")
   (pcase-lambda (`(,cmd _)) (eq cmd #'project-find-regexp))
   project-switch-commands))

(defun +clear-project ()
  "Clear project and reset windows."
  (interactive)
  (project-kill-buffers t)
  (delete-other-windows)
  (dashboard-open))

(defun +clear-all-windows()
  "Clear Tab."
  (interactive)
  (delete-other-windows)
  (dashboard-open))

(defun +create-new-tab ()
  "Create new tab."
  (interactive)
  (tab-bar-new-tab)
  (dashboard-open))

(defun +activities-new-project ()
  "Create new activity with project."
  (interactive)
  (+create-new-tab)
  (condition-case err
      (progn
        (let ((default-directory "~/"))
          (call-interactively #'project-switch-project)
          (call-interactively #'activities-define)
          (treemacs)))
    ((error quit)
     (tab-bar-close-tab))))

(use-package ess)

(use-package helpful)

(use-package impatient-mode)

(use-package prettier-js)

(use-package rfc-mode
  :custom-face
  (rfc-mode-browser-status-face
   ((nil :inherit font-lock-string-face)))
  (rfc-mode-browser-ref-face
   ((nil :inherit font-lock-operator-face)))
  (rfc-mode-browser-title-face
   ((nil :inherit font-lock-operator-face))))

(use-package rainbow-mode
  :defer t
  :custom
  (rainbow-r-colors-alist '())
  (rainbow-html-colors-alist '()))

(use-package leetcode
  :defer t
  :custom
  (leetcode-prefer-language "golang")
  (leetcode-prefer-sql "mysql")
  (leetcode-save-solutions t)
  :config
  (defun +leetcode--solving-window-layout-override ()
    (delete-other-windows)
    (setq leetcode--description-window (selected-window))
    (setq leetcode--code-window (split-root-window-right))
    (setq leetcode--testcase-window (split-window-below))
    (other-window 1)
    (setq leetcode--result-window (split-window-below))
    (select-window leetcode--code-window))

  (defun +leetcode--display-result-override (buffer &optional alist)
    (set-window-buffer leetcode--result-window buffer)
    leetcode--result-window)

  (defun +leetcode--display-testcase-override (buffer &optional alist)
    (set-window-buffer leetcode--testcase-window buffer)
    leetcode--testcase-window)

  (defun +leetcode--display-detail-override (buffer &optional alist)
    (set-window-buffer leetcode--description-window buffer)
    leetcode--description-window)

  (defun +leetcode--display-code-override (buffer &optional alist)
    (set-window-buffer leetcode--code-window buffer)
    leetcode--code-window)

  (advice-add #'leetcode--solving-window-layout :override #'+leetcode--solving-window-layout-override)
  (advice-add #'leetcode--display-result :override #'+leetcode--display-result-override)
  (advice-add #'leetcode--display-testcase :override #'+leetcode--display-testcase-override)
  (advice-add #'leetcode--display-detail :override #'+leetcode--display-detail-override)
  (advice-add #'leetcode--display-code :override #'+leetcode--display-code-override)
  :hook
  (leetcode-solution-mode . (lambda () (eglot--managed-mode -1))))

(use-package exercism :disabled
  :defer t
  :custom
  (exercism-enable-log-to-message-buffer nil)
  (exercism-open-url-on-submit nil)
  woman :hook
  (after-init . exercism-setup))

(use-package shr
  :defer t
  :custom
  (shr-image-animate nil)
  (shr-use-fonts nil)
  (shr-bullet "• ")
  (shr-hr-line "—")
  (shr-indentation 2)
  (shr-max-width 100))

(use-package devdocs
  :defer t)

(use-package man
  :defer t
  :custom
  (Man-width-max nil))

(use-package emacsql
  :hook
  (after-init . emacsql-fix-vector-indentation))

(use-package compile
  :custom
  (compilation-ask-about-save nil)
  :config
  (defun +colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point)))
  :hook
  (compilation-filter . +colorize-compilation-buffer))

(use-package rmsbolt
  :config
  (defun +rmsbolt-quit ()
    (interactive)
    (rmsbolt-mode -1)
    (if-let* ((buffer (get-buffer rmsbolt-output-buffer))
              (window (get-buffer-window buffer)))
        (quit-window t (get-buffer-window (get-buffer rmsbolt-output-buffer)))
      )
    ;; HACK: relies on rmsbolt closing assembly windows and focusing source window
    (window-state-put +rmsbolt-layout-state (frame-root-window) 'safe))

  (defun +rmsbolt-start ()
    (interactive)
    (unless (rmsbolt--get-lang)
      (user-error "rmsbolt unsupported language"))
    (setq-local +rmsbolt-layout-state (window-state-get (frame-root-window) t))
    (delete-other-windows)
    (rmsbolt))

  (defun +rmsbolt-toggle (&optional arg)
    (interactive "p")
    (pcase arg
      (4 (+rmsbolt-quit))
      (_ (if rmsbolt-mode
             (+rmsbolt-quit)
           (+rmsbolt-start)))
      )))

(use-package two-column
  :custom
  (2C-beyond-fill-column 4)
  (2C-window-width 120)
  (2C-mode-line-format ("%e" (:eval (doom-modeline-format--+default-modeline)))))

(provide 'packages-tools)

;;; packages-tools.el ends here
