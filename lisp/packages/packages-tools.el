;;; packages-tools.el --- Emacs Tool Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package treemacs
  :custom
  (treemacs-user-mode-line-format '((:eval (doom-modeline-format--+treemacs-modeline))))
  (treemacs-is-never-other-window t)
  (treemacs-display-in-side-window t)
  (treemacs-position 'left)
  (treemacs-width 35)
  (treemacs-RET-actions-config '((root-node-open . treemacs-toggle-node)
                                 (root-node-closed . treemacs-toggle-node)
                                 (dir-node-open . treemacs-toggle-node)
                                 (dir-node-closed . treemacs-toggle-node)
                                 (file-node-open . treemacs-visit-node-in-most-recently-used-window)
                                 (file-node-closed . treemacs-visit-node-in-most-recently-used-window)
                                 (tag-node-open . treemacs-toggle-node-prefer-tag-visit)
                                 (tag-node-closed . treemacs-toggle-node-prefer-tag-visit)
                                 (tag-node . treemacs-visit-node-in-most-recently-used-window)))
  :preface
  (defun +treemacs--clean-workspaces ()
    "Find top-level headings that are not 'Default' or don't match the pattern 'Tab :' and cut them."
    (interactive)
    (require 'treemacs)
    (mapc (lambda (workspace)
            (when (and (not (string= workspace "Default"))
                       (not (string-match-p "Tab @" workspace)))
              (treemacs-do-remove-workspace workspace nil)))
          (mapcar #'treemacs-workspace->name treemacs--workspaces)))

  (defun +treemacs--set-faces ()
    (setq-local doom-modeline-workspace-name t)
    (face-remap-add-relative 'doom-modeline-bar
                             :background (doom-color 'bg-alt)
                             :foreground (doom-color 'bg-alt))
    (face-remap-add-relative 'default :background (doom-color 'bg-alt))
    (face-remap-add-relative 'treemacs-hl-line-face :background (doom-color 'bg))
    (face-remap-add-relative 'treemacs-peek-mode-indicator-face :background (doom-color 'green))
    (face-remap-add-relative 'mode-line-inactive :background (doom-color 'bg-alt))
    (redraw-display))
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
    (treemacs-fringe-indicator-mode -1))
  :hook
  (kill-emacs . +treemacs--clean-workspaces)
  (treemacs-mode . +treemacs--set-faces)
  (treemacs-mode . +treemacs--setup))

(use-package treemacs-nerd-icons :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-evil :after treemacs)

(use-package treemacs-magit :after treemacs)

(use-package treemacs-tab-bar :after treemacs
  :config (treemacs-set-scope-type 'Tabs))

(use-package magit
  :custom
  (magit-repository-directories
   (list `(,(expand-file-name "~/Code/") . 1)))
  (magit-commit-diff-inhibit-same-window t)
  (magit-save-repository-buffers 'dontask)
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

(use-package ranger
  :hook
  (elpaca-after-init . ranger-override-dired-mode))

(use-package writeroom-mode
  :custom
  (writeroom-fullscreen-effect 'maximized)
  (writeroom-restore-window-config t)
  (writeroom-mode-line t)
  (writeroom-width 120))

(use-package vterm
  :elpaca (vterm :post-build
                 (progn
                   (setq vterm-always-compile-module t)
                   (require 'vterm)
                   ;;print compilation info for elpaca
                   (with-current-buffer (get-buffer-create vterm-install-buffer-name)
                     (goto-char (point-min))
                     (while (not (eobp))
                       (message "%S"
                                (buffer-substring (line-beginning-position)
                                                  (line-end-position)))
                       (forward-line)))
                   (when-let ((so (expand-file-name "./vterm-module.so"))
                              ((file-exists-p so)))
                     (make-symbolic-link
                      so (expand-file-name (file-name-nondirectory so)
                                           "../../builds/vterm")
                      'ok-if-already-exists)))))

(defun +vterm-run-command (command &rest args)
  "Open vterm and run COMMAND.
The optional ARGS are keyword arguments."
  (interactive "sEnter command: ")
  (let ((buffer-name (or (plist-get args :title) "*vterm*"))
        (buffer))
    (setq buffer (vterm buffer-name))
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
  :ensure nil
  :custom
  (project-vc-extra-root-markers '(".dir-locals.el"))
  (project-vc-include-untracked nil)
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

(use-package activities
  :custom
  (activities-name-prefix "@")
  :init
  (activities-mode)
  (activities-tabs-mode)
  (setf tab-bar-tab-face-function #'tab-bar-tab-face-default)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t))

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
  (affe-find-command "rg --color=never --files --hidden --glob=!.git/*"))

(use-package embark
  :custom
  (embark-indicators '(+vertico-embark-which-key-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
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
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
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

(defun +activites-new-project ()
  "Create new activity with project."
  (interactive)
  (+create-new-tab)
  (condition-case err
      (progn
        (call-interactively #'project-switch-project)
        (call-interactively #'activities-define)
        (treemacs))
    ((error quit)
     (tab-bar-close-tab))))

(provide 'packages-tools)

;;; packages-tools.el ends here
