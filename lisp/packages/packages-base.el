;;; packages-base.el --- Base Packages Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(defun +window-popup-fit-window-to-buffer (window &rest _)
  (fit-window-to-buffer window 20 1))

(use-package aio)

(use-package shut-up)

(use-package no-littering)

(use-package diminish)

(use-package undo-fu)

(use-package general)

(use-package hydra)

(use-package compat)

(use-package persist)

(use-package ov)

(setq-default evil-respect-visual-line-mode t
              evil-want-keybinding nil
              evil-want-minibuffer nil)

(defmacro evil-move-or-goto-line-around (dir)
  `(lambda (fn &optional arg)
     (let ((inhibit-message t))
       (if arg
           (goto-line (,(if dir '+ '-) (line-number-at-pos) arg))
         (apply fn arg)))))

(defun evil-window-move-ignore-params (fn &optional arg)
  (let ((ignore-window-parameters t)) (funcall fn arg)))

(use-package evil :after undo-fu
  :custom
  (evil-want-integration t)
  (evil-default-state 'normal)
  (evil-undo-system 'undo-fu)
  (evil-split-window-below nil)
  (evil-vsplit-window-right nil)
  :config
  (setq evil-insert-state-modes
        '(comint-mode erc-mode geiser-repl-mode gud-mode inferior-apl-mode inferior-caml-mode
                      inferior-emacs-lisp-mode inferior-j-mode inferior-python-mode inferior-scheme-mode inferior-sml-mode
                      internal-ange-ftp-mode haskell-interactive-mode prolog-inferior-mode racket-repl-mode reb-mode shell-mode
                      slime-repl-mode term-mode utop-mode wdired-mode))

  (setq evil-emacs-state-modes
        (delete-dups
         (append '(vterm-mode
                   eshell-mode
                   special-mode
                   dap-ui-breakpoints-ui-list-mode
                   dape-repl-mode
                   calc-mode
                   comint-mode
                   calculator-mode
                   calendar-mode
                   eglot-list-connections-mode
                   inferior-python-mode)
                 evil-emacs-state-modes)))

  (setq evil-motion-state-modes
        '(apropos-mode
          color-theme-mode
          command-history-mode
          messages-buffer-mode
          backtrace-mode
          compilation-mode
          dictionary-mode
          ert-results-mode
          help-mode
          helpful-mode
          Info-mode
          devdocs-mode
          Man-mode
          speedbar-mode
          embark-collect-mode
          undo-tree-visualizer-mode
          rfc-mode
          woman-mode))

  (advice-add #'evil-next-line :around (evil-move-or-goto-line-around t))
  (advice-add #'evil-previous-line :around (evil-move-or-goto-line-around nil))
  (advice-add #'evil-next-visual-line :around (evil-move-or-goto-line-around t))
  (advice-add #'evil-previous-visual-line :around (evil-move-or-goto-line-around nil))
  :hook
  (after-init . evil-mode))

(use-package evil-collection :after evil
  :custom
  (evil-collection-mode-list
   '(info
     dired
     ibuffer
     magit
     forge
     edebug
     org
     org-roam
     ediff))
  :diminish evil-collection-unimpaired-mode
  :init
  (setq forge-add-default-bindings nil)
  :hook
  (evil-mode . evil-collection-init))

(use-package evil-nerd-commenter :after evil)

(use-package evil-args :after evil)

(use-package evil-snipe :after evil
  :diminish evil-snipe-mode
  :custom
  (evil-snipe-enable-highlight t)
  :hook
  (evil-mode . evil-snipe-mode))

(use-package evil-easymotion :after evil)

(use-package evil-matchit :after evil
  :hook
  (evil-mode . global-evil-matchit-mode))

(use-package evil-mc :after evil
  :init
  (setq evil-mc-cursors-map (make-sparse-keymap)
        evil-mc-key-map (make-sparse-keymap))
  :hook
  (evil-mode . global-evil-mc-mode))

(use-package embrace
  :custom-face
  (embrace-help-separator-face
   ((nil :inherit font-lock-comment-face)))
  (embrace-help-key-face
   ((nil :inherit font-lock-function-name-face)))
  (embrace-help-mark-func-face
   ((nil :inherit font-lock-constant-face)))
  (embrace-help-pair-face
   ((nil :inherit nil
         :foreground  ,(doom-color 'blue)
         :inverse-video nil)))
  :init
  (setq embrace-help-separator " : ")
  (setq embrace--help-add-column-width 2)
  (setq embrace-show-help-p t)
  :config
  (setq embrace--help-buffer-name " *embrace-help*")
  (defun +embrace--show-help-buffer (help-string)
    (let ((alist '((window-width  . #'+window-popup-fit-window-to-buffer)
                   (window-height . #'+window-popup-fit-window-to-buffer)
                   (window-popup  . bottom)
                   (dedicated . t))))
      (embrace--setup-help-buffer)
      (with-current-buffer embrace--help-buffer
        (erase-buffer)
        (insert help-string)
        (goto-char (point-min)))
      (if (get-buffer-window embrace--help-buffer)
          (display-buffer-reuse-window embrace--help-buffer alist)
        (let ((w (split-window (frame-root-window nil) nil nil)))
          (window--display-buffer embrace--help-buffer w 'window alist)
          (fit-window-to-buffer w))
        )))

  (advice-add #'embrace--show-help-buffer :override #'+embrace--show-help-buffer))

(use-package editorconfig
  :config
  (setq editorconfig-lisp-use-default-indent t)
  (editorconfig-mode t)
  :diminish editorconfig-mode)

(defvar-local +envrc-update-hook '()
  "Buffer-local hook to run after `envrc--update'.")

(defun +envrc--update-after-setup ()
  "Setup to run after `envrc--update'."
  (run-hooks '+envrc-update-hook))

(use-package envrc
  :custom
  (envrc-show-summary-in-minibuffer nil)
  :hook
  (after-init . envrc-global-mode)
  :init
  (advice-add #'envrc--update :after #'+envrc--update-after-setup))

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "SauceCodePro Nerd Font Mono"))

(use-package posframe
  :custom
  (posframe-inhibit-double-buffering t)
  (posframe-mouse-banish-function #'posframe-mouse-banish-default)
  :config
  (defun +posframe-show-refresh (buffer &rest _)
    (posframe-refresh buffer))
  (advice-add #'posframe-show :after #'+posframe-show-refresh))

(use-package transient
  :custom
  (transient-show-popup t)
  (transient-display-buffer-action
   '(+display-buffer-in-pop-up-window
     (dedicated . t)))
  (transient-mode-line-format nil)
  (transient-force-fixed-pitch t))

(use-package which-key
  :custom
  (which-key-popup-type 'custom)
  (which-key-sort-order 'which-key-description-order)
  (which-key-show-prefix 'echo)
  (which-key-side-window-slot 0)
  (which-key-side-window-location 'bottom)
  (which-key-max-display-columns nil)
  (which-key-side-window-max-width 0)
  (which-key-min-column-description-width 30)
  (which-key-custom-hide-popup-function
   (lambda ()
     (when (buffer-live-p which-key--buffer)
       ;; in case which-key buffer was shown in an existing window, `quit-window'
       ;; will re-show the previous buffer, instead of closing the window
       (quit-windows-on which-key--buffer)
       (when (and which-key-preserve-window-configuration
                  which-key--saved-window-configuration)
         (set-window-configuration which-key--saved-window-configuration)
         (setq which-key--saved-window-configuration nil)))))

  (which-key-custom-show-popup-function #'+which-key--show-buffer-root-window)

  (which-key-custom-popup-max-dimensions-function
   (lambda (&optional _)
     (cons 30 (let ((edges (window-edges (frame-root-window))))
                (- (nth 2 edges) (nth 0 edges)))
           )))
  :config
  (defun +which-key--show-buffer-root-window (&optional _)
    (when (and which-key-preserve-window-configuration
               (not which-key--saved-window-configuration))
      (setq which-key--saved-window-configuration (current-window-configuration)))
    (let* ((alist `((window-width  . #'+window-popup-fit-window-to-buffer)
                    (window-height . #'+window-popup-fit-window-to-buffer)
                    (window-popup  . bottom)
                    (dedicated . t))))
      (cond
       ((eq which-key--multiple-locations t)
        (delete-windows-on which-key--buffer)
        (let ((w (split-window (frame-root-window nil) nil nil)))
          (window--display-buffer which-key--buffer w 'window alist)
          (+window-popup-fit-window-to-buffer w)))
       ((get-buffer-window which-key--buffer)
        (display-buffer-reuse-window which-key--buffer alist))
       (t
        (let ((w (split-window (frame-root-window nil) nil nil)))
          (window--display-buffer which-key--buffer w 'window alist)
          (+window-popup-fit-window-to-buffer w)))
       )))
  :hook
  (after-init . which-key-mode))

(use-package vertico
  :preface
  (defun +vertico-sort-directories-first (list)
    "Sort LIST by directories first."
    (setq list (vertico-sort-history-length-alpha list))
    (nconc (cl-loop for x in list if (string-suffix-p "/" x) collect x)
           (cl-loop for x in list if (not (string-suffix-p "/" x)) collect x)))

  (defun +vertico-sort-project (list)
    "Sort LIST by directories first."
    (setq list (vertico-sort-history-alpha list))
    (nconc (cl-loop for x in list if (not (string-suffix-p "/" x)) collect x)
           (cl-loop for x in list if (string-suffix-p "/" x) collect x)))

  (defun +vertico-add-options (command &rest options)
    `(,command posframe ,@options
               (vertico-posframe-fallback-mode . vertico-multiform-vertical)))
  :custom
  (minibuffer-prompt-properties
   '(read-only t
               cursor-intangible t
               face (:inherit minibuffer-prompt :weight bold :height 1.0)))
  (vertico-count-format
   `("%-6s " . ,(concat (nerd-icons-octicon "nf-oct-search")
                        " ( %s/%s )")))
  (vertico-count 20)
  (savehist-file (expand-file-name "var/savehist" user-emacs-directory))
  (vertico-multiform-commands
   `(,(+vertico-add-options #'project-switch-project
                            '(vertico-sort-override-function . +vertico-sort-project))
     ,(+vertico-add-options #'project-forget-project
                            '(vertico-sort-override-function . +vertico-sort-project))
     ,(+vertico-add-options #'project-forget-project-under
                            '(vertico-sort-override-function . +vertico-sort-directories-first))
     ,(+vertico-add-options #'project-kill-buffers
                            '(vertico-sort-override-function . +vertico-sort-directories-first))
     ,(+vertico-add-options #'project-find-file
                            '(vertico-sort-override-function . +vertico-sort-directories-first))
     ,(+vertico-add-options #'project-find-dir
                            '(vertico-sort-override-function . +vertico-sort-directories-first))

     ,(+vertico-add-options #'find-file
                            '(vertico-sort-override-function . +vertico-sort-directories-first))

     ,(+vertico-add-options #'rfc-mode-goto-section
                            '(minibuffer-default . nil)
                            '(vertico-sort-function . nil)
                            '(vertico-sort-override-function . nil))
     ,(+vertico-add-options #'activities-resume
                            '(vertico-sort-function . nil)
                            '(vertico-sort-override-function . nil))
     ,(+vertico-add-options #'activities-switch
                            '(vertico-sort-function . nil)
                            '(vertico-sort-override-function . nil))
     ;; default
     ,(+vertico-add-options t '(vertico-sort-override-function . nil))
     ))
  (vertico-multiform-categories
   `((symbol (vertico-sort-function . vertico-sort-history-length-alpha))
     (buffer (vertico-sort-function . vertico-sort-history-length-alpha))
     (file   (vertico-sort-function . +vertico-sort-directories-first))))
  (vertico-sort-function #'vertico-sort-history-length-alpha)
  (vertico-sort-override-function #'vertico-sort-history-length-alpha)
  :custom-face
  (vertico-default
   ((nil :inherit tooltip
         :background ,(doom-color 'bg-alt))))
  (vertico-current
   ((nil :inherit default
         :foreground ,(doom-color 'fg)
         :background ,(doom-color 'bg))))
  :config
  (advice-add
   #'vertico--format-candidate :around
   (lambda (orig-fun cand prefix suffix index start)
     (apply orig-fun (list cand
                           (if (= vertico--index index)
                               (concat (nerd-icons-faicon
                                        "nf-fa-angles_right"
                                        :face 'nerd-icons-red)
                                       "  " prefix)
                             (concat "   " prefix))
                           suffix
                           index start))))

  (advice-add #'vertico--resize-window :after #'vertico--no-truncate)
  (defun vertico--no-truncate (&rest _)
    (setq-local truncate-lines t))
  :hook
  (after-init . vertico-mode)
  (vertico-mode . vertico-multiform-mode)
  (vertico-mode . savehist-mode))

(use-package vertico-posframe :after vertico
  :custom
  (vertico-posframe-poshandler #'posframe-poshandler-frame-center)
  (vertico-posframe-border-width 1)
  (vertico-posframe-width 120)
  (vertico-posframe-parameters
   `((left-fringe . 8)
     (right-fringe . 8)))
  (vertico-posframe-size-function
   (lambda (buffer)
     (list
      :height (buffer-local-value 'vertico-posframe-height buffer)
      :width (buffer-local-value 'vertico-posframe-width buffer)
      :min-height 20
      :min-width 120
      :max-height 30
      :max-width 200
      :lines-truncate t
      )))
  :custom-face
  (vertico-posframe
   ((nil :inherit tooltip
         :foreground unspecified
         :background ,(doom-color 'bg-alt))))
  (vertico-posframe-border
   ((nil :inherit popup-border
         :background unspecified
         :foreground unspecified)))
  (vertico-posframe-border-2
   ((nil :inherit default
         :background ,(doom-color 'orange))))
  (vertico-posframe-border-3
   ((nil :inherit default
         :background ,(doom-color 'yellow))))
  (vertico-posframe-border-4
   ((nil :inherit default
         :background ,(doom-color 'base8))))
  (vertico-posframe-border-fallback
   ((nil :inherit default
         :background ,(doom-color 'vertical-bar))))
  :config
  (defun +vertico-posframe-show-cursor (buffer window-point)
    (with-current-buffer buffer
      (setq-local cursor-type 'box)
      (setq-local highlight-nonselected-windows t)
      (setq-local cursor-in-non-selected-windows 'box)
      (posframe-refresh buffer)))

  (advice-add #'vertico-posframe--show :after #'+vertico-posframe-show-cursor))

(use-package marginalia)

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

  (defun +consult--source-recentf-items ()
    (let ((ht (consult--buffer-file-hash))
          file-name-handler-alist ;; No Tramp slowdown please.
          items)
      (dolist (file recentf-list (nreverse items))
        ;; Emacs 29 abbreviates file paths by default, see
        ;; `recentf-filename-handlers'.
        (unless (eq (aref file 0) ?/)
          (setq file (expand-file-name file)))
        (unless (gethash file ht)
          (push (propertize
                 (file-name-nondirectory file)
                 'multi-category `(file . ,file))
                items)))))

  (plist-put consult--source-recent-file
             :items #'+consult--source-recentf-items)
  :hook
  (completion-list-mode . consult-preview-at-point-mode))

(use-package consult-dir :after consult)

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

(use-package dired
  :custom
  (dired-kill-when-opening-new-dired-buffer t))

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

(use-package magit
  :custom
  (magit-commit-diff-inhibit-same-window t)
  (magit-save-repository-buffers 'dontask)
  (magit-commit-show-diff nil)
  (magit-branch-direct-configure nil)
  (magit-refresh-status-buffer nil)
  :custom-face
  (hl-line
   ((nil
     :background ,(doom-color 'bg-alt))))
  (magit-header-line
   ((nil :weight bold
         :foreground ,(doom-color 'fg)
         :background ,(doom-color 'bg-alt)
         :box (:line-width (1 . 1) :color ,(doom-color 'bg-alt) :style nil))))
  (magit-diff-file-heading-selection
   ((nil :foreground ,(doom-color 'red)
         :background ,(doom-blend (doom-color 'dark-blue) (doom-color 'bg) 0.2))))
  (magit-diff-hunk-heading
   ((nil :foreground ,(doom-color 'violet)
         :background ,(doom-blend (doom-color 'violet) (doom-color 'bg) 0.1))))
  (magit-diff-hunk-heading-highlight
   ((nil :foreground ,(doom-color 'violet)
         :background ,(doom-blend (doom-color 'violet) (doom-color 'bg) 0.1))))
  (magit-diff-hunk-heading-selection
   ((nil :foreground ,(doom-color 'red))))
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

(use-package forge :after magit)

(provide 'packages-base)

;;; packages-base.el ends here
