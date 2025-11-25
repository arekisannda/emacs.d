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

(use-package treemacs-tab-bar :after treemacs
  :config (treemacs-set-scope-type 'Tabs))

(use-package diff-hl :after (magit)
  :custom
  (diff-hl-flydiff-delay 0.1)
  :init
  (setq diff-hl-show-hunk-map (make-sparse-keymap)
        diff-hl-inline-popup-transient-mode-map (make-sparse-keymap))
  :config
  (advice-add #'diff-hl-show-hunk
              :before
              (lambda (&optional _)
                (let ((inhibit-message t))
                  (cond
                   ((derived-mode-p 'org-mode) (org-fold-show-all))
                   ((derived-mode-p 'prog-mode) (funcall util/fold-show-all))
                   ))))

  (defun frame-live-visible-p (frame)
    (and frame (frame-live-p frame) (frame-visible-p frame)))

  (defun diff-hl-posframe-scroll-up (arg)
    "Scroll up ARG lines in the childframe."
    (interactive "p")
    (when diff-hl-show-hunk--frame
      (with-selected-frame diff-hl-show-hunk--frame
        (windex-scroll-up))))

  (defun diff-hl-posframe-scroll-down (arg)
    "Scroll down ARG lines in the childframe."
    (interactive "p")
    (when diff-hl-show-hunk--frame
      (with-selected-frame diff-hl-show-hunk--frame
        (windex-scroll-down))))
  :hook
  (window-setup . diff-hl-flydiff-mode)
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package diff-hl-show-hunk-posframe :after (diff-hl posframe)
  :custom
  (diff-hl-show-hunk-function #'diff-hl-show-hunk-posframe)
  (diff-hl-show-hunk-posframe-show-header-line nil)
  (diff-hl-show-hunk-posframe-internal-border-width 1)
  (diff-hl-show-hunk-posframe-poshandler nil)
  (diff-hl-show-hunk-posframe-parameters nil)
  (diff-hl-show-hunk-posframe-internal-border-color (face-attribute 'popup-border :background nil t))
  (diff-hl-show-staged-changes nil)
  :config
  (defun +diff-hl-show-hunk--posframe-hide ()
    "Hide the posframe and clean up buffer."
    (interactive)
    (diff-hl-show-hunk-posframe--transient-mode -1)
    (when (frame-live-p diff-hl-show-hunk--frame)
      (make-frame-invisible diff-hl-show-hunk--frame t)))

  (advice-add #'diff-hl-show-hunk--posframe-hide :override #'+diff-hl-show-hunk--posframe-hide)

  (defun +diff-hl-show-hunk-hide ()
    "Hide the current shown hunk."
    (interactive)
    (diff-hl-show-hunk--posframe-hide))

  (advice-add #'diff-hl-show-hunk-hide :override #'+diff-hl-show-hunk-hide)

  (defun +diff-hl-show-hunk-posframe (buffer &optional _line)
    "Implementation to show the hunk in a posframe."
    (save-excursion

      (unless (require 'posframe nil t)
        (user-error
         (concat
          "`diff-hl-show-hunk-posframe' requires the `posframe' package."
          "  Please install it or customize `diff-hl-show-hunk-function'.")))

      (unless (posframe-workable-p)
        (user-error
         "Package `posframe' is not workable.  Please customize diff-hl-show-hunk-function"))

      (diff-hl-show-hunk--posframe-hide)
      (setq diff-hl-show-hunk--hide-function #'diff-hl-show-hunk--posframe-hide)

      ;; put an overlay to override read-only-mode keymap
      (with-current-buffer buffer
        ;; Change face size
        (buffer-face-set 'diff-hl-show-hunk-posframe)

        (let ((full-overlay (make-overlay 1 (1+ (buffer-size)))))
          (overlay-put full-overlay
                       'keymap diff-hl-show-hunk-posframe--transient-mode-map)))

      (setq posframe-mouse-banish nil)
      (setq diff-hl-show-hunk--original-frame last-event-frame)
      (move-beginning-of-line 1)

      (let* ((hunk-overlay diff-hl-show-hunk--original-overlay)
             (width (let ((edges (window-edges (selected-window))))
                      (- (nth 2 edges) (nth 0 edges) 10))))
        (setq
         diff-hl-show-hunk--frame
         (posframe-show buffer
                        :poshandler #'posframe-poshandler-point-1
                        :internal-border-width diff-hl-show-hunk-posframe-internal-border-width
                        :internal-border-color diff-hl-show-hunk-posframe-internal-border-color
                        :hidehandler nil
                        :min-height (when diff-hl-show-hunk-posframe-show-header-line 10)
                        :min-width width
                        :max-height 30
                        :max-width width
                        :respect-header-line diff-hl-show-hunk-posframe-show-header-line
                        :respect-tab-line nil
                        :respect-mode-line nil
                        :override-parameters diff-hl-show-hunk-posframe-parameters)
         ))

      (with-selected-frame diff-hl-show-hunk--frame
        (with-current-buffer buffer
          (diff-hl-show-hunk-posframe--transient-mode 1)
          (when diff-hl-show-hunk-posframe-show-header-line
            (setq header-line-format (diff-hl-show-hunk-posframe--header-line)))
          (goto-char (point-min))
          (setq buffer-quit-function #'diff-hl-show-hunk--posframe-hide)
          (select-window (window-main-window diff-hl-show-hunk--frame))

          ;; Make cursor visible (mainly for selecting text in posframe)
          (setq cursor-type 'box)

          ;; Recenter around point
          (recenter)
          ))))

  (advice-add #'diff-hl-show-hunk-posframe :override #'+diff-hl-show-hunk-posframe)

  (defun +diff-hl-show-hunk-previous ()
    "Go to previous hunk/change and show it."
    (interactive)
    (let* ((point (when diff-hl-show-hunk--original-overlay
                    (overlay-start diff-hl-show-hunk--original-overlay)))
           (previous-overlay (diff-hl-show-hunk--next-hunk t point)))
      (if (not previous-overlay)
          (message "There is no previous change")
        (diff-hl-show-hunk-hide)
        (diff-hl-show-hunk--goto-hunk-overlay previous-overlay)
        (recenter)
        (move-beginning-of-line 1)
        (diff-hl-show-hunk))))

  (advice-add #'diff-hl-show-hunk-previous :override #'+diff-hl-show-hunk-previous)

  (defun +diff-hl-show-hunk-next ()
    "Go to next hunk/change and show it."
    (interactive)
    (let* ((point (when diff-hl-show-hunk--original-overlay
                    (overlay-start diff-hl-show-hunk--original-overlay)))
           (next-overlay (diff-hl-show-hunk--next-hunk nil point)))
      (if (not next-overlay)
          (message "There is no next change")
        (diff-hl-show-hunk-hide)
        (diff-hl-show-hunk--goto-hunk-overlay next-overlay)
        (recenter)
        (move-beginning-of-line 1)
        (diff-hl-show-hunk))))

  (advice-add #'diff-hl-show-hunk-next :override #'+diff-hl-show-hunk-next))

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

(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  :config
  (gptel-api-key-from-auth-source)
  :hook
  (gptel-mode . visual-fill-column-mode--disable))

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

(use-package embark-consult :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
