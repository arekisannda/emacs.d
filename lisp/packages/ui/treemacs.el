;;; ui/treemacs.el -*- lexical-binding: t; -*-

(require 'util-windows)

(use-package treemacs
  :demand t
  :custom
  (treemacs-user-header-line-format '("%e" (:eval (when (and tab-bar-mode (activities-current))
                                                    (concat
                                                     (propertize (propertize " " 'display `(space :width 1)))
                                                     (format "%s" (cdr (assq 'name (tab-bar--current-tab)))))))))
  (treemacs-user-mode-line-format '("%e" (:eval (doom-modeline-format--+treemacs-modeline))))
  (treemacs-is-never-other-window t)
  (treemacs-display-in-side-window t)
  (treemacs-position 'left)
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
  (treemacs-nerd-icons-root-face
   ((nil :height 1.0)))
  (treemacs-root-face
   ((nil :height 1.1 :weight normal :foreground ,(doom-color 'violet))))
  (treemacs-window-background-face
   ((nil :background ,(doom-color 'bg-alt))))
  (treemacs-hl-line-face
   ((nil :background ,(doom-color 'bg))))
  (treemacs-fringe-indicator-face
   ((nil :foreground unspecified)))
  (treemacs-peek-mode-indicator-face
   ((nil :background ,(doom-color 'green))))
  :config
  (defun +treemacs-add-project-to-workspace (dir)
    (interactive (list (funcall project-prompter)))
    (treemacs-add-project-to-workspace dir))

  (defun +treemacs--clean-workspaces ()
    "Remove non default and `activities' tabs."
    (interactive)
    (require 'treemacs)
    (mapc (lambda (workspace)
            (when (and (not (string= workspace "Default"))
                       (not (string-match-p "^Tab @" workspace)))
              (treemacs-do-remove-workspace workspace nil)))
          (mapcar #'treemacs-workspace->name treemacs--workspaces)))

  (defun +treemacs--popup-window-override ()
    "Pop up a side window and buffer for treemacs."
    (let ((buf (treemacs-get-local-buffer-create)))
      (util/windows-display-buffer-in-side-window
       buf nil
       `( :side left
          :slot 0
          :dedicated t
          :size ,util/windows-min-left-width
          :fixed width))
      (select-window (get-buffer-window buf))))

  (advice-add #'treemacs--popup-window :override #'+treemacs--popup-window-override)

  (defun +treemacs--select-window-guard (orig-fn &rest r)
    (when (not (seq-some
                (lambda (p) (funcall p))
                '((lambda () (frame-parameter nil '+side-frame)))))
      (apply orig-fn r)))

  (advice-add #'treemacs-select-window :around #'+treemacs--select-window-guard)

  (defun +treemacs--setup ()
    (treemacs-autohide-mode 1)
    (treemacs-filewatch-mode 1)
    (treemacs-fringe-indicator-mode 'only-when-focused)
    (face-remap-add-relative 'header-line
                             `(nil :height 1.1 :foreground ,(doom-color 'fg-alt)))
    (face-remap-add-relative 'mode-line-active
                             `(nil :inherit mode-line-active
                                   :foreground unspecified
                                   :background ,(doom-color 'bg-alt)))
    (face-remap-add-relative 'mode-line-inactive
                             `(nil :inherit mode-line-active
                                   :foreground unspecified
                                   :background ,(doom-color 'bg-alt))))
  :hook
  (kill-emacs                . +treemacs--clean-workspaces)
  (treemacs-switch-workspace . +treemacs--clean-workspaces)
  (treemacs-mode             . +treemacs--setup))

(use-package treemacs-workspaces :after treemacs)

(use-package treemacs-peek-mode :after treemacs
  :config
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

  (advice-add #'treemacs--setup-peek-buffer :override #'+treemacs--setup-peek-buffer))

(use-package treemacs-nerd-icons :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-evil :after (treemacs evil))

(use-package treemacs-tab-bar :after treemacs
  :config (treemacs-set-scope-type 'Tabs))

(with-eval-after-load 'treemacs
  (defcustom treemacs-autohide-threshold 460
    "Auto hide `treemacs` when `frame-width` is below threshold."
    :group 'treemacs
    :type 'integer)

  (defun treemacs-autohide-frame-size-changed-p (frame)
    (let ((new-size (cons (frame-width) (frame-height)))
          (previous-size (frame-parameter frame 'treemacs-frame-size)))
      (cond ((null previous-size)
             (set-frame-parameter frame 'treemacs-frame-size new-size)
             nil)
            ((not (equal previous-size new-size))
             (set-frame-parameter frame 'treemacs-frame-size new-size)
             t))))

  (defun treemacs-autohide-set-threshold (threshold)
    "Set `treemacs-autohide-threshold' to THRESHOLD."
    (interactive
     (list (read-number
            (format "Set `treemacs-autohide-threshold' (current width: %d): "
                    (frame-width (selected-frame)))
            treemacs-autohide-threshold)))
    (setq treemacs-autohide-threshold threshold)
    (treemacs-autohide-on-size-change (selected-frame) 'force))

  (defun treemacs-autohide--show ()
    (let ((visibility (treemacs-current-visibility)))
      (pcase visibility
        ('visible nil)
        ('exists (display-buffer (treemacs-get-local-buffer-create)))
        ('none (if-let ((buffer (treemacs-get-local-buffer)))
                        (display-buffer buffer)))
        )))

  (defun treemacs-autohide--hide ()
    (let ((visibility (treemacs-current-visibility)))
      (when (eq visibility 'visible)
        (delete-window (treemacs-get-local-window)))
      ))

  (defun treemacs-autohide-on-size-change (&optional frame forcep)
    (with-selected-frame frame
      (when (or (treemacs-autohide-frame-size-changed-p frame) forcep)
        (if (< (frame-width) treemacs-autohide-threshold)
            (treemacs-autohide--hide)
          (treemacs-autohide--show)
          ))
      ))

  (defun treemacs-autohide-defocus (frame)
    (with-selected-frame frame
      (let ((window (frame-selected-window frame)))
        (unless (or (minibuffer-window-active-p window)
                    (window-with-parameter 'window-side 'bottom frame))
          (when (and (< (frame-width) treemacs-autohide-threshold)
                     (not (eq window (treemacs-get-local-window)))
                     (eq 'visible (treemacs-current-visibility)))
            (delete-window (treemacs-get-local-window))))
        )))

  (defun treemacs-autohide-enable ()
    (add-hook 'window-size-change-functions #'treemacs-autohide-on-size-change)
    (add-hook 'window-selection-change-functions #'treemacs-autohide-defocus)
    (treemacs-autohide-on-size-change (selected-frame) 'force))

  (defun treemacs-autohide-disable ()
    (remove-hook 'window-size-change-functions #'treemacs-autohide-on-size-change)
    (remove-hook 'window-selection-change-functions #'treemacs-autohide-defocus)
    (with-selected-frame (selected-frame)
      (treemacs-autohide--show)))

  (define-minor-mode treemacs-autohide-mode
    "Toggle `treemacs` autohide."
    :global t
    :lighter nil
    (if treemacs-autohide-mode
        (treemacs-autohide-enable)
      (treemacs-autohide-disable)))
  )

(with-eval-after-load 'treemacs-workspaces
  (defun treemacs-project-directory-prompt ()
    (let* ((treemacs-projects (treemacs-workspace->projects (treemacs-current-workspace)))
           (projects-table (make-hash-table :test #'equal))
           (_ (mapc (lambda (cand) (puthash (treemacs-project->name cand)
                                            `(:path ,(treemacs-project->path cand))
                                            projects-table))
                    treemacs-projects))
           (selected (completing-read "Treemacs project: " projects-table (-const t) t)))
      (plist-get (gethash selected projects-table) :path)))

  (defun treemacs-project-directory-override-next-command (dir)
    (interactive
     (list (treemacs-project-directory-prompt)))
    (let ((default-directory (file-name-as-directory dir)))
      (let* ((keys (read-key-sequence "[temporary-default-directory]-"))
             (cmd (key-binding keys)))
        (unless (commandp cmd)
          (user-error "Not a command"))
        (call-interactively cmd))
      ))

  (defun treemacs-directory-override-next-command (dir)
    (interactive
     (list (read-directory-name "Select directory: " default-directory nil t)))
    (let ((default-directory (file-name-as-directory dir)))
      (let* ((keys (read-key-sequence "[temporary-default-directory]-"))
             (cmd (key-binding keys)))
        (unless (commandp cmd)
          (user-error "Not a command"))
        (call-interactively cmd))
      ))

  (defun treemacs-finish-edit ()
    "Finish editing your workspaces and apply the change."
    (interactive)
    (treemacs-block
     (treemacs-error-return-if (not (equal (buffer-name) treemacs--org-edit-buffer-name))
       "This is not a valid treemacs workspace edit buffer")
     (treemacs--org-edit-remove-validation-msg)
     (widen)
     (whitespace-cleanup)
     (-let [lines (treemacs--read-persist-lines (buffer-string))]
       (treemacs-error-return-if (null (buffer-string))
         "The buffer is empty, there is nothing here to save.")
       (pcase (treemacs--validate-persist-lines lines)
         (`(error ,err-line ,err-msg)
          (treemacs--org-edit-display-validation-msg err-msg err-line))
         ('success
          (treemacs--invalidate-buffer-project-cache)
          (write-region
           (apply #'concat (--map (concat it "\n") lines))
           nil
           treemacs-persist-file
           nil :silent)
          (treemacs--restore)
          (-if-let (ws (treemacs--find-workspace-by-name
                        (treemacs-workspace->name (treemacs-current-workspace))))
              (setf (treemacs-current-workspace) ws)
            (treemacs--find-workspace))
          (treemacs--consolidate-projects)
          (quit-window)
          (run-hooks 'treemacs-workspace-edit-hook)
          (when treemacs-hide-gitignored-files-mode
            (treemacs--prefetch-gitignore-cache 'all))
          (treemacs-log "Edit completed successfully."))))))
  )
