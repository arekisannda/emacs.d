;;; ui/treemacs.el -*- lexical-binding: t; -*-

(use-package treemacs
  :custom
  (treemacs-user-mode-line-format nil)
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
  (treemacs-window-background-face
   ((nil :background ,(doom-color 'bg-alt))))
  (treemacs-hl-line-face
   ((nil :background ,(doom-color 'bg))))
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
  (kill-emacs                . +treemacs--clean-workspaces)
  (treemacs-switch-workspace . +treemacs--clean-workspaces)
  (treemacs-mode             . +treemacs--setup))

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
  (defun treemacs-project-directory-override-next-command (dir)
    (interactive
     (let* ((treemacs-projects (treemacs-workspace->projects (treemacs-current-workspace)))
            (projects-table (make-hash-table :test #'equal))
            (_ (mapc (lambda (cand) (puthash (treemacs-project->name cand)
                                             `(:path ,(treemacs-project->path cand))
                                             projects-table))
                     treemacs-projects))
            (selected (completing-read "Treemacs project: " projects-table (-const t) t)))
       (list (plist-get (gethash selected projects-table) :path)))
     )
    (let ((default-directory (file-name-as-directory dir)))
      (let* ((keys (read-key-sequence "[temporary-default-directory]-"))
             (cmd (key-binding keys)))
        (unless (commandp cmd)
          (user-error "Not a command"))
        (call-interactively cmd))
      ))
  )
