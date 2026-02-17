;;; tools/magit.el -*- lexical-binding: t; -*-

(use-package magit
  :custom
  (magit-commit-diff-inhibit-same-window t)
  (magit-save-repository-buffers 'dontask)
  (magit-commit-show-diff nil)
  (magit-branch-direct-configure nil)
  (magit-refresh-status-buffer nil)
  :custom-face
  (hl-line
   ((nil :background unspecified)))
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

  (magit-diff-context
   ((nil :weight regular
         :inherit diff-context
         :foreground unspecified
         :background ,(doom-color 'bg-alt))))

  (magit-diff-context-highlight
   ((nil :weight regular
         :inherit diff-context
         :foreground unspecified
         :background ,(doom-color 'bg-alt))))

  (magit-diff-added
   ((nil :weight regular
         :inherit diff-refine-added
         :foreground unspecified
         :background unspecified)))
  (magit-diff-added-highlight
   ((nil :weight regular
         :inherit diff-refine-added
         :foreground unspecified
         :background unspecified)))

  (magit-diff-removed
   ((nil :weight regular
         :inherit diff-refine-removed
         :foreground unspecified
         :background unspecified)))
  (magit-diff-removed-highlight
   ((nil :weight regular
         :inherit diff-refine-removed
         :foreground unspecified
         :background unspecified)))

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

(use-package pr-review
  :custom
  (pr-review-fringe-icons nil)
  (pr-review-section-indent-width 4)
  :custom-face
  :config
  (defun pr-review-at-point (&optional )
    (interactive)
    (pr-review (forge-get-url (forge-current-pullreq))))
  )
