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

(use-package code-review :after (magit forge)
  :custom-face
  (code-review-outdated-comment-heading
   ((nil :box nil)))
  (code-review-recent-comment-heading
   ((nil :box nil)))
  :custom
  (code-review-new-buffer-window-strategy #'pop-to-buffer-same-window)
  :config
  (set-keymap-parent code-review-mode-map magit-mode-map)

  (defun code-review-comment-commit ()
    "Commit comment."
    (interactive)
    (unwind-protect
        (let* ((buffer (get-buffer code-review-comment-buffer-name))
               (comment-text (string-trim
                              (with-current-buffer buffer
                                (save-excursion
                                  (buffer-substring-no-properties (point-min) (point-max))))))
               (pr (code-review-db-get-pullreq)))

          (quit-window t (get-buffer-window(current-buffer)))
          ;; (if (= 1 (length (window-list (window-frame (selected-window)))))
          ;;     (delete-frame (window-frame (selected-window)))
          ;;   (delete-window (selected-window)))

          (cond

           (code-review-comment-description?
            (oset pr raw-infos (-> (oref pr raw-infos)
                                   (a-assoc 'bodyText comment-text)
                                   (a-assoc 'bodyHTML nil)))
            (code-review-send-description
             pr
             (lambda ()
               (code-review-db-update pr)
               (code-review--build-buffer)
               (code-review-comment-reset-global-vars))))

           (code-review-comment-title?
            (oset pr title comment-text)
            (code-review-send-title
             pr
             (lambda ()
               (code-review-db-update pr)
               (code-review--build-buffer)
               (code-review-comment-reset-global-vars))))

           (code-review-comment-feedback?
            (let ((msg
                   (code-review-utils--comment-clean-msg
                    comment-text
                    code-review-comment-feedback-msg)))
              (code-review-db--pullreq-feedback-update msg)
              (code-review--build-buffer)
              (code-review-comment-reset-global-vars)))

           (code-review-promote-comment-to-issue?
            (progn
              (oset code-review-comment-uncommitted buffer-text comment-text)
              (code-review-comment-handler-commit
               code-review-comment-uncommitted
               code-review-comment-buffer-msg)
              (code-review-comment-reset-global-vars)))

           (code-review-comment-send?
            (progn
              (oset code-review-comment-uncommitted msg comment-text)
              (code-review-comment-handler-commit
               code-review-comment-uncommitted
               code-review-comment-single-comment-msg)
              (code-review-comment-reset-global-vars)))

           (code-review-comment-single-comment?
            (let ((msg
                   (code-review-utils--comment-clean-msg
                    comment-text
                    code-review-comment-single-comment-msg))
                  (callback (lambda (&rest _)
                              (let ((code-review-section-full-refresh? t))
                                (code-review--build-buffer)
                                (code-review-comment-reset-global-vars)))))
              (code-review-new-issue-comment pr msg callback)))
           (t
            (progn
              (oset code-review-comment-uncommitted msg comment-text)
              (code-review-comment-handler-commit
               code-review-comment-uncommitted
               (if code-review-comment-suggestion?
                   code-review-comment-suggestion-msg
                 code-review-comment-buffer-msg))
              (code-review-comment-reset-global-vars)))))))

  (defun code-review-comment-quit ()
    "Quit the comment window."
    (interactive)
    (quit-window t (get-buffer-window(current-buffer)))
    (with-current-buffer (get-buffer code-review-buffer-name)
      (goto-char code-review-comment-cursor-pos)
      (code-review-comment-reset-global-vars)))
  )
