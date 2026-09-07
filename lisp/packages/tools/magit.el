;;; tools/magit.el -*- lexical-binding: t; -*-

(use-package magit
  :custom
  (magit-commit-diff-inhibit-same-window t)
  (magit-save-repository-buffers 'dontask)
  (magit-commit-show-diff nil)
  (magit-branch-direct-configure nil)
  (magit-refresh-status-buffer nil)
  (magit-repolist-columns
   '(("Name" 30 magit-repolist-column-ident nil)
     ("Version" 30 magit-repolist-column-version ((:sort magit-repolist-version<)))
     ("Updates" 8 magit-repolist-column-unpulled-from-upstream ((:right-align t) (:sort <)))
     ("Changes" 8 magit-repolist-column-unpushed-to-upstream ((:right-align t) (:sort <)))
     ("Path" 99 magit-repolist-column-path nil)))
  (magit-blame-styles
   '((headings
      (heading-format . "%-20a %C %s\n")
      ;; (highlight-face . magit-blame-highlight)
      )
     (highlight
      (highlight-face . magit-blame-highlight))
     (lines
      (show-lines . t)
      (show-message . t))))
  :config
  (utils/custom-set-faces
   (hl-line
    ((nil :background unspecified)))
   (magit-header-line
    ((nil :weight bold
          :box unspecified
          :inherit header-line
          :foreground ,(doom-color 'fg)
          :background ,(doom-color 'bg-alt))))
   (magit-diff-file-heading-selection
    ((nil :foreground ,(doom-color 'red)
          :background ,(doom-blend (doom-color 'dark-blue) (doom-color 'bg) 0.1))))
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

   (magit-blame-highlight
    ((nil :foreground unspecified
          :background ,(doom-color 'bg-alt))))
   (magit-blame-heading
    ((nil :background ,(doom-color 'bg-alt))))
   )

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

(use-package pr-review :after forge
  :demand t
  :custom
  (pr-review-fringe-icons nil)
  (pr-review-section-indent-width 2)
  (pr-review-diff-font-lock-syntax nil)
  :config
  (utils/custom-set-faces
   (pr-review-author-face
    ((nil :weight normal
          :foreground ,(doom-color 'fg-alt)
          )))

   (pr-review-branch-face
    ((nil :inherit unspecified
          :weight bold
          :foreground ,(doom-color 'green))))

   (pr-review-title-face
    ((nil :inherit font-lock-type-face)))

   (pr-review-button-face
    ((nil :inherit font-lock-comment-face)))

   (pr-review-check-face
    ((nil :foreground ,(doom-color 'yellow))))

   (pr-review-thread-comment-face
    ((nil :inherit hl-line)))

   (pr-review-timestamp-face
    ((nil :height 0.8 :slant italic :foreground ,(doom-color 'fg-alt))))

   (pr-review-link-face
    ((nil :inherit default)))

   (pr-review-success-state-face
    ((nil :inherit success)))

   (pr-review-error-state-face
    ((nil :inherit error)))

   (pr-review-info-state-face
    ((nil :inherit default :slant italic :foreground ,(doom-color 'fg-alt))))

   (pr-review-state-face
    ((nil :inherit default)))

   (pr-review-hash-face
    ((nil :inherit font-lock-comment-face)))

   (pr-review-label-face
    ((nil :inherit unspecified :box unspecified)))

   (pr-review-thread-item-title-face
    ((nil :inherit font-lock-constant-face)))

   (pr-review-reaction-face
    ((nil :inherit unspecified
          :height 1.0
          :foreground ,(doom-color 'dark-blue)
          :background ,(doom-blend (doom-color 'dark-blue) (doom-color 'bg-alt) 0.25)
          :box (:line-width (4 . 2) :color nil :style flat-button)
          )))

   (pr-review-in-diff-thread-title-face
    ((nil :inherit hl-line
          :foreground ,(doom-color 'fg-alt)
          :background ,(doom-color 'bg-alt)
          )))

   (pr-review-in-diff-pending-body-face
    ((nil :inherit hl-line
          :foreground unspecified
          :background ,(doom-color 'bg-alt)
          )))
   (pr-review-in-diff-pending-begin-face
    ((nil :inherit hl-line
          :foreground ,(doom-color 'fg-alt)
          :background ,(doom-color 'bg-alt)
          )))

   (pr-review-in-diff-pending-end-face
    ((nil :inherit hl-line
          :foreground ,(doom-color 'fg-alt)
          :background ,(doom-color 'bg-alt)
          )))
   )

  (defvar-keymap pr-review-minor-mode-map
    :doc "Keymap for pr-review minor mode."
    "C-RET"      #'pr-review-at-point
    "C-<return>" #'pr-review-at-point)

  (define-minor-mode pr-review-minor-mode
    "`pr-review' minor mode."
    :init-value nil
    :keymap pr-review-minor-mode-map)

  (defun pr-review-list-pullreq (&optional dir)
    (interactive
     (list (and (or current-prefix-arg (not (magit-toplevel)))
                (progn (magit--assert-usable-git)
                       (magit-read-repository
                        (>= (prefix-numeric-value current-prefix-arg) 16))))))
    (let* ((default-directory (or dir default-directory)))
      (magit-with-toplevel
        (if-let* ((magit-url (magit-get "remote" "origin" "url"))
                  (repo (forge-get-repository magit-url)))
            (forge-topics-setup-buffer repo nil :type 'pullreq)
          (magit-status)
          (forge-add-repository)))
      ))

  (defun pr-review-review-pullreq (pull-request)
    (interactive (list (forge-read-pullreq "View pull-request")))
    (pr-review-at-point (forge-get-url (forge-get-pullreq pull-request))))

  (defun pr-review-at-point (url)
    (interactive
     (list (forge-get-url (forge-current-pullreq))))
    (pr-review-open-url url))

  (defvar-local pr-review-mode-init nil)
  (defvar-local pr-review-last-pos nil)

  (defun pr-review-setup ()
    (setq-local font-lock-defaults nil
                pr-review-mode-init t
                shr-max-width nil)
    (face-remap-add-relative
     'diff-added
     `(nil :background ,(doom-blend (doom-color 'green) (doom-color 'bg) 0.1)))

    (face-remap-add-relative
     'diff-removed
     `(nil :background ,(doom-blend (doom-color 'red) (doom-color 'bg) 0.1)))

    (face-remap-add-relative
     'diff-context
     `(nil :background ,(doom-color 'bg-alt))))

  (defun pr-review--propertize-keyword-override (str)
    (let* ((trim-pattern "[][ \t\n\r]+")
           (trimmed-str (string-trim str trim-pattern trim-pattern)))
      (propertize str 'face
                  (cond
                   ((member trimmed-str '("OPEN" "SUCCESS" "APPROVED"))
                    'success)
                   ((member trimmed-str '("CHANGES_REQUESTED" "UNKNOWN" "WARNING"))
                    'warning)
                   ((member trimmed-str '("REJECTED" "FAILURE" "TIMED_OUT" "ERROR" "CONFLICTING" "CLOSED"))
                    'error)
                   ((member trimmed-str '("MERGED"))
                    'font-lock-constant-face)
                   ((member trimmed-str '("COMMENTED" "RESOLVED" "OUTDATED" "COMPLETED"))
                    'pr-review-info-state-face)
                   (t
                    'pr-review-state-face)))))

  (advice-add #'pr-review--propertize-keyword :override #'pr-review--propertize-keyword-override)

  (cl-defun magit-toggle-section-by-type (type &key hidep)
    (magit-map-sections
     (lambda (section)
       (when (eq (oref section type) type)
         (if hidep
             (magit-section-hide section)
           (magit-section-toggle section)))
       )))

  (cl-defun magit-section-show-level-by-type (type &key level)
    (magit-map-sections
     (lambda (section)
       (when (eq (oref section type) type)
         (if (< level 0)
             (let ((s section))
               (setq level (- level))
               (while (> (1- (length (magit-section-ident s))) level)
                 (setq s (oref s parent))
                 (goto-char (oref s start)))
               (magit-section-show-children magit-root-section (1- level)))
           (dolist (section (or (magit-region-sections)
                                (list section)))
             (cl-do* ((s section
                         (oref s parent))
                      (i (1- (length (magit-section-ident s)))
                         (cl-decf i)))
                 ((cond ((< i level) (magit-section-show-children s (- level i 1)) t)
                        ((= i level) (magit-section-hide s) t))
                  (magit-section-goto s)))))
         ))))

  (defun pr-review--refresh-internal-before (&rest _)
    (unless pr-review-mode-init
      (magit-map-sections (lambda (section) (magit-section-cache-visibility section)))
      (setq-local pr-review-last-pos (point))))

  (defun pr-review--refresh-internal-after (&rest _)
    (let ((magit-section-cache-visibility nil))
      (magit-map-sections (lambda (section) (magit-section-show section))))

    (unless pr-review-mode-init
      (magit-map-sections
       (lambda (section)
         (if (eq 'hide (magit-section-cached-visibility section))
             (magit-section-hide section))
         )))

    (when pr-review-mode-init
      (magit-toggle-section-by-type 'pr-review--review-thread-section :hidep t))

    (magit-section-show-level-by-type 'pr-review--diff-section :level 2)

    (when pr-review-last-pos (goto-char pr-review-last-pos))
    (setq-local pr-review-mode-init nil
                pr-review-last-pos nil))

  (advice-add #'pr-review--refresh-internal :before #'pr-review--refresh-internal-before)
  (advice-add #'pr-review--refresh-internal :after #'pr-review--refresh-internal-after)

  (defun pr-review--format-relative-time (time format-singular format-plural)
    (let ((val (round time)))
      (format (if (> val 1) format-plural format-singular) val)))

  (defun pr-review--format-relative-timestamp-override (str)
    "Convert and format timestamp STR from json."
    (let* ((timeobj (date-to-time str))
           (days-elapsed (time-to-number-of-days (time-subtract (current-time) timeobj)))
           (hours-elapsed (* 24 days-elapsed))
           (min-elapsed (* 60 hours-elapsed)))
      (concat
       (propertize (cond
                    ((< min-elapsed 1) "now")
                    ((< hours-elapsed 1)
                     (pr-review--format-relative-time min-elapsed "%d minute ago" "%d minutes ago"))
                    ((< days-elapsed 1)
                     (pr-review--format-relative-time hours-elapsed "%d hour ago" "%d hours ago"))
                    ((< days-elapsed 7)
                     (pr-review--format-relative-time days-elapsed "%d day ago" "%d days ago"))
                    ((< days-elapsed 21)
                     (pr-review--format-relative-time (/ days-elapsed 7) "%d week ago" "%d weeks ago"))
                    ((< days-elapsed 24) "last month")
                    (t (format-time-string "%b %d, %Y, %H:%M" timeobj)))
                   'face 'pr-review-timestamp-face
                   'help-echo (format-time-string "%b %d, %Y, %H:%M" timeobj))
       (when (and pr-review--last-read-time (string> str pr-review--last-read-time))
         (concat " " (propertize "UNREAD"
                                 'face 'pr-review-state-face
                                 'pr-review-unread t)))
       )))

  (advice-add #'pr-review--format-timestamp :override #'pr-review--format-relative-timestamp-override)

  (transient-define-group forge--review-group
    ["Review"
     ("r p" "pull requests" pr-review-review-pullreq)
     ""])

  (transient-append-suffix
    'forge-dispatch
    'forge--lists-group
    'forge--review-group)

  (defun magit-mode-quit-window-override (kill-buffer)
    "Quit the selected window and bury its buffer.

This behaves similar to `quit-window', but when the window
was originally created to display a Magit buffer and the
current buffer is the last remaining Magit buffer that was
ever displayed in the selected window, then delete that
window."
    (if (or (one-window-p)
            (seq-find (pcase-lambda (`(,buffer))
                        (and (not (eq buffer (current-buffer)))
                             (buffer-live-p buffer)
                             (or (not (window-parameter nil 'magit-dedicated))
                                 (with-current-buffer buffer
                                   (derived-mode-p 'magit-mode
                                                   'magit-process-mode)))))
                      (window-prev-buffers)))
        (quit-window kill-buffer)
      (let ((window (selected-window)))
        (quit-window kill-buffer)
        (when (window-live-p window)
          (delete-window window)))))

  :hook
  (forge-topics-mode  . pr-review-minor-mode)
  (forge-pullreq-mode . pr-review-minor-mode)
  (pr-review-mode     . pr-review-setup))
