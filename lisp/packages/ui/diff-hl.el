;;; ui/diff-hl.el -*- lexical-binding: t; -*-

(use-package diff-hl :after magit
  :custom
  (diff-hl-flydiff-delay 0.1)
  :init
  (setq diff-hl-show-hunk-map (make-sparse-keymap)
        diff-hl-inline-popup-transient-mode-map (make-sparse-keymap))
  :config
  (defmacro create-diff-hl-scroll-defun (direction)
    (let ((dir (symbol-name direction)))
      `(defun ,(intern (format "diff-hl-scroll-%s" dir)) ()
         ,(format "Scroll %s in `diff-hl` frame." dir)
         (interactive)
         (if diff-hl-show-hunk--frame
             (windex-with-selector diff-hl-show-hunk--frame nil
               (,(intern (concat "windex-scroll-" dir)))
               )))))

  (create-diff-hl-scroll-defun up)
  (create-diff-hl-scroll-defun down)
  (create-diff-hl-scroll-defun right)
  (create-diff-hl-scroll-defun left)

  (diff-hl-flydiff-mode)
  :hook
  (diff-hl-mode . diff-hl-margin-local-mode)
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package diff-hl-show-hunk-posframe :after (diff-hl posframe)
  :custom
  (diff-hl-show-hunk-function #'diff-hl-show-hunk-posframe)
  (diff-hl-show-hunk-posframe-show-header-line nil)
  (diff-hl-show-hunk-posframe-internal-border-width 1)
  (diff-hl-show-hunk-posframe-poshandler nil)
  (diff-hl-show-hunk-posframe-parameters
   '((left . -1)
     (top . -1)
     (width  . 0)
     (height  . 0)

     (no-accept-focus . t)
     (no-focus-on-map . t)
     (min-width  . 0)
     (min-height  . 0)
     (internal-border-width . 1)
     (vertical-scroll-bars . nil)
     (horizontal-scroll-bars . nil)
     (right-fringe . 3)
     (left-fringe . 3)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (line-spacing . 0)
     (unsplittable . t)
     (undecorated . t)
     (visibility . nil)
     (mouse-wheel-frame . nil)
     (no-other-frame . t)
     (cursor-type . nil)
     (inhibit-double-buffering . t)
     (drag-internal-border . t)
     (no-special-glyphs . t)
     (desktop-dont-save . t)
     (tab-bar-lines . 0)
     (tab-bar-lines-keep-state . 1)))
  (diff-hl-show-hunk-posframe-internal-border-color (face-attribute 'popup-border :background nil t))
  (diff-hl-show-staged-changes nil)
  (diff-hl-show-hunk-buffer-name " *diff-hl-show-hunk-buffer*")
  (diff-hl-show-hunk-diff-buffer-name " *diff-hl-show-hunk-diff-buffer*")
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
      (recenter)
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
          (setq-local truncate-lines t)
          (face-remap-add-relative 'default 'treemacs-window-background-face)
          (face-remap-add-relative 'fringe  'treemacs-window-background-face)
          (visual-fill-column-mode -1)
          (visual-line-mode -1)
          (diff-hl-show-hunk-posframe--transient-mode 1)
          (when diff-hl-show-hunk-posframe-show-header-line
            (setq header-line-format (diff-hl-show-hunk-posframe--header-line)))
          (goto-char (point-min))
          (setq buffer-quit-function #'diff-hl-show-hunk--posframe-hide)
          (select-window (window-main-window diff-hl-show-hunk--frame))

          ;; Make cursor visible (mainly for selecting text in posframe)
          (setq cursor-type 'box)

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
