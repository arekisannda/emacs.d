;;; ui/diff-hl.el -*- lexical-binding: t; -*-

(use-package diff-hl :after (magit fringe-helper)
  :preface
  (fringe-helper-define '+diff-hl-bar '(top repeat)
    "X......."
    "X......."
    "X......."
    "X......."
    "X.......")

  (defun +diff-hl-bmp (_type _pos) '+diff-hl-bar)
  :custom
  (diff-hl-fringe-bmp-function #'+diff-hl-bmp)
  (diff-hl-disable-on-remote t)
  (diff-hl-bmp-max-width 16)
  (diff-hl-update-async t)
  (diff-hl-flydiff-delay 0.1)
  (diff-hl-show-staged-changes nil)
  (diff-hl-command-prefix nil)
  (diff-hl-show-hunk-inline-scroll-indicators nil)
  (diff-hl-margin-symbols-alist
   '((insert    . "▊")
     (delete    . "▊")
     (change    . "▊")
     (unknown   . " ")
     (ignored   . " ")
     (reference . " ")))
  :init
  (setq diff-hl-show-hunk-map (make-sparse-keymap)
        diff-hl-inline-popup-transient-mode-map (make-sparse-keymap))
  (setq diff-hl-show-hunk-buffer-name "*diff-hl-show-hunk-buffer*")
  (setq diff-hl-show-hunk-diff-buffer-name "*diff-hl-show-hunk-diff-buffer*")

  :config
  (utils/custom-set-faces
   (diff-hl-c
    ((nil :inherit unspecified
          :foreground ,(doom-color 'grey)
          :background ,(doom-color 'bg)
          )))

   (diff-hl-change
    ((nil :inherit unspecified
          :foreground ,(doom-color 'grey)
          :background ,(doom-color 'bg)
          )))

   (diff-hl-margin-change
    ((nil :inherit diff-hl-change
          :inverse-video t)))

   (diff-hl-insert
    ((nil :inherit unspecified
          :foreground ,(doom-color 'green)
          :background ,(doom-color 'bg)
          )))

   (diff-hl-margin-insert
    ((nil :inherit diff-hl-insert
          :inverse-video t)))

   (diff-hl-delete
    ((nil :inherit unspecified
          :foreground ,(doom-color 'red)
          :background ,(doom-color 'bg)
          )))

   (diff-hl-margin-delete
    ((nil :inherit diff-hl-delete
          :inverse-video t)))
   )

  (defun +diff-hl-show-hunk-inline-show (lines &optional header footer keymap close-hook point height)
    "Create a phantom overlay to show the inline popup, with some
content LINES, and a HEADER and a FOOTER, at POINT.  KEYMAP is
added to the current keymaps.  CLOSE-HOOK is called when the popup
is closed."
    (when diff-hl-show-hunk-inline--current-popup
      (delete-overlay diff-hl-show-hunk-inline--current-popup)
      (setq diff-hl-show-hunk-inline--current-popup nil))
    (when (< (diff-hl-show-hunk-inline--compute-content-height 99) 2)
      (user-error "There is no enough vertical space to show the inline popup"))
    (let* ((the-point (or point (line-end-position)))
           (the-buffer (current-buffer))
           (overlay (make-overlay the-point the-point the-buffer)))
      (overlay-put overlay 'phantom t)
      (overlay-put overlay 'diff-hl-show-hunk-inline t)
      (setq diff-hl-show-hunk-inline--current-popup overlay)
      (setq diff-hl-show-hunk-inline--current-lines
            (mapcar (lambda (s) (replace-regexp-in-string "\n" " " s)) lines))
      (setq diff-hl-show-hunk-inline--current-header header)
      (setq diff-hl-show-hunk-inline--current-footer nil)
      (setq diff-hl-show-hunk-inline--invoking-command this-command)
      (setq diff-hl-show-hunk-inline--current-custom-keymap keymap)
      (setq diff-hl-show-hunk-inline--close-hook close-hook)
      (setq diff-hl-show-hunk-inline--height (diff-hl-show-hunk-inline--compute-content-height height))
      (setq diff-hl-show-hunk-inline--height  20)
      ;; (diff-hl-show-hunk-inline--ensure-enough-lines point diff-hl-show-hunk-inline--height)
      (diff-hl-show-hunk-inline-transient-mode 1)
      (diff-hl-show-hunk-inline-scroll-to 0)
      overlay))

  (advice-add #'diff-hl-show-hunk-inline-show :override #'+diff-hl-show-hunk-inline-show)

  (diff-hl-flydiff-mode 1)
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package diff-hl-show-hunk-display-buffer :after diff-hl
  :custom
  (diff-hl-show-hunk-function #'diff-hl-show-hunk-display-buffer)
  :config

  (advice-add #'diff-hl-show-hunk-display-buffer
              :before
              (lambda (buffer &optional _ignored_line)
                (with-current-buffer buffer
                  (emacs-set-alt-face)))))
