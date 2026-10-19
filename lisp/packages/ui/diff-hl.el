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
  (diff-hl-show-hunk-ignorable-commands
   '(ignore
     diff-hl-show-hunk
     handle-switch-frame
     diff-hl-show-hunk--click
     windex-scroll-up
     windex-scroll-down
     ))
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

  (defun +diff-hl-show-hunk-clear (&rest _)
    (when (buffer-live-p (get-buffer diff-hl-show-hunk-buffer-name))
      (kill-buffer diff-hl-show-hunk-buffer-name)))

  (advice-add #'diff-hl-show-hunk :before #'+diff-hl-show-hunk-clear)

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
