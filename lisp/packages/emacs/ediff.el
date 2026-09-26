;;; ediff.el -*- lexical-binding: t; -*-

(require 'util-helpers)

(use-package ediff
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (utils/custom-set-faces
   (ediff-current-diff-A
    ((nil :inherit unspecified
          :background ,(doom-blend (doom-color 'red) (doom-color 'bg) 0.1)
          )))
   (ediff-fine-diff-A
    ((nil :inherit unspecified
          :weight bold
          :foreground ,(doom-color 'red)
          :background ,(doom-blend (doom-color 'red) (doom-color 'bg) 0.1)
          )))
   (ediff-current-diff-B
    ((nil :inherit unspecified
          :background ,(doom-blend (doom-color 'green) (doom-color 'bg) 0.1)
          )))
   (ediff-fine-diff-B
    ((nil :inherit unspecified
          :weight bold
          :foreground ,(doom-color 'green)
          :background ,(doom-blend (doom-color 'green) (doom-color 'bg) 0.1)
          )))
   (ediff-current-diff-C
    ((nil :inherit unspecified
          :background ,(doom-blend (doom-color 'dark-blue) (doom-color 'bg) 0.1)
          )))
   (ediff-fine-diff-C
    ((nil :inherit unspecified
          :weight bold
          :foreground ,(doom-color 'dark-blue)
          :background ,(doom-blend (doom-color 'dark-blue) (doom-color 'bg) 0.1)
          )))
   )

  (defun +diff-remove-footer (code &rest _)
    "Delete the \"Diff finished\" line, except when there's something worth reporting."
    (when (eq code 1)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (when (re-search-backward "^Diff finished" nil t)
            (goto-char (match-beginning 0))
            (skip-chars-backward "\n")
            (delete-region (point) (point-max))
            (insert "\n"))))))

  (advice-add 'diff-sentinel :after #'+diff-remove-footer))
