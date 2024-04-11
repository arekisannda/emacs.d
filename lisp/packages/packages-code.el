;;; packages-code.el --- Coding Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package leetcode
  :custom
  (leetcode-prefer-language "golang")
  (leetcode-prefer-sql "mysql")
  (leetcode-save-solutions t)
  (leetcode-directory (expand-file-name "~/Code/leetcode"))
  :hook
  (leetcode-solution-mode . (lambda () (eglot--managed-mode -1)))
  :config
  (defun +leetcode--solving-window-layout-override ()
    (delete-other-windows)
    (setq leetcode--description-window (selected-window))
    (setq leetcode--testcase-window (split-window-below))
    (other-window 1)
    (setq leetcode--result-window (split-window-below))
    (setq leetcode--code-window (split-root-window-right))
    (other-window 1)
    (other-window 1))

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
  (advice-add #'leetcode--display-code :override #'+leetcode--display-code-override))

(use-package exercism.
  :ensure (exercism :type git :host github :repo "arekisannda/exercism.el")
  :custom
  (exercism-workspace-path (expand-file-name "~/Code/exercism"))
  (exercism-enable-log-to-message-buffer nil)
  (exercism-open-url-on-submit nil)
  :hook
  (window-setup . exercism-setup))

(provide 'packages-code)

;;; packages-code.el ends here
