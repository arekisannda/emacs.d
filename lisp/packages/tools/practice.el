;;; tools/practice.el -*- lexical-binding: t; -*-

(use-package leetcode
  :defer t
  :custom
  (leetcode-prefer-language "golang")
  (leetcode-prefer-sql "mysql")
  (leetcode-save-solutions t)
  :config
  (defun +leetcode--solving-window-layout-override ()
    (delete-other-windows)
    (setq leetcode--description-window (selected-window))
    (setq leetcode--code-window (split-root-window-right))
    (setq leetcode--testcase-window (split-window-below))
    (other-window 1)
    (setq leetcode--result-window (split-window-below))
    (select-window leetcode--code-window))

  (defun +leetcode--display-result-override (buffer &optional _alist)
    (set-window-buffer leetcode--result-window buffer)
    leetcode--result-window)

  (defun +leetcode--display-testcase-override (buffer &optional _alist)
    (set-window-buffer leetcode--testcase-window buffer)
    leetcode--testcase-window)

  (defun +leetcode--display-detail-override (buffer &optional _alist)
    (set-window-buffer leetcode--description-window buffer)
    leetcode--description-window)

  (defun +leetcode--display-code-override (buffer &optional _alist)
    (set-window-buffer leetcode--code-window buffer)
    leetcode--code-window)

  (advice-add #'leetcode--solving-window-layout :override #'+leetcode--solving-window-layout-override)
  (advice-add #'leetcode--display-result :override #'+leetcode--display-result-override)
  (advice-add #'leetcode--display-testcase :override #'+leetcode--display-testcase-override)
  (advice-add #'leetcode--display-detail :override #'+leetcode--display-detail-override)
  (advice-add #'leetcode--display-code :override #'+leetcode--display-code-override))

(use-package exercism :disabled
  :defer t
  :custom
  (exercism-enable-log-to-message-buffer nil)
  (exercism-open-url-on-submit nil)
  :hook
  (after-init . exercism-setup))
