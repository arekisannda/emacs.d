;;; emacs/window.el -*- lexical-binding: t; -*-

(use-package window
  :custom
  (switch-to-buffer-obey-display-actions t)
  (window-combination-limit 'window-size)
  (window-sides-slots '(3 0 3 2))
  (window-sides-vertical t)
  (even-window-sizes nil)
  :init
  (setq-default window-persistent-parameters
                '((window-slot             . writable)
                  (window-side             . writable)
                  (window-purpose          . writable)
                  (window-popup            . writable)
                  (clone-of                . t)
                  (no-other-window         . t)
                  (no-delete-other-windows . t)
                  (window-preserved-size   . t))))

(use-package windmove
  :custom
  (windmove-allow-all-windows t))

(use-package winner
  :custom
  (winner-dont-bind-my-keys t)
  :hook
  (window-setup . winner-mode))
