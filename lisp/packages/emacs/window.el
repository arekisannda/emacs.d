;;; emacs/window.el -*- lexical-binding: t; -*-

(require 'util-windows)



(defun emacs-skip-none-tabbed-buffers (win buf _bok)
  (not (memq buf (with-selected-window win
                   (tab-line-tabs-window-buffers)))))

(use-package window
  :custom
  (window-min-height 1)
  (switch-to-buffer-obey-display-actions t)
  (window-combination-limit nil)
  (window-combination-resize nil)
  (window-sides-slots '(3 0 3 2))
  (window-sides-vertical nil)
  (even-window-sizes nil)
  (util/windows-disable-shrink t)
  (switch-to-prev-buffer-skip #'emacs-skip-none-tabbed-buffers)
  :init
  (setq-default window-persistent-parameters
                '((window-slot             . writable)
                  (window-side             . writable)
                  (window-purpose          . writable)
                  (window-popup            . writable)
                  (window-aux              . writable)
                  (window-aux-id           . writable)
                  (window-aux-other        . writable)
                  (quit-restore            . t)
                  (split-window            . t)
                  (clone-of                . t)
                  (no-other-window         . t)
                  (no-delete-other-windows . t)
                  (window-preserved-size   . t)))

  (advice-add 'quit-window :around #'util/windows--quit-popup-window))

(use-package windmove
  :custom
  (windmove-allow-all-windows t))

(use-package tab-bar
  :hook
  (window-setup . tab-bar-history-mode))

(use-package winner
  :custom
  (winner-dont-bind-my-keys t))
