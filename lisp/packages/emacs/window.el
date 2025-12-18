;;; emacs/window.el -*- lexical-binding: t; -*-

(require 'windmove)

(setq-default windmove-allow-all-windows t)
(setq-default switch-to-buffer-obey-display-actions t)
(setq-default window-combination-limit 'window-size)
(setq-default window-sides-slots '(3 0 3 2))
(setq-default window-sides-vertical t)
(setq-default even-window-sizes nil)
(setq-default window-persistent-parameters
              '((window-slot             . writable)
                (window-side             . writable)
                (window-purpose          . writable)
                (window-popup            . writable)
                (clone-of                . t)
                (no-other-window         . t)
                (no-delete-other-windows . t)
                (window-preserved-size   . t)))
