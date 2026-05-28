;;; completion/corfu.el -*- lexical-binding: t; -*-

(require 'keymap)

(use-package corfu
  :init
  (setq corfu-map (make-sparse-keymap)
        corfu-popupinfo-map (make-sparse-keymap))
  :custom-face
  (completion-preview
   ((nil :inherit nil
         :foreground ,(doom-darken (doom-color 'yellow) 0.2))))
  (completion-preview-exact
   ((nil :inherit completion-preview-common
         :underline (:color ,(doom-darken (doom-color 'yellow) 0.2)
                            :style line
                            :position nil))))
  (corfu-border
   ((nil :inherit popup-border
         :background unspecified
         :foreground unspecified)))
  (corfu-current
   ((nil :inherit default
         :background ,(doom-color 'bg))))
  :custom
  (tab-always-indent 'complete)
  (completion-auto-help 'always)
  (completion-cycle-threshold nil)
  (corfu-preselect 'first)
  (corfu-preview-current nil)
  (corfu-on-exact-match 'show)
  (corfu-cycle nil)
  (corfu-auto t)
  (corfu-auto-prefix 3)
  (corfu-auto-delay 0.3)
  (corfu-popupinfo-delay (cons nil 0.5))
  (corfu-min-width 40)
  (corfu-max-width 100)
  (corfu-left-margin-width 1.0)
  (corfu-right-margin-width 1.0)
  (corfu-scroll-margin 2)
  (corfu-bar-width 0.5)
  (global-corfu-modes
   '(agent-shell-mode
     (not repl-mode vterm-mode)
     t))
  (global-corfu-minibuffer
   (lambda ()
     (not (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map)))))
  :config
  (advice-add #'completion-preview-insert :before #'corfu-quit)

  (defun +corfu-auto-disable ()
    "Setup to run for minibuffer mode."
    (setq-local corfu-auto nil
                tab-always-indent nil))
  :hook
  (eshell-mode       . +corfu-auto-disable)
  (after-init        . global-corfu-mode)
  (minibuffer-setup  . +corfu-auto-disable))

(use-package nerd-icons-corfu :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
