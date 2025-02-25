;;; packages-completion.el --- Completion Tools Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'keymap)
(require 'cl-seq)
(require 'util-helpers)

(use-package corfu
  :init
  (setq corfu-map (make-sparse-keymap)
        corfu-popupinfo-map (make-sparse-keymap))
  :custom
  (completion-auto-help 'always)
  (corfu-preselect 'first)
  (corfu-preview-current t)
  (corfu-on-exact-match nil)
  (corfu-sort-override-function
   (lambda (candidates)
     "Yasnippet candidates first"
     (sort candidates
           (lambda (x y)
             (and (< (length x) (length y) )
                  (get-text-property 0 'yas-annotation x))))
     candidates))
  (corfu-cycle nil)
  (corfu-auto nil)
  (corfu-popupinfo-delay (cons nil 0.5))
  (corfu-min-width 40)
  (corfu-max-width 100)
  (corfu-left-margin-width 1.0)
  (corfu-right-margin-width 1.0)
  (corfu-scroll-margin 2)
  (corfu-bar-width 0.5)
  (completion-cycle-threshold nil)
  (tab-always-indent nil)
  :config
  (defun +corfu-minibuffer-completion-setup ()
    "Setup to run for minibuffer mode."
    (shut-up
      (unless (or (bound-and-true-p mct--active)
                  (bound-and-true-p vertico--input)
                  (eq (current-local-map) read-passwd-map))
        (when (local-variable-p 'completion-at-point-functions)
          (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                      corfu-auto nil
                      corfu-cycle nil
                      corfu-popupinfo-delay (cons nil 0.5)
                      corfu-min-width 40))
        (setq-local completion-cycle-threshold nil)
        (setq-local tab-always-indent nil)
        (corfu-mode 1))))
  :hook
  (window-setup . global-corfu-mode)
  (global-corfu-mode . corfu-popupinfo-mode)
  (corfu-mode . completion-preview-mode)
  (minibuffer-setup . +corfu-minibuffer-completion-setup))

(use-package emacs
  :ensure nil
  :custom-face
  (completion-preview
   ((nil :inherit nil
         :foreground ,(doom-darken (doom-color 'yellow) 0.2))))
  (completion-preview-exact
   ((nil :inherit completion-preview-common
         :underline (:color ,(doom-darken (doom-color 'yellow) 0.2)
                            :style line
                            :position nil)))))

(use-package nerd-icons-corfu :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package corfu-terminal :disabled)

(use-package cape
  :config
  (require 'cape-char)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :hook
  (eglot-managed-mode
   . (lambda ()
       (setq-local completion-at-point-functions
                   (list
                    (cape-capf-super
                     #'eglot-completion-at-point
                     #'yasnippet-capf)))
       (add-to-list 'completion-at-point-functions #'cape-file)))
  )

(use-package yasnippet-capf :after yasnippet
  :custom
  (yasnippet-capf-lookup-by 'key))

(provide 'packages-completion)

;;; packages-completion.el ends here
