;;; packages-completion.el --- Completion Tools Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'keymap)
(require 'util-helpers)

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
   '((not repl-mode vterm-mode comint-mode)
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
  (eshell-mode . +corfu-auto-disable)
  (after-init . global-corfu-mode)
  (global-corfu-mode . corfu-popupinfo-mode)
  (minibuffer-setup . +corfu-auto-disable))

(use-package nerd-icons-corfu :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :config
  (require 'cape-char)

  (util/add-capf-hooks
   #'cape-dabbrev
   #'cape-file
   #'cape-keyword)

  (plist-put cape--tex-properties :exit-function nil)

  (defcustom with-capf-extras-functions '()
    "Functions to replace in command `with-capf-extras'"
    :type '(repeat function)
    :group 'convenience)

  (defun with-capf-extras-command ()
    (interactive)
    (let ((completion-at-point-functions
           (list
            #'yasnippet-capf)))
      (completion-at-point)))
  :hook
  (eglot-managed-mode
   . (lambda ()
       (pcase major-mode
         ('org-mode (remove-hook 'completion-at-point-functions #'eglot-completion-at-point t)))
       )))

(use-package yasnippet-capf :after yasnippet
  :custom
  (yasnippet-capf-lookup-by 'key))

(use-package yasnippet
  :custom-face
  (yas-field-highlight-face
   ((nil :inherit region)))
  :custom
  (yas-indent-line 'fixed)
  (yas-keymap-disable-hook
   (lambda () (and (frame-live-p corfu--frame)
                   (frame-visible-p corfu--frame))))
  :diminish yas-minor-mode)

(use-package yasnippet-snippets :after yasnippet
  :config
  (let ((dir (expand-file-name "snippets" user-emacs-directory)))
    (unless (member dir yas-snippet-dirs)
      (add-to-list 'yas-snippet-dirs dir)
      (yas--load-snippet-dirs))))

(provide 'packages-completion)

;;; packages-completion.el ends here
