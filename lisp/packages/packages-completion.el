;;; packages-completion.el --- Completion Tools Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'keymap)
(require 'util-helpers)

(use-package vertico
  :preface
  (defun +vertico-sort-directories-first (list)
    "Sort LIST by directories first."
    (setq list (vertico-sort-history-length-alpha list))
    (nconc (cl-loop for x in list if (string-suffix-p "/" x) collect x)
           (cl-loop for x in list if (not (string-suffix-p "/" x)) collect x)))

  (defun +vertico-add-options (command options)
    (append `(,command
              posframe)
            options
            '((vertico-posframe-fallback-mode . vertico-multiform-vertical))))
  :custom
  (minibuffer-prompt-properties
   '(read-only t
               cursor-intangible t
               face (:inherit minibuffer-prompt :weight bold :height 1.0)))
  (vertico-count-format
   `("%-6s " . ,(concat (nerd-icons-octicon "nf-oct-search")
                        " ( %s/%s )")))
  (vertico-count 20)
  (savehist-file (expand-file-name "var/savehist" user-emacs-directory))
  (vertico-multiform-commands
   `(,(+vertico-add-options #'find-file
                            '((vertico-sort-override-function . vertico-sort-alpha)))
     ,(+vertico-add-options #'project-switch-project
                            '((vertico-sort-override-function . +vertico-sort-directories-first)))
     ,(+vertico-add-options #'project-kill-buffers
                            '((vertico-sort-override-function . +vertico-sort-directories-first)))
     ,(+vertico-add-options #'project-find-file
                            '((vertico-sort-override-function . +vertico-sort-directories-first)))
     ,(+vertico-add-options #'project-find-dir
                            '((vertico-sort-override-function . +vertico-sort-directories-first)))
     ,(+vertico-add-options #'project-forget-project
                            '((vertico-sort-override-function . +vertico-sort-directories-first)))
     ,(+vertico-add-options #'project-forget-project-under
                            '((vertico-sort-override-function . +vertico-sort-directories-first)))
     ,(+vertico-add-options #'describe-symbol
                            '((vertico-sort-override-function . vertico-sort-alpha)))
     ,(+vertico-add-options #'rfc-mode-goto-section
                            '((vertico-sort-override-function . nil)))
     ,(+vertico-add-options #'execute-extended-command
                            '((vertico-sort-override-function . vertico-sort-history-alpha)))
     ;; default
     ,(+vertico-add-options t '((vertico-sort-override-function . nil)))
     ))
  (vertico-multiform-categories
   `((font    (vertico-sort-function . nil))
     (face    (vertico-sort-function . nil))

     (keyword (vertico-sort-function . vertico-sort-history-alpha))
     (command (vertico-sort-function . vertico-sort-history-alpha))
     (history (vertico-sort-function . vertico-sort-history-alpha))

     (symbol  (vertico-sort-function . vertico-sort-history-length-alpha))
     (buffer  (vertico-sort-function . vertico-sort-history-alpha))
     (file    (vertico-sort-function . +vertico-sort-directories-first))))
  (vertico-sort-function nil)
  :custom-face
  (vertico-default
   ((nil :inherit tooltip
         :background ,(doom-color 'bg-alt))))
  (vertico-current
   ((nil :inherit default
         :foreground ,(doom-color 'fg)
         :background ,(doom-color 'bg))))
  :config
  (advice-add
   #'vertico--format-candidate :around
   (lambda (orig-fun cand prefix suffix index start)
     (apply orig-fun (list cand
                           (if (= vertico--index index)
                               (concat (nerd-icons-faicon
                                        "nf-fa-angles_right"
                                        :face 'nerd-icons-red)
                                       "  " prefix)
                             (concat "   " prefix))
                           suffix
                           index start))))
  :hook
  (after-init . vertico-mode)
  (vertico-mode . vertico-multiform-mode)
  (vertico-mode . savehist-mode))

(use-package vertico-posframe :after vertico
  :custom
  (vertico-posframe-poshandler #'posframe-poshandler-frame-center)
  (vertico-posframe-border-width 1)
  (vertico-posframe-width 120)
  (vertico-posframe-min-width nil)
  (vertico-posframe-min-height 20)
  (vertico-posframe-parameters
   `((max-width . 200)
     (max-height . 20)
     (left-fringe . 3)
     (right-fringe . 3)))
  :custom-face
  (vertico-posframe
   ((nil :inherit tooltip
         :foreground unspecified
         :background ,(doom-color 'bg-alt))))
  (vertico-posframe-border
   ((nil :inherit popup-border
         :background unspecified
         :foreground unspecified)))
  (vertico-posframe-border-2
   ((nil :inherit default
         :background ,(doom-color 'orange))))
  (vertico-posframe-border-3
   ((nil :inherit default
         :background ,(doom-color 'yellow))))
  (vertico-posframe-border-4
   ((nil :inherit default
         :background ,(doom-color 'base8))))
  (vertico-posframe-border-fallback
   ((nil :inherit default
         :background ,(doom-color 'vertical-bar))))
  :config
  (defun +vertico-posframe-show-cursor (buffer window-point)
    (with-current-buffer buffer
      (setq-local cursor-type 'box)
      (setq-local highlight-nonselected-windows t)
      (setq-local cursor-in-non-selected-windows 'box)
      (posframe-refresh buffer)))

  (advice-add #'vertico-posframe--show :after #'+vertico-posframe-show-cursor))

(use-package marginalia)

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
  (corfu-auto-prefix 0)
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

(provide 'packages-completion)

;;; packages-completion.el ends here
