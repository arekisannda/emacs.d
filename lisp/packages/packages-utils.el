;;; packages-utils.el --- Emacs Utility Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'lib-layouts)
(require 'cl-lib)

(use-package posframe
  :custom
  (posframe-inhibit-double-buffering t)
  (posframe-mouse-banish-function #'posframe-mouse-banish-default)
  :config
  (defun +posframe-show-refresh (buffer &rest _)
    (posframe-refresh buffer))
  (advice-add #'posframe-show :after #'+posframe-show-refresh))

(use-package transient
  :custom
  (transient-show-popup t)
  (transient-display-buffer-action
   '(display-buffer-in-side-window
     (side . right)
     (slot . 1)
     (dedicated . t)))
  (transient-mode-line-format nil)
  (transient-force-fixed-pitch t))

(use-package which-key
  :ensure nil
  :custom
  (which-key-popup-type 'side-window)
  (which-key-sort-order 'which-key-key-order)
  (which-key-show-prefix 'echo)
  (which-key-side-window-slot 0)
  (which-key-side-window-location 'left)
  (which-key-max-display-columns nil)
  (which-key-side-window-max-width 40)
  (which-key-min-column-description-width 40)
  :init
  (defun +which-key-buffer-width-setup ()
    (treemacs-select-window))

  (defun +which-key-buffer-post-display-setup (&rest _)
    (when (buffer-live-p which-key--buffer)
      (with-current-buffer which-key--buffer
        (face-remap-add-relative 'default :background (doom-color 'bg-alt)))))

  (advice-add #'which-key--show-popup :after #'+which-key-buffer-post-display-setup)
  :hook
  (which-key-init-buffer . +which-key-buffer-width-setup)
  (elpaca-after-init . which-key-mode))

(use-package vertico
  :preface
  (defun +vertico-sort-directories-first (files)
    "Sort FILES by directories first."
    (setq files (vertico-sort-history-length-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  (defun +vertico-add-options (command options)
    (append `(,command
              posframe)
            options
            '((vertico-posframe-fallback-mode . vertico-buffer-mode))))
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
   ;; disable posframe
   ;; (function-symbol (:not posframe))
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
     (t posframe)))
  (vertico-multiform-categories
   `((file (vertico-sort-override-function . +vertico-sort-directories-first))))
  :hook
  (window-setup . vertico-mode)
  (vertico-mode . vertico-multiform-mode)
  (vertico-mode . savehist-mode)
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
                           index start)))))

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
     (left-fringe . nil)
     (right-fringe . nil)))
  :custom-face
  (vertico-posframe
   ((nil :inherit default
         :foreground ,(doom-color 'fg)
         :background ,(doom-color 'bg))))
  (vertico-posframe-border
   ((nil :inherit default
         :background ,(doom-color 'fg-alt))))
  (vertico-posframe-border-2
   ((nil :inherit default
         :background ,(doom-color 'red))))
  (vertico-posframe-border-3
   ((nil :inherit default
         :background ,(doom-color 'green))))
  (vertico-posframe-border-4
   ((nil :inherit default
         :background ,(doom-color 'blue))))
  :config
  (defun +vertico-posframe-show-cursor (buffer window-point)
    (with-current-buffer buffer
      (setq-local cursor-type 'box)
      (setq-local highlight-nonselected-windows t)
      (setq-local cursor-in-non-selected-windows 'box)
      (posframe-refresh buffer)))

  (advice-add #'vertico-posframe--show :after #'+vertico-posframe-show-cursor))

(use-package marginalia)

(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-dispatch-when-more-than 0)
  :custom-face
  (aw-leading-char-face
   ((nil :weight bold
         :height 2.00
         :foreground ,(doom-color 'red))))
  :init
  (setq aw-dispatch-alist
        '((?x aw-delete-window "Delete Window")
          (?m aw-swap-window "Swap Windows")
          (?M aw-move-window "Move Window")
          (?c aw-copy-window "Copy Window")
          (?j aw-switch-buffer-in-window "Select Buffer")
          (?\\ aw-flip-window)
          (?F aw-split-window-fair "Split Fair Window")
          (?v aw-split-window-vert "Split Vert Window")
          (?b aw-split-window-horz "Split Horz Window")
          (?? aw-show-dispatch-help)))

  (defun +window-check-aw-ignored-p (orig-func &rest args)
    ;; Ignore side-windows or popup-windows
    (let ((window (nth 0 args)))
      (cond
       ((util/window-side-p window) t)
       ((util/window-popup-p window) t)
       (t (apply orig-func args)))))

  (advice-add #'aw-ignored-p :around #'+window-check-aw-ignored-p)
  :hook
  (elpaca-after-init . ace-window-posframe-mode))

(use-package flymake
  :ensure nil
  :custom
  (flymake-start-on-flymake-mode t)
  (flymake-show-diagnostics-at-end-of-line nil)
  (flymake-indicator-type nil)
  (flymake-fringe-indicator-position nil)
  :hook
  (prog-mode . flymake-mode)
  (text-mode . flymake-mode))

(use-package flymake-python-pyflakes
  :custom
  (flymake-python-pyflakes-executable "flake8")
  :hook
  (python-ts-mode . flymake-python-pyflakes-load))

(use-package flymake-golangci
  :hook
  (go-ts-mode . flymake-golangci-load))

(provide 'packages-utils)

;;; packages-utils.el ends here
