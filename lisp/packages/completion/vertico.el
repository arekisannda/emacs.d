;;; completion/vertico.el -*- lexical-binding: t; -*-

(defun +vertico-sort-directories-first (list)
  "Sort LIST by directories first."
  (setq list (vertico-sort-history-length-alpha list))
  (nconc (cl-loop for x in list if (string-suffix-p "/" x) collect x)
         (cl-loop for x in list if (not (string-suffix-p "/" x)) collect x)))

(defun +vertico-sort-project (list)
  "Sort LIST by directories first."
  (setq list (vertico-sort-history-alpha list))
  (nconc (cl-loop for x in list if (not (string-suffix-p "/" x)) collect x)
         (cl-loop for x in list if (string-suffix-p "/" x) collect x)))

(defun +vertico-add-options (command &rest options)
  `(,command posframe ,@options
             (vertico-posframe-fallback-mode . vertico-multiform-vertical)))

(use-package vertico
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
   `(,(+vertico-add-options #'project-switch-project
                            '(vertico-sort-override-function . +vertico-sort-project))
     ,(+vertico-add-options #'project-forget-project
                            '(vertico-sort-override-function . +vertico-sort-project))
     ,(+vertico-add-options #'+treemacs-add-project-to-workspace
                            '(vertico-sort-override-function . +vertico-sort-project))
     ,(+vertico-add-options #'+activities-new-project
                            '(vertico-sort-override-function . +vertico-sort-project))
     ,(+vertico-add-options #'project-forget-project-under
                            '(vertico-sort-override-function . +vertico-sort-directories-first))
     ,(+vertico-add-options #'project-kill-buffers
                            '(vertico-sort-override-function . +vertico-sort-directories-first))
     ,(+vertico-add-options #'project-find-file
                            '(vertico-sort-override-function . +vertico-sort-directories-first))
     ,(+vertico-add-options #'project-find-dir
                            '(vertico-sort-override-function . +vertico-sort-directories-first))

     ,(+vertico-add-options #'find-file
                            '(vertico-sort-override-function . +vertico-sort-directories-first))

     ,(+vertico-add-options #'rfc-mode-goto-section
                            '(minibuffer-default . nil)
                            '(vertico-sort-function . nil)
                            '(vertico-sort-override-function . nil))
     ,(+vertico-add-options #'activities-resume
                            '(vertico-sort-function . nil)
                            '(vertico-sort-override-function . nil))
     ,(+vertico-add-options #'activities-switch
                            '(vertico-sort-function . nil)
                            '(vertico-sort-override-function . nil))
     ;; default
     ,(+vertico-add-options t '(vertico-sort-override-function . nil))
     ))
  (vertico-multiform-categories
   `((symbol (vertico-sort-function . vertico-sort-history-length-alpha))
     (buffer (vertico-sort-function . vertico-sort-history-length-alpha))
     (file   (vertico-sort-function . +vertico-sort-directories-first))))
  (vertico-sort-function #'vertico-sort-history-length-alpha)
  (vertico-sort-override-function #'vertico-sort-history-length-alpha)
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

  (advice-add #'vertico--resize-window :after #'vertico--no-truncate)
  (defun vertico--no-truncate (&rest _)
    (setq-local truncate-lines t))
  :hook
  (after-init . vertico-mode)
  (vertico-mode . vertico-multiform-mode)
  (vertico-mode . savehist-mode))

(use-package vertico-posframe :after (vertico posframe)
  :custom
  (vertico-posframe-poshandler #'posframe-poshandler-frame-center)
  (vertico-posframe-border-width 1)
  (vertico-posframe-width 120)
  (vertico-posframe-parameters
   `((left-fringe . 8)
     (right-fringe . 8)

     (min-width . t)
     (min-height . t)
     (border-width . 0)
     (outer-border-width . 0)
     (internal-border-width . 1)
     (child-frame-border-width . 1)
     (vertical-scroll-bars . nil)
     (horizontal-scroll-bars . nil)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (tab-bar-lines . 0)
     (tab-bar-lines-keep-state . t)
     (no-other-frame . t)
     (unsplittable . t)
     (undecorated . t)
     (cursor-type . nil)
     (no-special-glyphs . t)
     (desktop-dont-save . t)
     (inhibit-double-buffering . t)
     ))
  (vertico-posframe-size-function
   (lambda (buffer)
     (list
      :height (buffer-local-value 'vertico-posframe-height buffer)
      :width (buffer-local-value 'vertico-posframe-width buffer)
      :min-height 20
      :min-width 120
      :max-height 30
      :max-width 200
      :lines-truncate t
      )))
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

(use-package orderless
  :custom
  (completion-styles '(orderless partial-completion basic))
  ;; (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides nil)
  (completion-ignore-case t)
  (orderless-smart-case t)
  ;; (completion-category-overrides '((file (styles basic partial-completion))))
  :init
  (setq completion-category-defaults nil)
  (defun +consult-orderless-regexp-compiler (input type &rest _config)
    (setq input (cdr (orderless-compile input)))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input t str))))

  ;; OPTION 1: Activate globally for all consult-grep/ripgrep/find/...
  ;; (setq consult--regexp-compiler #'+consult-orderless-regexp-compiler)

  ;; OPTION 2: Activate only for some commands, e.g., consult-ripgrep!
  (defun +consult-with-orderless (&rest args)
    (minibuffer-with-setup-hook
        (lambda ()
          (setq-local consult--regexp-compiler #'+consult-orderless-regexp-compiler))
      (apply args)))
  (advice-add #'consult-ripgrep :around #'+consult-with-orderless))
