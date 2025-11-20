;;; packages-interface.el --- Interface Packages Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-helpers)

(defface popup-border
  '((((type tty)) :inherit mode-line-inactive))
  "Face used for popup borders."
  :group 'basic-faces)

(use-package doom-modeline
  :custom-face
  (doom-modeline-bar
   ((nil :foreground ,(doom-color 'bg-alt)
         :background ,(doom-color 'bg-alt))))
  (doom-modeline-bar-inactive
   ((nil :foreground ,(doom-color 'bg-alt)
         :background ,(doom-color 'bg-alt))))
  (doom-modeline-evil-insert-state
   ((nil :weight bold
         :foreground ,(doom-color 'bg-alt)
         :background ,(doom-color 'green))))
  (doom-modeline-evil-normal-state
   ((nil :weight bold
         :foreground ,(doom-color 'bg-alt)
         :background ,(doom-color 'blue))))
  (doom-modeline-evil-visual-state
   ((nil :weight bold
         :foreground ,(doom-color 'bg-alt)
         :background ,(doom-color 'yellow))))
  (doom-modeline-evil-replace-state
   ((nil :weight bold
         :foreground ,(doom-color 'bg-alt)
         :background ,(doom-color 'red))))
  (doom-modeline-evil-motion-state
   ((nil :weight bold
         :foreground ,(doom-color 'bg-alt)
         :background ,(doom-color 'magenta))))
  (doom-modeline-evil-operator-state
   ((nil :weight bold
         :foreground ,(doom-color 'bg-alt)
         :background ,(doom-color 'orange))))
  (doom-modeline-evil-emacs-state
   ((nil :weight bold
         :foreground ,(doom-color 'bg-alt)
         :background ,(doom-color 'violet))))
  (doom-modeline-evil-user-state
   ((nil :weight bold
         :foreground ,(doom-color 'bg-alt)
         :background ,(doom-color 'dark-blue))))
  :custom
  (doom-modeline-bar-width 2)
  (doom-modeline-height 15)
  (doom-modeline-buffer-file-name-style 'truncate-upto-root)
  (doom-modeline-buffer-file-state-icon nil)
  (doom-modeline-buffer-modification-icon nil)
  (doom-modeline-window-width-limit nil)
  (doom-modeline-env-version nil)
  (mode-line-right-align-edge 'right-fringe)
  :config

  (defun custom-emacs-state (state)
    (cond
     ((equal state 'normal)   "NORMAL")
     ((equal state 'insert)   "INSERT")
     ((equal state 'replace)  "REPLCE")
     ((equal state 'operator) "OPRTOR")
     ((equal state 'motion)   "MOTION")
     ((equal state 'emacs)    "EMACS")))

  (doom-modeline-def-segment evil
    "Display evil mode states."
    (when (bound-and-true-p evil-mode)
      (let ((tag (cond
                  ((not (evil-visual-state-p)) "")
                  ((eq evil-visual-selection 'block) "")
                  ((eq evil-visual-selection 'line) "")
                  (t "")))
            (face (cond
                   ((evil-normal-state-p) 'doom-modeline-evil-normal-state)
                   ((evil-emacs-state-p) 'doom-modeline-evil-emacs-state)
                   ((evil-insert-state-p) 'doom-modeline-evil-insert-state)
                   ((evil-motion-state-p) 'doom-modeline-evil-motion-state)
                   ((evil-operator-state-p) 'doom-modeline-evil-operator-state)
                   ((evil-replace-state-p) 'doom-modeline-evil-replace-state)
                   ((evil-visual-state-p) 'doom-modeline-evil-visual-state)
                   (t 'doom-modeline-evil-user-state))))
        (propertize
         (format " " tag)
         'face (doom-modeline-face face)
         'help-echo (evil-state-property evil-state :name t)))))

  (doom-modeline-def-segment buffer-info-extra
    `(" "
      mode-line-mule-info
      mode-line-modified
      mode-line-client
      mode-line-remote))

  (doom-modeline-def-segment space
    `(" "))

  (doom-modeline-def-segment minibuffer-depth
    (let* ((depth (minibuffer-depth)))
      (when (> depth 0)
        (format "[%d]" depth))))

  (doom-modeline-def-segment treemacs-peek
    (when treemacs-peek-mode
      (propertize
       "  Peek "
       'face (doom-modeline-face 'doom-modeline-evil-insert-state))))

  (doom-modeline-def-segment dedicated
    (if (window-dedicated-p)
        (propertize
         " "
         'face (doom-modeline-face 'warning))
      "  "))

  (doom-modeline-def-segment purpose
    (if-let* ((purpose (window-parameter (selected-window) 'window-purpose)))
        (propertize
         (format "[%s]" (symbol-name purpose))
         'face (doom-modeline-face 'success))))

  (doom-modeline-def-modeline
    '+default-modeline
    '(evil buffer-info-extra buffer-info dedicated buffer-position)
    '(misc-info minibuffer-depth selection-info lsp repl check major-mode purpose))

  (defun +doom-modeline-set ()
    (doom-modeline-set-modeline '+default-modeline 'default))

  (setq doom-modeline-mode-alist nil)
  :hook
  (doom-modeline-mode . +doom-modeline-set)
  (doom-modeline-mode . column-number-mode)
  (after-init . doom-modeline-mode))

(use-package indent-bars
  :custom
  (indent-bars-no-stipple-char 9615)
  (indent-bars-depth-update-delay 0.1)
  (indent-bars-display-on-blank-lines nil)
  (indent-bars-starting-column 0)
  (indent-bars-color-by-depth nil)
  (indent-bars-color `(,(doom-blend (doom-color 'vertical-bar) (doom-color 'bg) 0.5)))
   ;; '(highlight :face-bg t :blend 0.2))
  (indent-bars-highlight-current-depth nil)
  ;; '(:face default :blend 0.4))
  (indent-bars-pad-frac 0.0)
  (indent-bars-width-frac 0.1)
  (indent-bars-pattern ".")
  (indent-bars-zigzag nil)
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-display-on-blank-lines 'least))

(use-package nil
  :custom-face
  (match
   ((nil :inherit unspecified
         :box nil
         :foreground unspecified
         :background ,(doom-blend (doom-color 'yellow) (doom-color 'bg) 0.3)
         )))
  (isearch
   ((nil :inherit unspecified
         :box nil
         :foreground ,(doom-color 'bg)
         :background ,(doom-color 'yellow))))
  (isearch-fail
   ((nil :inherit unspecified
         :box nil
         :foreground ,(doom-color 'magenta)
         :background unspecified)))
  (isearch-group-1
   ((nil :inherit unspecified
         :box nil
         :foreground ,(doom-color 'fg)
         :background ,(doom-blend (doom-color 'orange) (doom-color 'bg) 0.5)
         )))
  (isearch-group-2
   ((nil :inherit unspecified
         :box nil
         :foreground ,(doom-color 'fg)
         :background ,(doom-blend (doom-color 'red) (doom-color 'bg) 0.5)
         )))
  (popup-isearch-match
   ((nil :inherit unspecified
         :box nil
         :foreground ,(doom-color 'fg)
         :background ,(doom-blend (doom-color 'yellow) (doom-color 'bg) 0.3)
         )))

  (line-number
   ((nil :inherit default
         :weight normal
         :foreground ,(doom-color 'comments))))
  (line-number-current-line
   ((nil :inherit default
         :weight bold
         :foreground ,(doom-color 'orange))))

  (show-paren-match
   ((nil :inherit region
         :weight bold
         :foreground unspecified
         :background unspecified)))
  (show-paren-mismatch
   ((nil :weight bold
         :foreground ,(doom-color 'red))))

  (default
   ((nil :weight normal
         :font ,+fonts-fixed-pitch-face
         :height ,+fonts-fixed-pitch-size)))
  (fixed-pitch
   ((nil :weight normal
         :font ,+fonts-fixed-pitch-face
         :height ,+fonts-fixed-pitch-size)))
  (variable-pitch
   ((nil :weight normal
         :font ,+fonts-variable-pitch-face
         :height ,+fonts-variable-pitch-size)))
  (variable-pitch-text
   ((nil :weight normal
         :font ,+fonts-variable-pitch-face
         :height ,+fonts-variable-pitch-size)))
  (italic
   ((nil :slant italic
         :underline nil
         :font ,+fonts-fixed-pitch-italic-face)))
  (bold-italic
   ((nil :weight bold
         :slant italic
         :underline nil
         :font ,+fonts-fixed-pitch-italic-face)))
  (highlight
   ((nil :extend t
         :background ,(doom-color 'selection)
         :foreground unspecified)))

  (font-lock-comment-face
   ((nil :inherit italic)))

  (popup-border
   ((nil :inherit unspecified
         :foreground ,(doom-darken (doom-blend (doom-color 'red) (doom-color 'orange) 0.3) 0.3)
         :background ,(doom-darken (doom-blend (doom-color 'red) (doom-color 'orange) 0.3) 0.3)
         )))

  (diff-refine-removed
   ((nil  :inverse-video nil
          :foreground ,(doom-color 'red)
          :background ,(doom-blend (doom-color 'red) (doom-color 'bg) 0.2)
          )))

  (diff-refine-added
   ((nil :inverse-video nil
         :foreground ,(doom-color 'green)
         :background ,(doom-blend (doom-color 'green) (doom-color 'bg) 0.2)
         )))
  )

(use-package rainbow-delimiters)

(provide 'packages-interface)

;;; packages-interface.el ends here
