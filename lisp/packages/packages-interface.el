;;; packages-interface.el --- Interface Packages Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-helpers)
(require 'util-windows)


(defvar +fonts-fixed-pitch-face "SauceCodePro NFM")
(defvar +fonts-fixed-pitch-italic-face "SauceCodePro NFM")
(defvar +fonts-variable-pitch-face "SauceCodePro NFP")
(set-frame-font "SauceCodePro NFP 9" nil t)

(defvar +fonts-fixed-pitch-size 90)
(defvar +fonts-variable-pitch-size 90)
(defvar +fonts-tab-size 100)

(add-to-list
 'default-frame-alist
 `(font . ,(concat +fonts-fixed-pitch-face
                   (format "-%d" (/ +fonts-fixed-pitch-size 10)))))

(use-package helpful)

(use-package rainbow-delimiters)

(use-package rainbow-mode
  :custom
  (rainbow-r-colors-alist '())
  (rainbow-html-colors-alist '()))

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
      (let (
            ;; (tag (cond
            ;;       ((not (evil-visual-state-p)) (upcase (symbol-name evil-state)))
            ;;       ((eq evil-visual-selection 'block) "V-BLOCK")
            ;;       ((eq evil-visual-selection 'line) "V-LINE")
            ;;       (t "VISUAL")))
            (tag (cond
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

  (doom-modeline-def-modeline
    '+treemacs-modeline
    '(workspace-name)
    '(treemacs-peek))

  (doom-modeline-def-modeline
    '+default-modeline
    '(evil vcs process buffer-info-extra buffer-info buffer-position)
    '(misc-info minibuffer-depth matches selection-info lsp repl check major-mode))

  (defun +doom-modeline-set ()
    (doom-modeline-set-modeline '+default-modeline 'default))

  (setq doom-modeline-mode-alist nil)
  :hook
  (doom-modeline-mode . +doom-modeline-set)
  (doom-modeline-mode . column-number-mode)
  (elpaca-after-init . doom-modeline-mode))

(use-package ext-tab-bar :disabled
  :ensure (:host github :repo "arekisannda/ext-tab-bar")
  :preface
  (defun +ext-tab-bar-name-format (tab i)
    (let ((current-p (eq (car tab) 'current-tab)))
      (propertize
       (concat (if tab-bar-tab-hints (format " %d " i) " ")
               (truncate-string-to-width
                (alist-get 'name tab)
                tab-bar-tab-name-truncated-max nil nil
                tab-bar-tab-name-ellipsis)
               (or (and tab-bar-close-button-show
                        (not (eq tab-bar-close-button-show
                                 (if current-p 'non-selected 'selected)))
                        tab-bar-close-button)
                   ""))
       'face (funcall tab-bar-tab-face-function tab))))

  (defun +ext-tab-bar-customize-face ()
    (util/custom-faces
     (tab-bar
      ((nil :weight bold
            :underline nil
            :box (:line-width 5 :style flat-button)
            :font ,+fonts-fixed-pitch-face
            :height ,+fonts-fixed-pitch-size)))
     (tab-bar-tab
      ((nil :inherit tab-bar
            :underline (:color ,(doom-color 'vertical-bar)
                               :style line :position 0))))
     (tab-bar-tab-inactive
      ((nil :inherit tab-bar)))
     (ext-tab-bar-faces-default
      ((nil :weight normal
            :box (:line-width 5 :style flat-button)
            :font ,+fonts-fixed-pitch-face
            :height ,+fonts-fixed-pitch-size)))
     (ext-tab-bar-faces-project
      ((nil :foreground ,(doom-darken (doom-color 'green) 0.2))))
     (ext-tab-bar-faces-debug
      ((nil :foreground ,(doom-darken (doom-color 'red) 0.2))))))
  :custom
  (tab-bar-tab-name-format-function #'+ext-tab-bar-name-format)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-tab-name-truncated-max 60)
  (tab-bar-auto-width t)
  (tab-bar-auto-width-max '(400 60))
  (tab-bar-auto-width-min '(100 15))
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (ext-tab-bar-project-disable-paths (list (expand-file-name elpaca-directory)
                                           (expand-file-name package-user-dir)))
  :hook
  (elpaca-after-init . ext-tab-bar-mode)
  (ext-tab-bar-mode . (lambda ()
                        (+ext-tab-bar-customize-face)
                        (setq tab-bar-map (make-sparse-keymap))
                        (setq tab-bar-mode-map (make-sparse-keymap)))))

(use-package indent-bars
  :custom
  (indent-bars-no-stipple-char 9615)
  (indent-bars-depth-update-delay 0.1)
  (indent-bars-display-on-blank-lines nil)
  (indent-bars-starting-column 0)
  (indent-bars-color-by-depth nil)
  (indent-bars-color '(highlight :face-bg t :blend 0.2))
  (indent-bars-highlight-current-depth nil)
  ;; '(:face default :blend 0.4))
  (indent-bars-pad-frac 0.0)
  (indent-bars-width-frac 0.1)
  (indent-bars-pattern ".")
  (indent-bars-zigzag nil)
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-display-on-blank-lines 'least))

(use-package diff-hl :after magit
  :ensure (:type git :host github :repo "arekisannda/diff-hl" :branch "master")
  :custom
  (diff-hl-show-staged-changes nil)
  (diff-hl-flydiff-delay 0.1)
  :init
  (setq diff-hl-show-hunk-map (make-sparse-keymap)
        diff-hl-inline-popup-transient-mode-map (make-sparse-keymap))
  :hook
  (window-setup . diff-hl-flydiff-mode)
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(defun +delayed-customize-face ()
  "Delay customize face."
  (util/custom-faces
   ;; general
   (match
    ((nil :inherit unspecified
          :box nil
          :foreground ,(doom-color 'bg)
          :background ,(doom-color 'fg))))
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
          :background ,(doom-color 'red))))
   (isearch-group-2
    ((nil :inherit unspecified
          :box nil
          :foreground ,(doom-color 'fg)
          :background ,(doom-color 'magenta))))
   (popup-isearch-match
    ((nil :inherit unspecified
          :box nil
          :foreground ,(doom-color 'fg)
          :background ,(doom-color 'yellow))))

   (line-number
    ((nil :inherit default
          :weight normal
          :foreground ,(doom-color 'comments))))
   (line-number-current-line
    ((nil :inherit default
          :weight bold
          :foreground ,(doom-color 'orange))))

   (show-paren-match
    ((nil :inherit shadow
          :weight bold
          :inverse-video t
          :foreground ,(doom-color 'green))))
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
   (italic
    ((nil :slant italic
          :underline nil
          :font ,+fonts-fixed-pitch-italic-face)))
   (bold-italic
    ((nil :weight bold
          :slant italic
          :underline nil
          :font ,+fonts-fixed-pitch-italic-face)))
   (font-lock-comment-face
    ((nil :inherit italic)))
   ))

(add-hook 'elpaca-after-init-hook #'+delayed-customize-face)

(provide 'packages-interface)

;;; packages-interface.el ends here
