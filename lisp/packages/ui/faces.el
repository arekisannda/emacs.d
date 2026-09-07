;;; ui/faces.el -*- lexical-binding: t; -*-

(defface popup-border
  '((((type tty)) :inherit mode-line-inactive))
  "Face used for popup borders."
  :group 'basic-faces)

(defface fold-replacement-face
  '((((type tty)) :inherit 'font-lock-comment-face))
  "Face used for fold replacement face."
  :group 'basic-faces)

(defun emacs-set-alt-face ()
  (face-remap-add-relative 'default `(nil :background ,(doom-color 'bg-alt)))
  (face-remap-add-relative 'markdown-code-face `(nil :background ,(doom-color 'bg-alt)))
  (face-remap-add-relative 'mode-line-active
                           `(nil :inherit mode-line-active
                                 :foreground unspecified
                                 :background ,(doom-color 'bg-alt)))
  (face-remap-add-relative 'mode-line-inactive
                           `(nil :inherit mode-line-active
                                 :foreground unspecified
                                 :background ,(doom-color 'bg-alt))))

(defun emacs-alt-face-setup (window buffer)
  (with-current-buffer buffer (emacs-set-alt-face)))

(defun emacs-alt-face-side-setup (window buffer &optional flags)
  (when (member 'enable-alt-face flags)
    (emacs-alt-face-setup window buffer)))

(defun emacs-tabulated-list-setup ()
  (face-remap-add-relative 'hl-line `(nil :background ,(doom-color 'bg-alt)))
  (face-remap-add-relative 'header-line `(nil :background ,(doom-color 'bg)))
  (hl-line-mode 1))

(use-package emacs ; _faces_
  :ensure nil
  :custom
  (+fonts-ckj-family "Source Han Sans")
  (+fonts-fixed-pitch-family "SauceCodePro NFM")
  (+fonts-fixed-pitch-italic-family "SauceCodePro NFM")
  (+fonts-variable-pitch-family "SauceCodePro NFP")
  (+fonts-fixed-pitch-size 90)
  (+fonts-variable-pitch-size 90)
  (+fonts-tab-size 100)
  :config
  (utils/custom-set-faces
   (shadow
    ((nil :inherit unspecified
          :foreground ,(doom-color 'grey)
          )))
   (header-line
    ((nil :inherit unspecified
          :foreground ,(doom-color 'fg-alt)
          :background ,(doom-color 'bg-alt)
          :underline (:color ,(doom-color 'vertical-bar) :style double-line :position t)
          :box (:line-width (1 . 4) :style flat-button)
          )))

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
          :foreground ,(doom-color 'orange)
          :background unspecified)))

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
          :font ,+fonts-fixed-pitch-family
          :height ,+fonts-fixed-pitch-size)))
   (fixed-pitch
    ((nil :weight normal
          :font ,+fonts-fixed-pitch-family
          :height ,+fonts-fixed-pitch-size)))
   (variable-pitch
    ((nil :weight normal
          :font ,+fonts-variable-pitch-family
          :height ,+fonts-variable-pitch-size)))
   (variable-pitch-text
    ((nil :weight normal
          :font ,+fonts-variable-pitch-family
          :height ,+fonts-variable-pitch-size)))
   (italic
    ((nil :slant italic
          :underline nil
          :font ,+fonts-fixed-pitch-italic-family)))
   (bold-italic
    ((nil :weight bold
          :slant italic
          :underline nil
          :font ,+fonts-fixed-pitch-italic-family)))
   (region
    ((nil :stipple nil :foreground unspecified)))
   (highlight
    ((nil :inherit region
          :background unspecified
          :foreground unspecified)))
   (fringe
    ((nil :foreground ,(doom-color 'fg-alt))))

   (font-lock-comment-face
    ((nil :inherit italic)))

   (popup-border
    ((nil :inherit unspecified
          :foreground "#5a5a5a"
          :background "#5a5a5a"
          )))

   (fold-replacement-face
    ((nil :foreground ,(doom-color 'dark-blue)
          :background ,(doom-blend (doom-color 'dark-blue) (doom-color 'bg) 0.1)
          :weight bold)))

   (diff-removed
    ((nil :background unspecified
          :foreground ,(doom-color 'red))))

   (diff-refine-removed
    ((nil  :inverse-video nil
           :foreground ,(doom-color 'red)
           :background ,(doom-blend (doom-color 'red) (doom-color 'bg) 0.1)
           )))

   (diff-indicator-removed
    ((nil :inherit hl-line
          :background unspecified
          :foreground ,(doom-color 'red))))

   (diff-added
    ((nil :background unspecified
          :foreground ,(doom-color 'green))))

   (diff-refine-added
    ((nil :inverse-video nil
          :foreground ,(doom-color 'green)
          :background ,(doom-blend (doom-color 'green) (doom-color 'bg) 0.1)
          )))

   (diff-indicator-added
    ((nil :inherit hl-line
          :background unspecified
          :foreground ,(doom-color 'green))))

   (bookmark-face
    ((nil :inherit unspecified
          :background unspecified
          :foreground ,(doom-color 'red)))))
  :hook
  (window-setup . +emacs-set-font)
  (tabulated-list-mode . emacs-tabulated-list-setup)
  (util/windows-side-window . emacs-alt-face-side-setup)
  (Custom-mode . emacs-set-alt-face))

(use-package rainbow-delimiters)

(use-package rainbow-mode
  :defer t
  :custom
  (rainbow-r-colors-alist '())
  (rainbow-html-colors-alist '()))

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "SauceCodePro Nerd Font Mono"))
