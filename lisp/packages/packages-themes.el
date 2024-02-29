;;; packages-themes.el --- Emacs Theme Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package ext-tab-bar :after persp-mode
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
  (ext-tab-bar-mode . ext-tab-bar-persp-mode-setup)
  (emacs-startup . ext-tab-bar-mode))

(use-package easy-color-faces
  :ensure (:host github :repo "arekisannda/easy-color-faces"))

(use-package sonokai-theme :after easy-color-faces
  :ensure (:host github :repo "arekisannda/sonokai-emacs")
  :preface
  (defun +themes-sonokai-easy-load-theme ()
    (easy-color-faces-set-colors "yellow" sonokai-yellow)
    (easy-color-faces-set-colors "orange" sonokai-orange)
    (easy-color-faces-set-colors "red" sonokai-red)
    (easy-color-faces-set-colors "magenta" sonokai-magenta)
    (easy-color-faces-set-colors "blue" sonokai-blue)
    (easy-color-faces-set-colors "green" sonokai-green)
    (easy-color-faces-set-colors "cyan" sonokai-cyan)
    (easy-color-faces-set-colors "violet" sonokai-violet)
    (easy-color-faces-set-colors "gray" sonokai-gray)
    (set-face-attribute 'easy-color-faces-white nil
                        :foreground sonokai-white)
    (set-face-attribute 'easy-color-faces-black nil
                        :foreground sonokai-black))

  (add-to-list 'easy-color-theme-function-alist
               '(sonokai . +themes-sonokai-easy-load-theme))

  (defun +themes-load-sonokai ()
    (load-theme 'sonokai t)
    (easy-color-load-theme 'sonokai))

  :hook
  (elpaca-after-init . +themes-load-sonokai))

(use-package emacs :after (sonokai-theme ext-tab-bar easy-color-faces)
  :ensure nil
  :preface
  (defun +themes-configure-fonts ()
    (set-face-attribute 'default nil
                        :font +fonts-fixed-pitch-face
                        :height +fonts-fixed-pitch-size
                        :weight 'normal)
    (set-face-attribute 'fixed-pitch nil
                        :font +fonts-fixed-pitch-face
                        :height +fonts-fixed-pitch-size
                        :weight 'normal)
    (set-face-attribute 'variable-pitch nil
                        :font +fonts-variable-pitch-face
                        :height +fonts-variable-pitch-size
                        :weight 'normal)
    (set-face-attribute 'italic nil
                        :font +fonts-fixed-pitch-italic-face
                        :slant 'italic
                        :underline nil)
    (set-face-attribute 'bold-italic nil
                        :font +fonts-fixed-pitch-italic-face
                        :weight 'bold
                        :slant 'italic
                        :underline nil)
    (set-face-attribute 'font-lock-comment-face nil
                        :inherit 'italic)
    (set-face-attribute 'tab-bar nil
                        :font +fonts-fixed-pitch-face
                        :height +fonts-fixed-pitch-size
                        :weight 'bold
                        :box '(:line-width 5 :style flat-button))
    (set-face-attribute 'tab-bar-tab nil
                        :inherit 'tab-bar
                        :underline `(:color ,(face-foreground 'easy-color-faces-gray-d)
                                            :style line :position 0))
    (set-face-attribute 'tab-bar-tab-inactive nil
                        :inherit 'tab-bar)
    (set-face-attribute 'ext-tab-bar-faces-default nil
                        :font +fonts-fixed-pitch-face
                        :height +fonts-fixed-pitch-size
                        :weight 'normal
                        :box '(:line-width 5 :style flat-button))
    )
  :hook
  (window-setup . +themes-configure-fonts)
  (server-after-make-frame-hook . +themes-configure-fonts))

(provide 'packages-themes)

;;; packages-themes.el ends here
