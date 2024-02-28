;;; packages-themes.el --- Emacs Theme Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package ext-tab-bar :after persp-mode
  :ensure (:host github :repo "arekisannda/ext-tab-bar")
  :init
  (setq ext-tab-bar-project-disable-paths
        (list (expand-file-name elpaca-directory)
              (expand-file-name package-user-dir)))
  :config
  (require 'ext-tab-bar-persp-mode)
  (ext-tab-bar-persp-mode-setup))

(use-package emacs :after ext-tab-bar
  :ensure nil
  :config
  (defun ui/tab-bar-tab-name-format (tab i)
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

  (setq-default tab-bar-close-button-show nil)
  (setq-default tab-bar-new-button-show nil)

  (setq tab-bar-tab-name-format-function #'ui/tab-bar-tab-name-format)
  (setq-default tab-bar-tab-name-truncated-max 60)
  (setq-default tab-bar-auto-width t)
  (setq-default tab-bar-auto-width-max '(400 60))
  (setq-default tab-bar-auto-width-min '(100 15))
  (setq-default tab-bar-separator "")
  (setq tab-bar-format '(tab-bar-format-tabs
                         tab-bar-separator))
  (ext-tab-bar-mode))

(use-package easy-color-faces
  :ensure (:host github :repo "arekisannda/easy-color-faces"))

(use-package sonokai-theme :after easy-color-faces
  :ensure (:host github :repo "arekisannda/sonokai-emacs")
  :config
  (defun packages/themes--sonokai-easy-load-theme ()
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
               '(sonokai . packages/themes--sonokai-easy-load-theme))

  (load-theme 'sonokai t)
  (easy-color-load-theme 'sonokai))

(use-package emacs :after (sonokai-theme ext-tab-bar easy-color-faces)
  :ensure nil
  :config
  (util/if-daemon-run-after-make-frame
   (progn
     (set-face-attribute 'default nil
                         :font ui/fonts-fixed-pitch-face
                         :height ui/fonts-fixed-pitch-size
                         :weight 'normal)
     (set-face-attribute 'fixed-pitch nil
                         :font ui/fonts-fixed-pitch-face
                         :height ui/fonts-fixed-pitch-size
                         :weight 'normal)
     (set-face-attribute 'variable-pitch nil
                         :font ui/fonts-variable-pitch-face
                         :height ui/fonts-variable-pitch-size
                         :weight 'normal)
     (set-face-attribute 'italic nil
                         :font ui/fonts-fixed-pitch-italic-face
                         :slant 'italic
                         :underline nil)
     (set-face-attribute 'bold-italic nil
                         :font ui/fonts-fixed-pitch-italic-face
                         :weight 'bold
                         :slant 'italic
                         :underline nil)
     (set-face-attribute 'font-lock-comment-face nil
                         :inherit 'italic)
     (set-face-attribute 'tab-bar nil
                         :font ui/fonts-fixed-pitch-face
                         :height ui/fonts-fixed-pitch-size
                         :weight 'bold
                         :box '(:line-width 5 :style flat-button))
     (set-face-attribute 'tab-bar-tab nil
                         :inherit 'tab-bar
                         :underline `(:color ,(face-foreground 'easy-color-faces-gray-d)
                                             :style line :position 0))
     (set-face-attribute 'tab-bar-tab-inactive nil
                         :inherit 'tab-bar)
     (set-face-attribute 'ext-tab-bar-faces-default nil
                         :font ui/fonts-fixed-pitch-face
                         :height ui/fonts-fixed-pitch-size
                         :weight 'normal
                         :box '(:line-width 5 :style flat-button))
     )))

(provide 'packages-themes)

;;; packages-themes.el ends here
