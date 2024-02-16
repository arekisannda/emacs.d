;;; ui-fonts.el --- Emacs Font Configuration -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'packages-init)

(require 'util-helpers)

(defvar ui/fonts-fixed-pitch-face "FiraMono Nerd Font Mono")
(defvar ui/fonts-variable-pitch-face "Fira Sans")
(defvar ui/fonts-fixed-pitch-size 90)
(defvar ui/fonts-variable-pitch-size 90)
(defvar ui/fonts-tab-size 100)
(setq mixed-pitch-set-height ui/fonts-variable-pitch-size)

(defun ui/fonts--custom-faces ()
  "Set up custom font configurations."

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
                      :slant 'italic
                      :underline nil)
  (set-face-attribute 'font-lock-comment-face nil
                      :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil
                      :slant 'italic)
  (set-face-attribute 'tab-bar nil
                      :font ui/fonts-fixed-pitch-face
                      :height ui/fonts-tab-size
                      :weight 'bold)
  (set-face-attribute 'tab-bar-tab nil
                      :font ui/fonts-fixed-pitch-face
                      :height ui/fonts-tab-size
                      :weight 'bold
                      :box '(:line-width (5 . 5) :style flat-button)
                      :underline '(:inherit tab-bar-tab :style line :position 0))
  (set-face-attribute 'tab-bar-tab-inactive nil
                      :font ui/fonts-fixed-pitch-face
                      :height ui/fonts-tab-size
                      :weight 'light
                      :box '(:line-width (5 . 5) :style flat-button)
                      :underline '(:inherit tab-bar-tab :style line :position 0)))

(defun ui/fonts-setup ()
  "Set up fonts configurations."
  (util/dedup-add-to-list
   'default-frame-alist
   `(font . ,(concat ui/fonts-fixed-pitch-face
                     (format "-%d" (/ ui/fonts-fixed-pitch-size 10)))))
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame) (with-selected-frame frame (ui/fonts--custom-faces))))
    (ui/fonts--custom-faces)))

(provide 'ui-fonts)

;;; ui-fonts.el ends here
