;;; packages-themes.el --- Emacs Theme Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'packages-colors)

(use-package sonokai-theme :ensure t
  :elpaca (:host github :repo "arekisannda/sonokai-emacs")
  :config
  (defun ui/colors-load-theme-sonokai ()
    (ui/colors-set-color-range "yellow" sonokai-yellow)
    (ui/colors-set-color-range "orange" sonokai-orange)
    (ui/colors-set-color-range "red" sonokai-red)
    (ui/colors-set-color-range "magenta" sonokai-magenta)
    (ui/colors-set-color-range "blue" sonokai-blue)
    (ui/colors-set-color-range "green" sonokai-green)
    (ui/colors-set-color-range "cyan" sonokai-cyan)
    (ui/colors-set-color-range "violet" sonokai-violet)
    (ui/colors-set-color-range "gray" sonokai-gray)
    (set-face-attribute 'ui/colors-white nil
                        :foreground sonokai-white)
    (set-face-attribute 'ui/colors-black nil
                        :foreground sonokai-black))
  (ui/colors-add-load-theme-function 'sonokai #'ui/colors-load-theme-sonokai)
  )

(provide 'packages-themes)

;;; packages-themes.el ends here
