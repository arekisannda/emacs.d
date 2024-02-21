;;; packages-themes.el --- Emacs Theme Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defgroup ui/color-faces nil
  "Faces for custom ui colors."
  :group 'faces
  :group 'convenience
  :prefix "ui/color-")

(defface ui/color-yellow
  '((t :foreground "yellow"))
  "Face for yellow."
  :group 'ui/color-faces)
(defface ui/color-orange
  '((t :foreground "orange"))
  "Face for orange."
  :group 'ui/color-faces)
(defface ui/color-red
  '((t :foreground "red"))
  "Face for red."
  :group 'ui/color-faces)
(defface ui/color-magenta
  '((t :foreground "magenta"))
  "Face for magenta."
  :group 'ui/color-faces)
(defface ui/color-blue
  '((t :foreground "blue"))
  "Face for blue."
  :group 'ui/color-faces)
(defface ui/color-green
  '((t :foreground "green"))
  "Face for green."
  :group 'ui/color-faces)
(defface ui/color-cyan
  '((t :foreground "cyan"))
  "Face for cyan."
  :group 'ui/color-faces)
(defface ui/color-violet
  '((t :foreground "violet"))
  "Face for violet."
  :group 'ui/color-faces)
(defface ui/color-grey
  '((t :foreground "grey"))
  "Face for grey."
  :group 'ui/color-faces)
(defface ui/color-white
  '((t :foreground "white"))
  "Face for white."
  :group 'ui/color-faces)
(defface ui/color-black
  '((t :foreground "black"))
  "Face for black."
  :group 'ui/color-faces)

(use-package sonokai-theme :ensure t
  :elpaca (:host github :repo "arekisannda/sonokai-emacs"))

(provide 'packages-themes)

;;; packages-themes.el ends here
