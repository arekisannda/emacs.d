;;; packages-colors.el --- Color Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defgroup ui/colors nil
  "Faces for custom ui colors."
  :group 'faces
  :group 'convenience
  :prefix "ui/colors-")

(defun ui/colors-darker (hex-string percentage)
  "Dim HEX-STRING color by reducing its RGB values by a PERCENTAGE."
  ;; Convert hex string to RGB components
  (let ((red (string-to-number (substring hex-string 1 3) 16))
        (green (string-to-number (substring hex-string 3 5) 16))
        (blue (string-to-number (substring hex-string 5 7) 16)))
    ;; Reduce each component by the given percentage
    (setq red   (truncate (* red   (/ (- 100 percentage) 100.0))))
    (setq green (truncate (* green (/ (- 100 percentage) 100.0))))
    (setq blue  (truncate (* blue  (/ (- 100 percentage) 100.0))))
    ;; Ensure the values are within the valid range [0, 255]
    (setq red   (max 0 (min 255 red)))
    (setq green (max 0 (min 255 green)))
    (setq blue  (max 0 (min 255 blue)))
    ;; Convert the modified RGB values back to hex format
    (format "#%02x%02x%02x" red green blue)))

(defun ui/colors-lighter (hex-string percentage)
  "Brighthen HEX-STRING color by reducing its RGB values by a PERCENTAGE."
  ;; Convert hex string to RGB components
  (let ((red (string-to-number (substring hex-string 1 3) 16))
        (green (string-to-number (substring hex-string 3 5) 16))
        (blue (string-to-number (substring hex-string 5 7) 16)))
    ;; Reduce each component by the given percentage
    (setq red   (truncate (* red   (/ (+ 100 percentage) 100.0))))
    (setq green (truncate (* green (/ (+ 100 percentage) 100.0))))
    (setq blue  (truncate (* blue  (/ (+ 100 percentage) 100.0))))
    ;; Ensure the values are within the valid range [0, 255]
    (setq red   (max 0 (min 255 red)))
    (setq green (max 0 (min 255 green)))
    (setq blue  (max 0 (min 255 blue)))
    ;; Convert the modified RGB values back to hex format
    (format "#%02x%02x%02x" red green blue)))

(defface ui/colors-yellow-l
  '((t :foreground "LightGoldenrod1"))
  "Face for light yellow."
  :group 'ui/colors)

(defface ui/colors-yellow
  '((t :foreground "LightGoldenrod3"))
  "Face for yellow."
  :group 'ui/colors)

(defface ui/colors-yellow-d
  '((t :foreground "LightGoldenrod4"))
  "Face for dark yellow."
  :group 'ui/colors)

(defface ui/colors-orange-l
  '((t :foreground "goldenrod1"))
  "Face for light orange."
  :group 'ui/colors)

(defface ui/colors-orange
  '((t :foreground "DarkGoldenrod1"))
  "Face for orange."
  :group 'ui/colors)

(defface ui/colors-orange-d
  '((t :foreground "DarkGoldenrod3"))
  "Face for dark orange."
  :group 'ui/colors)

(defface ui/colors-red-l
  '((t :foreground "firebrick1"))
  "Face for light red."
  :group 'ui/colors)

(defface ui/colors-red
  '((t :foreground "firebrick3"))
  "Face for red."
  :group 'ui/colors)

(defface ui/colors-red-d
  '((t :foreground "firebrick4"))
  "Face for dark red."
  :group 'ui/colors)

(defface ui/colors-magenta-l
  '((t :foreground "HotPink1"))
  "Face for light magenta."
  :group 'ui/colors)

(defface ui/colors-magenta
  '((t :foreground "HotPink3"))
  "Face for magenta."
  :group 'ui/colors)

(defface ui/colors-magenta-d
  '((t :foreground "HotPink4"))
  "Face for dark magenta."
  :group 'ui/colors)

(defface ui/colors-blue-l
  '((t :foreground "RoyalBlue1"))
  "Face for light blue."
  :group 'ui/colors)

(defface ui/colors-blue
  '((t :foreground "RoyalBlue3"))
  "Face for blue."
  :group 'ui/colors)

(defface ui/colors-blue-d
  '((t :foreground "RoyalBlue4"))
  "Face for dark blue."
  :group 'ui/colors)

(defface ui/colors-green-l
  '((t :foreground "SpringGreen1"))
  "Face for light green."
  :group 'ui/colors)

(defface ui/colors-green
  '((t :foreground "SpringGreen3"))
  "Face for green."
  :group 'ui/colors)

(defface ui/colors-green-d
  '((t :foreground "SpringGreen4"))
  "Face for dark green."
  :group 'ui/colors)

(defface ui/colors-cyan-l
  '((t :foreground "DeepSkyBlue1"))
  "Face for light cyan."
  :group 'ui/colors)

(defface ui/colors-cyan
  '((t :foreground "DeepSkyBlue3"))
  "Face for cyan."
  :group 'ui/colors)

(defface ui/colors-cyan-d
  '((t :foreground "DeepSkyBlue4"))
  "Face for dark cyan."
  :group 'ui/colors)

(defface ui/colors-violet-l
  '((t :foreground "DarkOrchid1"))
  "Face for light violet."
  :group 'ui/colors)

(defface ui/colors-violet
  '((t :foreground "DarkOrchid3"))
  "Face for violet."
  :group 'ui/colors)

(defface ui/colors-violet-d
  '((t :foreground "DarkOrchid4"))
  "Face for dark violet."
  :group 'ui/colors)

(defface ui/colors-gray-l
  '((t :foreground "LightGray"))
  "Face for light gray."
  :group 'ui/colors)

(defface ui/colors-gray
  '((t :foreground "gray"))
  "Face for gray."
  :group 'ui/colors)

(defface ui/colors-gray-d
  '((t :foreground "DimGray"))
  "Face for dark gray."
  :group 'ui/colors)

(defface ui/colors-white
  '((t :foreground "white"))
  "Face for white."
  :group 'ui/colors)

(defface ui/colors-black
  '((t :foreground "black"))
  "Face for black."
  :group 'ui/colors)

(defcustom ui/colors-load-theme-function-alist '()
  "Map of theme to color face load function."
  :type '(alist :key-type symbol
                :value-type function)
  :group 'ui/colors)

(defun ui/colors-load-theme (theme &optional no-confirm no-enable)
  (let ((fn (cdr (assoc theme ui/colors-load-theme-function-alist))))
    (unless (null fn)
      (funcall fn))))

(eval-when-compile
  (defmacro ui/colors-add-load-theme-function (theme function)
    `(setq ui/colors-load-theme-function-alist
           (cons (cons ,theme ,function) ui/colors-load-theme-function-alist)))

  (defmacro ui/colors-set-color-range (color-name color-value)
    "Set colors faces - light, normal, and dim - for base COLOR-NAME of COLOR-VALUE."
    `(progn
       (set-face-attribute (intern (format "ui/colors-%s-l" ,color-name)) nil
                           :foreground (ui/colors-lighter ,color-value 25))
       (set-face-attribute (intern (format "ui/colors-%s" ,color-name)) nil
                           :foreground ,color-value)
       (set-face-attribute (intern (format "ui/colors-%s-d" ,color-name)) nil
                           :foreground (ui/colors-darker ,color-value 25))))
  )

(provide 'packages-colors)

;;; packages-colors.el ends here
