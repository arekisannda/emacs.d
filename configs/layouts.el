;;; layouts.el --- Layout definitions -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(elpaca-wait)

(defun configs--layout-base ()
  "Load 1x1 layout."
  (delete-other-windows))

(defun configs--layout-col-2 ()
  "Load 2x1 layout."
  (delete-other-windows)
  (split-window-right)
  (balance-windows))

(defun configs--layout-row-2 ()
  "Load 1x2 layout."
  (delete-other-windows)
  (split-window-right)
  (balance-windows))

(defun configs--layout-col-3 ()
  "Load 3x1 layout."
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (balance-windows))

(defun configs--layout-col-2-split-left ()
  "Load 2x1 layout with 1x2 left column."
  (delete-other-windows)
  (split-window-right)
  (split-window-below)
  (balance-windows))

(defun configs--layout-col-3-split-left ()
  "Load 3x1 layout with 1x2 left column."
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (split-window-below)
  (balance-windows))

(defun configs--layout-col-2-split-right ()
  "Load 2x1 layout with 1x2 right column."
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (split-window-below)
  (balance-windows))

(defun configs--layout-col-3-split-right ()
  "Load 3x1 layout with 1x2 right column."
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (other-window 2)
  (split-window-below)
  (balance-windows))

(defun configs--layout-row-2-split-top ()
  "Load 1x2 layout with 2x1 top row."
  (delete-other-windows)
  (split-window-below)
  (split-window-right)
  (balance-windows))

(defun configs--layout-row-2-split-bottom ()
  "Load 1x2 layout with 2x1 bottom row."
  (delete-other-windows)
  (split-window-below)
  (other-window 1)
  (split-window-right)
  (balance-windows))

(defun configs--layout-tile ()
  (delete-other-windows)
  (split-window-below)
  (split-window-right)
  (other-window 2)
  (split-window-right)
  (balance-windows))

(require 'consult)

(defvar configs--layouts-alist
  '(("base" . configs--layout-base)
    ("column-2" . configs--layout-col-2)
    ("column-3" . configs--layout-col-3)
    ("column-2-split-left" . configs--layout-col-2-split-left)
    ("column-2-split-right" . configs--layout-col-2-split-right)
    ("column-3-split-left" . configs--layout-col-3-split-left)
    ("column-3-split-right" . configs--layout-col-3-split-right)
    ("row-2" . configs--layout-row-2)
    ("row-2-split-top" . configs--layout-row-2-split-top)
    ("row-2-split-bottom" . configs--layout-row-2-split-bottom)
    ("tile" . configs--layout-tile)))

(defvar consult--source-layouts
  (list :name     "Layouts"
        :narrow   ?l
        :category 'layouts
        :items    (lambda ()
                    (mapcar
                     (lambda (layout)
                       (propertize (car layout) 'consult--candidate layout))
                     configs--layouts-alist))
	:action   (lambda (cand)
		    (let ((recipe (cdr (assoc cand configs--layouts-alist))))
		      (funcall recipe)))))

(defun consult-layouts ()
  (interactive)
  (consult--multi '(consult--source-layouts)))

(provide 'configs-layouts)
;;; layouts.el ends here
