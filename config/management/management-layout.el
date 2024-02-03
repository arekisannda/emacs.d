;;; management-layout.el --- Layout Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'packages-init)
(require 'util-strings)

(defun management/layout--base ()
  "Load 1x1 layout."
  (delete-other-windows))

(defun management/layout--tile ()
  "Load 2x2 layout."
  (delete-other-windows)
  (split-window-below)
  (split-window-right)
  (other-window 2)
  (split-window-right)
  (balance-windows)
  (other-window -2))

(defun management/layout--col-2 ()
  "Load 2x1 layout."
  (delete-other-windows)
  (split-window-right)
  (balance-windows))

(defun management/layout--col-2-left ()
  "Load 2x1 layout with 1x2 left column."
  (delete-other-windows)
  (split-window-right)
  (split-window-below)
  (balance-windows))

(defun management/layout--col-2-right ()
  "Load 2x1 layout with 1x2 right column."
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (split-window-below)
  (balance-windows)
  (other-window -1))

(defun management/layout--row-2 ()
  "Load 1x2 layout."
  (delete-other-windows)
  (split-window-below)
  (balance-windows))

(defun management/layout--row-2-top ()
  "Load 1x2 layout with 2x1 top row."
  (delete-other-windows)
  (split-window-below)
  (split-window-right)
  (balance-windows))

(defun management/layout--row-2-bottom ()
  "Load 1x2 layout with 2x1 bottom row."
  (delete-other-windows)
  (split-window-below)
  (other-window 1)
  (split-window-right)
  (balance-windows)
  (other-window -1))

(defun management/layout--col-3 ()
  "Load 3x1 layout."
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (balance-windows))

(defun management/layout--col-3-left ()
  "Load 3x1 layout with 1x2 left column."
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (split-window-below)
  (balance-windows))

(defun management/layout--col-3-right ()
  "Load 3x1 layout with 1x2 right column."
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (other-window 2)
  (split-window-below)
  (balance-windows)
  (other-window -2))

(defun management/layout--col-3-both ()
  "Load 3x1 layout with side 1x2 splits."
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (split-window-below)
  (other-window 3)
  (split-window-below)
  (balance-windows)
  (other-window -3))

(defvar management/layout--layouts-alist
  '(("base"           . management/layout--base)
    ("tile"           . management/layout--tile)
    ("column-2"       . management/layout--col-2)
    ("column-2-left"  . management/layout--col-2-left)
    ("column-2-right" . management/layout--col-2-right)
    ("column-3-both"  . management/layout--col-3-both)
    ("row-2"          . management/layout--row-2)
    ("row-2-top"      . management/layout--row-2-top)
    ("row-2-bottom"   . management/layout--row-2-bottom)
    ("column-3"       . management/layout--col-3)
    ("column-3-left"  . management/layout--col-3-left)
    ("column-3-right" . management/layout--col-3-right)
    ("column-3-both"  . management/layout--col-3-both)))

(defun management/layout--create-candidate (layout)
  "Create LAYOUT consult candidate."
  (let* ((layout-recipe (assoc layout management/layout--layouts-alist))
         (help-info (documentation (cdr layout-recipe))))
    (concat (propertize (util/strings-pad-string layout 20)
                        'face '(:inherit 'default)
                        'consult--candidate layout-recipe
                        'help-echo help-info)
            " "
            (propertize help-info
                        'face '(:inherit 'shadow :underline '(:style 'line))))))

(defun management/layout--items ()
  "Return itemized list of layout candidates."
  (mapcar #'management/layout--create-candidate
          (mapcar #'car management/layout--layouts-alist)))

(defun management/layout--action (cand)
  "Execute CAND layout recipe."
  (let ((recipe (get-text-property 0 'consult--candidate cand)))
    (funcall (cdr recipe))))

(defvar consult--source-layouts
  (list :name     "Layouts"
        :narrow   ?l
        :category 'layouts
        :items    #'management/layout--items
        :action   #'management/layout--action))

(defun consult-layouts ()
  "Search for predefined layouts."
  (interactive)
  (consult--multi '(consult--source-layouts)
                  :prompt "Switch layout: "
                  :sort t))

(provide 'management-layout)

;;; management-layout.el ends here
