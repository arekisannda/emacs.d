;;; layouts.el --- Layout definitions -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(elpaca-wait)
(require 'consult)

(defun configs--layout-base ()
  "Load 1x1 layout."
  (delete-other-windows))

(defun configs--layout-tile ()
  "Load 2x2 layout."
  (delete-other-windows)
  (split-window-below)
  (split-window-right)
  (other-window 2)
  (split-window-right)
  (balance-windows)
  (other-window -2))

(defun configs--layout-col-2 ()
  "Load 2x1 layout."
  (delete-other-windows)
  (split-window-right)
  (balance-windows))

(defun configs--layout-col-2-left ()
  "Load 2x1 layout with 1x2 left column."
  (delete-other-windows)
  (split-window-right)
  (split-window-below)
  (balance-windows))

(defun configs--layout-col-2-right ()
  "Load 2x1 layout with 1x2 right column."
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (split-window-below)
  (balance-windows)
  (other-window -1))

(defun configs--layout-row-2 ()
  "Load 1x2 layout."
  (delete-other-windows)
  (split-window-right)
  (balance-windows))

(defun configs--layout-row-2-top ()
  "Load 1x2 layout with 2x1 top row."
  (delete-other-windows)
  (split-window-below)
  (split-window-right)
  (balance-windows))

(defun configs--layout-row-2-bottom ()
  "Load 1x2 layout with 2x1 bottom row."
  (delete-other-windows)
  (split-window-below)
  (other-window 1)
  (split-window-right)
  (balance-windows)
  (other-window -1))

(defun configs--layout-col-3 ()
  "Load 3x1 layout."
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (balance-windows))

(defun configs--layout-col-3-left ()
  "Load 3x1 layout with 1x2 left column."
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (split-window-below)
  (balance-windows))

(defun configs--layout-col-3-right ()
  "Load 3x1 layout with 1x2 right column."
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (other-window 2)
  (split-window-below)
  (balance-windows)
  (other-window -2))

(defun configs--layout-col-3-both ()
  "Load 3x1 layout with side 1x2 splits."
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (split-window-below)
  (other-window 3)
  (split-window-below)
  (balance-windows)
  (other-window -3))

(defvar configs--layouts-alist
  '(("base"           . configs--layout-base)
    ("tile"           . configs--layout-tile)
    ("column-2"       . configs--layout-col-2)
    ("column-2-left"  . configs--layout-col-2-left)
    ("column-2-right" . configs--layout-col-2-right)
    ("column-3-both"  . configs--layout-col-3-both)
    ("row-2"          . configs--layout-row-2)
    ("row-2-top"      . configs--layout-row-2-top)
    ("row-2-bottom"   . configs--layout-row-2-bottom)
    ("column-3"       . configs--layout-col-3)
    ("column-3-left"  . configs--layout-col-3-left)
    ("column-3-right" . configs--layout-col-3-right)
    ("column-3-both"  . configs--layout-col-3-both)))

(defun configs--layout-create-candidate (layout)
  "Create LAYOUT consult candidate."
  (let* ((layout-recipe (assoc layout configs--layouts-alist))
         (help-info (documentation (cdr layout-recipe))))
    (propertize (format "%s %s" (configs--pad-string layout 20) help-info)
                'consult--candidate layout-recipe
                'help-echo help-info)))

(defun configs--layout-items ()
  "Return itemized list of layout candidates."
  (mapcar #'configs--layout-create-candidate
          (mapcar #'car configs--layouts-alist)))

(defun configs--layout-action (cand)
  "Execute CAND layout recipe."
  (let ((recipe (get-text-property 0 'consult--candidate cand)))
    (funcall (cdr recipe))))

(defvar consult--source-layouts
  (list :name     "Layouts"
        :narrow   ?l
        :category 'layouts
        :items    #'configs--layout-items
        :action   #'configs--layout-action))

(defun consult-layouts ()
  (interactive)
  (consult--multi '(consult--source-layouts)
                  :prompt "Switch layout: "
                  :sort t))

(provide 'configs-layouts)
;;; layouts.el ends here
