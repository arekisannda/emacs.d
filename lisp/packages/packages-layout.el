;;; packages-layout.el --- Layout Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package emacs
  :ensure nil
  :config
  (defun +layout-base ()
    "1x1 layout."
    (delete-other-windows))

  (defun +layout-tile ()
    "2x2 layout."
    (delete-other-windows)
    (split-window-below)
    (split-window-right)
    (other-window 2)
    (split-window-right)
    (balance-windows)
    (other-window -2))

  (defun +layout-col-2 ()
    "2x1 layout."
    (delete-other-windows)
    (split-window-right)
    (balance-windows))

  (defun +layout-col-2-left ()
    "2x1 layout with 1x2 left column."
    (delete-other-windows)
    (split-window-right)
    (split-window-below)
    (balance-windows))

  (defun +layout-col-2-right ()
    "2x1 layout with 1x2 right column."
    (delete-other-windows)
    (split-window-right)
    (other-window 1)
    (split-window-below)
    (balance-windows)
    (other-window -1))

  (defun +layout-row-2 ()
    "1x2 layout."
    (delete-other-windows)
    (split-window-below)
    (balance-windows))

  (defun +layout-row-2-top ()
    "1x2 layout with 2x1 top row."
    (delete-other-windows)
    (split-window-below)
    (split-window-right)
    (balance-windows))

  (defun +layout-row-2-bottom ()
    "1x2 layout with 2x1 bottom row."
    (delete-other-windows)
    (split-window-below)
    (other-window 1)
    (split-window-right)
    (balance-windows)
    (other-window -1))

  (defun +layout-col-3 ()
    "3x1 layout."
    (delete-other-windows)
    (split-window-right)
    (split-window-right)
    (balance-windows))

  (defun +layout-col-3-left ()
    "3x1 layout with 1x2 left column."
    (delete-other-windows)
    (split-window-right)
    (split-window-right)
    (split-window-below)
    (balance-windows))

  (defun +layout-col-3-right ()
    "3x1 layout with 1x2 right column."
    (delete-other-windows)
    (split-window-right)
    (split-window-right)
    (other-window 2)
    (split-window-below)
    (balance-windows)
    (other-window -2))

  (defun +layout-col-3-both ()
    "3x1 layout with 1x2 left/right splits."
    (delete-other-windows)
    (split-window-right)
    (split-window-right)
    (split-window-below)
    (other-window 3)
    (split-window-below)
    (balance-windows)
    (other-window -3))

  (defvar +layout-layouts-alist
    '(("base"           . +layout-base)
      ("tile"           . +layout-tile)
      ("column-2"       . +layout-col-2)
      ("column-2-left"  . +layout-col-2-left)
      ("column-2-right" . +layout-col-2-right)
      ("column-3-both"  . +layout-col-3-both)
      ("row-2"          . +layout-row-2)
      ("row-2-top"      . +layout-row-2-top)
      ("row-2-bottom"   . +layout-row-2-bottom)
      ("column-3"       . +layout-col-3)
      ("column-3-left"  . +layout-col-3-left)
      ("column-3-right" . +layout-col-3-right)
      ("column-3-both"  . +layout-col-3-both)))

  (defun +layout-create-candidate (layout)
    "Create LAYOUT consult candidate."
    (let* ((layout-recipe (assoc layout +layout-layouts-alist))
           (help-info (documentation (cdr layout-recipe))))
      (concat (propertize (util/strings-pad-string layout 20)
                          'face '(:inherit 'default)
                          'consult--candidate layout-recipe
                          'help-echo help-info)
              " "
              (propertize help-info
                          'face '(:inherit 'font-lock-comment-face :underline '(:style 'line))))))

  (defun +layout-items ()
    "Return itemized list of layout candidates."
    (mapcar #'+layout-create-candidate
            (mapcar #'car +layout-layouts-alist)))

  (defun +layout-action (cand)
    "Execute CAND layout recipe."
    (let ((recipe (get-text-property 0 'consult--candidate cand)))
      (funcall (cdr recipe))))

  (defvar consult--source-layouts
    (list :name     "Layouts"
          :narrow   ?l
          :category 'layouts
          :items    #'+layout-items
          :action   #'+layout-action))

  (defun consult-layouts ()
    "Search for predefined layouts."
    (interactive)
    (require 'consult)
    (consult--multi '(consult--source-layouts)
                    :prompt "Switch layout: "
                    :sort t)))

(provide 'packages-layout)

;;; packages-layout.el ends here
