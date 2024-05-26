;;; lib-layouts.el --- Layout  Commands -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-strings)
(require 'util-windows)

(defun +layout-base ()
  "1x1 layout."
  (delete-other-windows)
  (util/window--set-purpose (selected-window) 'edit-main))

(defun +layout-tile ()
  "2x2 layout."
  (delete-other-windows)
  (util/window--set-purpose (selected-window) 'view-info)
  (util/window--set-purpose (split-window-below) 'view-reference)
  (util/window--set-purpose (split-window-right) 'edit-main)
  (other-window 2)
  (util/window--set-purpose (split-window-right) 'edit-general))

(defun +layout-col-2 ()
  "2x1 layout."
  (delete-other-windows)
  (util/window--set-purpose (selected-window) 'view-info)
  (util/window--set-purpose (split-window-right) 'edit-main))

(defun +layout-col-2-left ()
  "2x1 layout with 1x2 left column."
  (delete-other-windows)
  (util/window--set-purpose (selected-window) 'view-info)
  (util/window--set-purpose (split-window-right) 'edit-main)
  (util/window--set-purpose (split-window-below) 'view-reference))

(defun +layout-col-2-right ()
  "2x1 layout with 1x2 right column."
  (delete-other-windows)
  (util/window--set-purpose (selected-window) 'view-info)
  (util/window--set-purpose (split-window-right) 'edit-main)
  (other-window 1)
  (util/window--set-purpose (split-window-below) 'edit-general))

(defun +layout-row-2 ()
  "1x2 layout."
  (delete-other-windows)
  (util/window--set-purpose (selected-window) 'view-info)
  (util/window--set-purpose (split-window-below) 'edit-main))

(defun +layout-row-2-top ()
  "1x2 layout with 2x1 top row."
  (delete-other-windows)
  (util/window--set-purpose (selected-window) 'view-info)
  (util/window--set-purpose (split-window-below) 'edit-main)
  (util/window--set-purpose (selected-window) 'view-log))

(defun +layout-row-2-bottom ()
  "1x2 layout with 2x1 bottom row."
  (delete-other-windows)
  (util/window--set-purpose (selected-window) 'view-log)
  (util/window--set-purpose (split-window-below) 'edit-info)
  (other-window 1)
  (util/window--set-purpose (split-window-right) 'edit-main))

(defun +layout-col-3 ()
  "3x1 layout."
  (delete-other-windows)
  (util/window--set-purpose (selected-window) 'view-info)
  (util/window--set-purpose (split-window-right) 'edit-general)
  (util/window--set-purpose (split-window-right) 'edit-main))

(defun +layout-col-3-left ()
  "3x1 layout with 1x2 left column."
  (delete-other-windows)
  (util/window--set-purpose (selected-window) 'view-info)
  (util/window--set-purpose (split-window-right) 'edit-general)
  (util/window--set-purpose (split-window-right) 'edit-main)
  (util/window--set-purpose (split-window-below) 'view-reference))

(defun +layout-col-3-right ()
  "3x1 layout with 1x2 right column."
  (delete-other-windows)
  (util/window--set-purpose (selected-window) 'view-info)
  (util/window--set-purpose (split-window-right) 'edit-general)
  (util/window--set-purpose (split-window-right) 'edit-main)
  (other-window 2)
  (split-window-below))

(defun +layout-col-3-both ()
  "3x1 layout with 1x2 left/right splits."
  (delete-other-windows)
  (util/window--set-purpose (selected-window) 'view-info)
  (util/window--set-purpose (split-window-right) 'edit-general)
  (util/window--set-purpose (split-window-right) 'edit-main)
  (util/window--set-purpose (split-window-below) 'view-reference)
  (other-window 3)
  (split-window-below))

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

(defun +layout-create-candidate (cand)
  "Create layout candidate from CAND."
  (let* ((layout (car cand))
         (layout-recipe (cdr cand))
         (layout-info (documentation layout-recipe)))
    (list (util/strings-pad-string layout 20) `(:info ,layout-info))))

(defun +layout--annotation-fn (layout)
  "Annotate LAYOUT option with description."
  (let* ((option (car (last (assoc layout minibuffer-completion-table))))
         (info (plist-get option :info)))
    (concat " " (util/strings-add-font-lock info 'font-lock-comment-face))))

(defun +layout-choose-windows-layout ()
  "Select predefined layouts."
  (interactive)
  (if (window-parameter (selected-window) 'window-side)
      (select-window (util/window-get-mru-in-main)))
  (let* ((layouts (mapcar #'+layout-create-candidate +layout-layouts-alist))
         (completion-extra-properties '(:annotation-function +layout--annotation-fn))
         (prompt (format "Choose layout: "))
         (layout (s-trim (completing-read prompt layouts (-const t) t)))
         (recipe (cdr (assoc layout +layout-layouts-alist))))
    (funcall recipe)
    (balance-windows)
    (select-window (window-with-parameter 'window-purpose 'edit-main))))

(provide 'lib-layouts)

;;; lib-layouts.el ends here
