;;; layouts.el --- Layout definitions -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'evil-commands)

(defun configs--layout-base ()
  "Load 1x1 layout."
  (interactive)
  (delete-other-windows))

(defun configs--layout-col-2 ()
  "Load 2x1 layout."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (balance-windows))

(defun configs--layout-col-3 ()
  "Load 3x1 layout."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (balance-windows))

(defun configs--layout-col-2-split-left ()
  "Load 2x1 layout with 1x2 left column."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (split-window-below)
  (balance-windows))

(defun configs--layout-col-3-split-left ()
  "Load 3x1 layout with 1x2 left column."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (split-window-below)
  (balance-windows))

(defun configs--layout-col-2-split-right ()
  "Load 2x1 layout with 1x2 right column."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (split-window-below)
  (balance-windows))

(defun configs--layout-col-3-split-right ()
  "Load 3x1 layout with 1x2 right column."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (other-window 2)
  (split-window-below)
  (balance-windows))

(defun configs--layout-row-2-split-top ()
  "Load 1x2 layout with 2x1 top row."
  (interactive)
  (delete-other-windows)
  (split-window-below)
  (split-window-right)
  (balance-windows))

(defun configs--layout-row-2-split-bottom ()
  "Load 1x2 layout with 2x1 bottom row."
  (interactive)
  (delete-other-windows)
  (split-window-below)
  (other-window 1)
  (split-window-right)
  (balance-windows))

(defun configs--layout-tile ()
  (interactive)
  (delete-other-windows)
  (split-window-below)
  (split-window-right)
  (other-window 2)
  (split-window-right)
  (balance-windows))

(provide 'configs-layouts)
;;; layouts.el ends here
