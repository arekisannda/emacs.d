;;; swaywm.el -*- lexical-binding: t; -*-

(require 'util-frames)

;;;###autoload
(defun swaywm/open-new-window ()
  (with-current-buffer (window-buffer (windex-get-mru-in-main))
    (display-buffer (current-buffer)) (beginning-of-line)))

(defvar notes-frame-name "Notes Viewer")

;;;###autoload
(defun swaywm/notes-open (title)
  (util/frames-select-frame-with-params `((+side-frame . t)
                                          (name . ,notes-frame-name))
    (org-roam-node-visit (org-roam-node-from-title-or-alias title t))
    (tab-line-close-other-tabs)))

;;;###autoload
(defun swaywm/notes-create (key title)
  (util/frames-select-frame-with-params `((+side-frame . t)
                                          (name . ,notes-frame-name))
    (let ((buffer (generate-new-buffer "*new*")))
      (set-buffer-major-mode buffer)
      (set-window-buffer nil buffer))
    (org-roam-capture- :keys key :node (org-roam-node-create :title title))
    (tab-line-close-other-tabs)))
