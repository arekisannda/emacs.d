;;; swaywm.el -*- lexical-binding: t; -*-

(require 'util-frames)
(require 'util-windows)

;;;###autoload
(defun swaywm/open-new-window (&optional dashboard-p)
  (let ((+shackle-ignore-checks t)
        (last-mru-window (util/windows-get-mru-in-main t nil t)))
    (if dashboard-p
        (dashboard-open)
      (switch-to-buffer (window-buffer last-mru-window))
      (tab-line-close-other-tabs))
    ))

(defvar notes-frame-name "[Note Viewer] ")

(defconst notes-frame-parameters `((+side-frame . t)
                                   (prefix . ,notes-frame-name)
                                   (min-width . 120)
                                   (width . 120)
                                   (popup . t)))

;;;###autoload
(defun swaywm/notes-open (title)
  (util/frames-select-frame-with-params notes-frame-parameters
    (org-roam-node-visit (org-roam-node-from-title-or-alias title t))))

;;;###autoload
(defun swaywm/notes-create (key title)
  (util/frames-select-frame-with-params notes-frame-parameters
    (let ((buffer (generate-new-buffer "*new*")))
      (set-buffer-major-mode buffer)
      (set-window-buffer nil buffer))
    (org-roam-capture- :keys key :node (org-roam-node-create :title title))))

(with-eval-after-load 'windex
  (defun swaywm/show-scratchpad-frame (&rest args)
    (call-process-shell-command "swaymsg '[floating app_id=\"^.*emacs.*$\"] scratchpad show'"))

  (advice-add #'windex-frame-display-buffer :after #'swaywm/show-scratchpad-frame))
