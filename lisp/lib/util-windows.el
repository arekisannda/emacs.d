;;; util-windows.el --- Window Helper Methods -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl-lib)

;;; ##############
;;; Window Purpose
;;; ##############

(defcustom +window-purpose-list '(edit-main
                                  edit-general
                                  view-info
                                  view-reference
                                  view-log)
  "A list of purpose names."
  :type '(repeat symbol)
  :group 'convenience)

(defun util/window--set-purpose (&optional window purpose)
  "Set window parameter`window-purpose` for WINDOW to PURPOSE."
  (if purpose
      (set-window-parameter window 'window-purpose purpose)
    (let* ((purpose-list (mapcar #'symbol-name +window-purpose-list))
           (prompt (format "Set purpose: "))
           (purpose (s-trim (completing-read prompt purpose-list (-const t) t))))
      (set-window-parameter window 'window-purpose (intern purpose)))))

(defun util/window--unset-purpose (&optional window)
  "Unset window parameter`window-purpose` for WINDOW."
  (set-window-parameter window 'window-purpose nil))

(defun util/window-get-purpose (&optional window)
  "Get value of window parameter `window-purpose.
If WINDOW is nil, use currently selected window."
  (window-parameter window 'window-purpose))

;;; #############
;;; Window Select
;;; #############

(defun util/window-popup-p (&optional window)
  "Return t if WINDOW is a popup window."
  (interactive)
  (if (window-parameter window 'window-popup) t nil))

(defun util/window-side-p (&optional window)
  "Return t if WINDOW is a side window."
  (interactive)
  (if (and (window-parameter window 'window-side)
           (window-parameter window 'window-slot))
      t nil))

(defun util/window-main-p (&optional window)
  "Return t if WINDOW is a main window."
  (interactive)
  (not (or (util/window-side-p) (util/window-popup-p))))

(cl-defun util/window-with-parameters (parameters &optional frame first)
  "Get windows in FRAME with PARAMETERS.
If FIRST is non-nil, return first window."
  (let* ((frame (window-normalize-frame frame)))
    (let (matching-windows)
      (walk-windows
       (lambda (window)
         (when (cl-every
                (lambda (parameter)
                  (eq (window-parameter window (car parameter)) (cdr parameter)))
                parameters)
           (if first
               (cl-return-from util/window-with-parameters window)
             (push window matching-windows))))
       'no-minibuf)
      matching-windows)))

(cl-defun util/window-with-parameters-delete (parameters &optional frame)
  "Delete windows in FRAME with PARAMETERS."
  (cl-loop for window in (util/window-with-parameters parameters frame) do
           (delete-window window)))

(defun util/window-get-mru-in-main (&optional all-frames dedicated not-selected no-other)
  "Get most recently used main window."
  (let (best-window best-time time)
    (dolist (window (window-list-1 nil 'nomini all-frames))
      (setq time (window-use-time window))
      (when (and (or dedicated (not (window-dedicated-p window)))
                 (or (not not-selected) (not (eq window (selected-window))))
                 (or (not no-other)
                     (not (window-parameter window 'no-other-window)))
                 (or (not best-time) (> time best-time))
                 (or (not (window-parameter window 'window-side)))
                 (or (not (window-parameter window 'window-popup))))
        (setq best-time time)
        (setq best-window window)))
    best-window))

(defun util/window-get-lru-in-main (&optional all-frames dedicated not-selected no-other)
  "Get least recently used main window."
  (let ((windows (window-list-1 nil 'nomini all-frames))
        best-window best-time second-best-window second-best-time time)
    (dolist (window windows)
      (when (and (or dedicated (not (window-dedicated-p window)))
                 (or (not not-selected) (not (eq window (selected-window))))
                 (or (not no-other)
                     (not (window-parameter window 'no-other-window)))
                 (or (not (window-parameter window 'window-side)))
                 (or (not (window-parameter window 'window-popup))))
        (setq time (window-use-time window))
        (if (or (eq window (selected-window))
                (not (window-full-width-p window)))
            (when (or (not second-best-time) (< time second-best-time))
              (setq second-best-time time)
              (setq second-best-window window))
          (when (or (not best-time) (< time best-time))
            (setq best-time time)
            (setq best-window window)))))
    (or best-window second-best-window)))

(defun util/window-toggle-window (parameters &optional init-buffer)
  "Helper function for toggling between main and side window with PARAMETERS.

If INIT-BUFFER is non-nil, when a side-window does not exist, a new side-window
will be created using INIT-BUFFER.  INIT-BUFFER can be a buffer name (string),
a buffer object, or a function."
  (let* ((init-window (selected-window))
         (side-window (util/window-with-parameters parameters nil t)))

    (cond
     ((eq side-window init-window)
      ;; if selected window is the right side-window
      ;; (util/window-with-parameters-delete parameters)
      (select-window (util/window-get-mru-in-main)))

     (side-window (select-window side-window))

     ((functionp init-buffer)
      (select-buffer (funcall init-buffer)))

     ((bufferp init-buffer)
      (display-buffer init-buffer)
      (select-window (get-buffer-window init-buffer)))

     ((stringp init-buffer)
      (select-buffer (get-buffer-create init-buffer)))
     )))

(provide 'util-windows)

;;; util-windows.el ends here
