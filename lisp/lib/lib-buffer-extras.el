;;; lib-buffer-extras.el --- Additional Buffer Commands -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(defvar +buffer-scroll-lines 5)
(defvar +buffer-hscroll-lines 5)

(defcustom +buffer-scroll-left-function nil
  "Function used to scroll left."
  :type 'function
  :group 'buffer
  :group 'convenience)

(defcustom +buffer-scroll-right-function nil
  "Function used to scroll right."
  :type 'function
  :group 'buffer
  :group 'convenience)

(defcustom +buffer-scroll-up-function nil
  "Function used to scroll up."
  :type 'function
  :group 'buffer
  :group 'convenience)

(defcustom +buffer-scroll-down-function nil
  "Function used to scroll down."
  :type 'function
  :group 'buffer
  :group 'convenience)

(defun +buffer-scroll-left ()
  "Scroll window left."
  (interactive)
  (unless +buffer-scroll-left-function (error "Scroll function not set"))
  (funcall +buffer-scroll-left-function +buffer-hscroll-lines))

(defun +buffer-scroll-right ()
  "Scroll window right."
  (interactive)
  (unless +buffer-scroll-right-function (error "Scroll function not set"))
  (funcall +buffer-scroll-right-function +buffer-hscroll-lines))

(defun +buffer-scroll-down ()
  "Scroll window down."
  (interactive)
  (unless +buffer-scroll-down-function (error "Scroll function not set"))
  (funcall +buffer-scroll-down-function +buffer-scroll-lines))

(defun +buffer-scroll-up ()
  "Scroll window up."
  (interactive)
  (unless +buffer-scroll-up-function (error "Scroll function not set"))
  (funcall +buffer-scroll-up-function +buffer-scroll-lines))

(defcustom +buffer-other-window-selector nil
  "Function used to select other window."
  :type 'function
  :group 'convenience)

(defmacro +buffer-with-other-window (&rest body)
  "Run BODY with other window."
  `(let* ((window (and (functionp +buffer-other-window-selector)
                       (funcall +buffer-other-window-selector))))
     (when window
       (with-selected-window window ,@body))))

(defun +buffer-scroll-other-down ()
  "Scroll other window down."
  (interactive)
  (+buffer-with-other-window
   (funcall (or (command-remapping #'scroll-up-command)
                #'scroll-up-command)
            +buffer-scroll-lines)))

(defun +buffer-scroll-other-up ()
  "Scroll other window up."
  (interactive)
  (+buffer-with-other-window
   (funcall (or (command-remapping #'scroll-down-command)
                #'scroll-down-command)
            +buffer-scroll-lines)))

(defun +buffer-minibuffer-scroll-other-down ()
  "Scroll other window down."
  (interactive)
  (minibuffer-scroll-other-window +buffer-scroll-lines))

(defun +buffer-minibuffer-scroll-other-up ()
  "Scroll other window up."
  (interactive)
  (minibuffer-scroll-other-window-down +buffer-scroll-lines))

(defmacro +buffer-scroll-line-to (ppt)
  `(defun ,(intern (format "+buffer-scroll-line-to-%d-ppt" ppt)) ()
     ,(format "Scroll current to %d%% of window" ppt)
     (interactive)
     (when-let* ((buffer-height (count-lines (point-min) (point-max)))
                 (_ (> buffer-height (window-height)))
                 (window-ppt (/ ,ppt 100.0))
                 (window-start-line (line-number-at-pos (window-start)))
                 (target-line (+ window-start-line (ceiling (* (window-height) window-ppt))))
                 (current-line (line-number-at-pos))
                 (scroll-count (- target-line current-line)))
       (scroll-down-line scroll-count))))

(provide 'lib-buffer-extras)

;;; lib-buffer-extras.el ends here
