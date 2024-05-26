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

(cl-defun util/window-with-parameters (parameters &optional frame)
  "Get first window on FRAME with PARAMETERS."
  (let* ((frame (window-normalize-frame frame)))
    (let (matching-windows)
      (walk-windows
       (lambda (window)
         (when (cl-every
                (lambda (parameter)
                  (eq (window-parameter window (car parameter)) (cdr parameter)))
                parameters)
           (cl-return-from util/window-with-parameters window)))
       'no-minibuf))))

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
                 (or (not (window-parameter window 'window-side))))
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
                 (or (not (window-parameter window 'window-side))))
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

(defun util/window-display-buffer-in-directed-side-window (buffer alist)
  "Custom display BUFFER in side window.
ALIST is an association list of action symbols and values."
  (let* ((side (or (cdr (assq 'side alist)) 'bottom))
         (slot (or (cdr (assq 'slot alist)) 0))
         (direction (or (cdr (assq 'direction alist)) 'vertical))
         (left-or-right (memq side '(left right))))
    (cond
     ((not (memq side '(top bottom left right)))
      (error "Invalid side %s specified" side))
     ((not (numberp slot))
      (error "Invalid slot %s specified" slot)))

    (let* ((major (window-with-parameter 'window-side side nil t))
           ;; `major' is the major window on SIDE, `windows' the list of
           ;; life windows on SIDE.
           (reversed (window--sides-reverse-on-frame-p (selected-frame)))
           (windows
            (cond
             ((window-live-p major)
              (list major))
             ((window-valid-p major)
              (let* ((first (window-child major))
                     (next (window-next-sibling first))
                     (windows (list next first)))
                (setq reversed (> (window-parameter first 'window-slot)
                                  (window-parameter next 'window-slot)))
                (while (setq next (window-next-sibling next))
                  (setq windows (cons next windows)))
                (if reversed windows (nreverse windows))))))
           (slots (when major (max 1 (window-child-count major))))
           (max-slots
            (nth (cond
                  ((eq side 'left) 0)
                  ((eq side 'top) 1)
                  ((eq side 'right) 2)
                  ((eq side 'bottom) 3))
                 window-sides-slots))
           (window--sides-inhibit-check t)
           (alist (if (assq 'dedicated alist)
                      alist
                    (cons `(dedicated . ,(or display-buffer-mark-dedicated 'side))
                          alist)))
           window this-window this-slot prev-window next-window
           best-window best-slot abs-slot)

      (cond
       ((and (numberp max-slots) (<= max-slots 0))
        ;; No side-slots available on this side.  Don't raise an error,
        ;; just return nil.
        nil)
       ((not windows)
        ;; No major side window exists on this side, make one.
        (window--make-major-side-window buffer side slot alist))
       (t
        ;; Scan windows on SIDE.
        (catch 'found
          (dolist (window windows)
            (setq this-slot (window-parameter window 'window-slot))
            (cond
             ;; The following should not happen and probably be checked
             ;; by window--sides-check.
             ((not (numberp this-slot)))
             ((= this-slot slot)
              ;; A window with a matching slot has been found.
              (setq this-window window)
              (throw 'found t))
             (t
              ;; Check if this window has a better slot value wrt the
              ;; slot of the window we want.
              (setq abs-slot
                    (if (or (and (> this-slot 0) (> slot 0))
                            (and (< this-slot 0) (< slot 0)))
                        (abs (- slot this-slot))
                      (+ (abs slot) (abs this-slot))))
              (unless (and best-slot (<= best-slot abs-slot))
                (setq best-window window)
                (setq best-slot abs-slot))
              (if reversed
                  (cond
                   ((<= this-slot slot)
                    (setq next-window window))
                   ((not prev-window)
                    (setq prev-window window)))
                (cond
                 ((<= this-slot slot)
                  (setq prev-window window))
                 ((not next-window)
                  (setq next-window window))))))))

        ;; `this-window' is the first window with the same SLOT.
        ;; `prev-window' is the window with the largest slot < SLOT.  A new
        ;; window will be created after it.
        ;; `next-window' is the window with the smallest slot > SLOT.  A new
        ;; window will be created before it.
        ;; `best-window' is the window with the smallest absolute difference
        ;; of its slot and SLOT.
        (or (and this-window
                 ;; Reuse `this-window'.
                 (with-current-buffer buffer
                   (setq window--sides-shown t))
                 (window--display-buffer buffer this-window 'reuse alist))
            (and (or (not max-slots) (< slots max-slots))
                 (or (and next-window
                          ;; Make new window before `next-window'.
                          (let ((next-side (if (equal direction 'horizontal) 'left 'above))
                                (window-combination-resize 'side))
                            (setq window (split-window-no-error
                                          next-window nil next-side))))
                     (and prev-window
                          ;; Make new window after `prev-window'.
                          (let ((prev-side (if (equal direction 'horizontal) 'right 'below))
                                (window-combination-resize 'side))
                            (setq window (split-window-no-error
                                          prev-window nil prev-side)))))
                 (set-window-parameter window 'window-slot slot)
                 (with-current-buffer buffer
                   (setq window--sides-shown t))
                 (window--display-buffer buffer window 'window alist))
            (and best-window
                 ;; Reuse `best-window'.
                 (progn
                   ;; Give best-window the new slot value.
                   (set-window-parameter best-window 'window-slot slot)
                   (with-current-buffer buffer
                     (setq window--sides-shown t))
                   (window--display-buffer
                    buffer best-window 'reuse alist)))))))))

(provide 'util-windows)

;;; util-windows.el ends here
