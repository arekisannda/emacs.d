;;; lib-window-extras.el --- Additional Windows Commands -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'util-windows)

(setq-default window-sides-slots '(3 0 3 1))
(setq-default window-sides-vertical nil)
(setq-default window-persistent-parameters
              '((window-slot . writable) ;
                (window-side . writable)
                (window-purpose . writable)
                (window-popup . writable)
                (clone-of . t)
                (no-delete-other-windows . t)))

(defun +toggle-dedicated-window-buffer (&optional window)
  "Toggle window WINDOW's dedication to its current buffer on or off.
WINDOW defaults to the selected window."
  (interactive)
  (let* ((flag (not (window-dedicated-p window))))
    (set-window-dedicated-p window flag)
    (if flag
        (message "Window buffer is now dedicated")
      (message "Window buffer is not dedicated anymore"))
    (force-mode-line-update)
    flag))

(defun +window-set-purpose (&optional prefix)
  "Set/unset window purpose.

With prefix PREFIX \\[universal-argument], remove window purpose.
With double-prefix PREFIX \\[universal-argument], echo window purpose."
  (interactive "p")
  (pcase prefix
    (4 (util/window--unset-purpose))
    (16 (message "window-purpose: %s" (util/window-get-purpose)))
    (_ (util/window--set-purpose))))

(defun +window-with-purpose (&optional purpose noselect)
  "Select window with `window-purpose` value PURPOSE.

If NOSELECT is nil, select and return window."
  (interactive)
  (let* ((purpose-list (mapcar #'symbol-name +window-purpose-list))
         (prompt (format "Select purpose: "))
         (purpose (or purpose (s-trim (completing-read prompt purpose-list (-const t) t)))))
    (if-let ((window (window-with-parameter 'window-purpose
                                            (cond ((stringp purpose) (intern purpose))
                                                  ((symbolp purpose) purpose)))))
        (if noselect window (select-window window)))))


(defcustom +window-init-right-side-window-function  nil
  "Function used to set initial right side-window buffer if one does not exist."
  :type 'function
  :group 'window
  :group 'convenience)

(defcustom +window-init-bottom-side-window-function nil
  "Function used to set initial bottom side-window buffer if one does not exist."
  :type 'function
  :group 'window
  :group 'convenience)

(defun +window-toggle-right-side-window ()
  "Toggle right side-window."
  (interactive)
  (util/window-toggle-window '((window-side . right))
                             +window-init-right-side-window-function))

(defun +window-select-right-side-window (&optional prefix)
  "Select right side-window.

With double-prefix PREFIX \\[universal-argument], delete right side windows."
  (interactive "p")
  (let ((params '((window-side . right))))
    (pcase prefix
      (16 (util/window-with-parameters-delete params))
      (_ (select-window (util/window-with-parameters params nil t))))))

(defun +window-toggle-bottom-side-window ()
  "Toggle bottom side-window."
  (interactive)
  (util/window-toggle-window '((window-popup . below))
                             +window-init-bottom-side-window-function))

(defun +window-select-bottom-side-window (&optional prefix)
  "Select bottom side-window.

With double-prefix PREFIX \\[universal-argument], delete bottom side-windows."
  (interactive "p")
  (let ((params '((window-popup . below))))
    (pcase prefix
      (16 (util/window-with-parameters-delete params))
      (_ (select-window (util/window-with-parameters params nil t))))))

(defun +window-kill-non-main-windows ()
  "Kill side windows and pop up windows."
  (interactive)
  (walk-windows
   (lambda (window)
     (if (or (window-parameter window 'window-side)
             (window-parameter window 'window-popup))
         (delete-window window)))
   'nomini
   nil))

(defun +display-popup-disable-split-window (&optional _window _size _side)
  "Disable `split-window` of popup window."
  (user-error "Cannot split popup window"))

(defun +display-buffer-in-popup-window (buffer &optional alist plist)
  "Display BUFFER in popup window according to ALIST and PLIST."
  (if (plist-get plist :ignore) 'fail
    (let* ((side (plist-get plist :side))
           (size (plist-get plist :size))
           (fixed (plist-get plist :fixed))
           (init-window (selected-window))
           state
           parameters
           window)

      (if side
          (setq parameters `((window-popup . ,side)))
        (user-error "Missing popup window parameters"))

      (cond
       ;; if reuse flag is set and if buffer is visible, reuse the window
       ((and (setq window (get-buffer-window buffer)) (plist-get plist :reuse))
        (select-window window))
       ((setq window (util/window-with-parameters parameters nil t))
        (setq state 'reuse-window))
       ((setq window (split-window (window-main-window) (if size (- size)) side nil))
        (setq state 'create-window))
       (t (user-error "Unable to create popup window")))

      (when (plist-get plist :disable-modeline)
        (set-window-parameter window 'mode-line-format 'none))

      (set-window-parameter window 'no-delete-other-windows t)
      (set-window-parameter window 'window-popup side)
      (set-window-parameter window 'split-window #'+display-popup-disable-split-window)

      (set-window-buffer window buffer)

      (with-current-buffer buffer
        (setq-local window-size-fixed fixed))

      (if (plist-get plist :select) window init-window))))

(defun +display-buffer-in-side-window (buffer &optional alist plist) ;
  "Display buffer in side window according to ALIST and PLIST."
  (if (plist-get plist :ignore) 'fail
    (let* ((side (plist-get plist :side))
           (slot (plist-get plist :slot))
           (size (plist-get plist :size))
           (fixed (plist-get plist :fixed))
           (init-window (selected-window))
           parameters
           window)

      (if (and side slot)
          (setq parameters `((window-side . ,side) (window-slot . ,slot)))
        (user-error "Missing side window parameters"))

      (cond
       ;; if reuse flag is set and if buffer is visible, reuse the window
       ((and (setq window (get-buffer-window buffer)) (plist-get plist :reuse))
        (select-window window))
       ((setq window
              (display-buffer-in-side-window
               buffer
               (append alist
                       `((dedicated                . ,(plist-get plist :dedicated))
                         (direction                . ,(plist-get plist :direction))
                         (side                     . ,side)
                         (slot                     . ,slot))
                       (cond
                        ((equal fixed 'height) `((window-height . ,size)))
                        ((equal fixed 'width) `((window-width . ,size)))))))
        (set-window-buffer window buffer))
       (t (user-error "Unable to create side window")))

      (when (plist-get plist :disable-modeline)
        (set-window-parameter window 'mode-line-format 'none))
      (set-window-parameter window 'no-delete-other-windows t)

      (with-current-buffer buffer
        (setq-local window-size-fixed fixed))

      (if (plist-get plist :select) window init-window))))

(defun +display-buffer-in-purposed-window (buffer &optional alist plist)
  "Display BUFFER in window with specified purpose according to ALIST and PLIST."
  (if (plist-get plist :ignore) 'fail
    (let* ((purpose-list (plist-get plist :purpose))
           (init-window (selected-window))
           window)
      (cond
       ;; if reuse flag is set and if buffer is visible, reuse the window
       ((and (setq window (get-buffer-window buffer)) (plist-get plist :reuse))
        (select-window window))
       ((setq window (get-buffer-window buffer))
        (select-window window))
       ;; return first window with purpose matching an element from `purpose-list`
       ;; if `purpose-list` contains multiple matched elements, return window of the
       ;; matched element from the list.
       ((setq window (cl-some (lambda (purpose) (+window-with-purpose purpose)) purpose-list))
        (set-window-buffer window buffer))
       (t (user-error "Unable to find window with the requested purpose")))

      (if (plist-get plist :select) window init-window))))

(defun +display-buffer-in-mru-main-window (buffer &optional alist plist)
  "Display BUFFER in most recently used window according to ALIST and PLIST."
  (if (plist-get plist :ignore) 'fail
    (let* ((init-window (selected-window))
           window)
      (cond
       ;; if reuse flag is set and if buffer is visible, reuse the window
       ((and (setq window (get-buffer-window buffer)) (plist-get plist :reuse))
        (select-window window))
       ((setq window (util/window-get-mru-in-main))
        (set-window-buffer window buffer))
       (t (user-error "Unable to get main window")))

      (if (plist-get plist :select) window init-window))))

(defun +display-buffer-in-lru-main-window (buffer &optional alist plist)
  "Display BUFFER in least recently used window according to ALIST and PLIST."
  (if (plist-get plist :ignore) 'fail
    (let* ((init-window (selected-window))
           window)
      (cond
       ;; if reuse flag is set and if buffer is visible, reuse the window
       ((and (setq window (get-buffer-window buffer)) (plist-get plist :reuse))
        (select-window window))
       ((setq window (util/window-get-lru-in-main))
        (set-window-buffer window buffer))
       (t (user-error "Unable to get main window")))

      (if (plist-get plist :select) window init-window))))

(defun +window--action-match (buffer-or-name condition plist)
  "Action match function.
When BUFFER-OR-NAME matches CONDITION, PLIST is returned."
  (let* ((buffer (get-buffer buffer-or-name))
         (buffer-major-mode (buffer-local-value 'major-mode buffer))
         (buffer-name (buffer-name buffer))
         (condition-if (plist-get plist :if)))
    (when (or (not condition-if)
              (and condition-if (funcall condition-if)))
      (when (or (and (symbolp condition)
                     (provided-mode-derived-p buffer-major-mode condition))
                (and (stringp condition)
                     (or (string= condition buffer-name)
                         (and (plist-get plist :regexp)
                              (string-match condition buffer-name))))
                (and (consp condition)
                     (or (and (eq (car condition) :custom)
                              (funcall (cadr condition) buffer))
                         (cl-some (lambda (c)(+window--action-match buffer-or-name
                                                                    c
                                                                    plist))
                                  condition))))
        plist))))

(defun +dynamic-display-buffer--match-action (buffer-or-name action-list)
  "Return action from ACTION-LIST for BUFFER-OR-NAME."
  (cl-loop for (condition . plist) in action-list
           when (+window--action-match buffer-or-name condition plist)
           return plist
           finally return nil))

(defun +dynamic-display-buffer (buffer &optional alist plist)
  "DISPLAY BUFFER according to ALIST, PLIST, and the inititial window.

If the inititial window is a side window, display BUFFER using the rules
defined in `:dynamic`.  `:dynamic` is a list of
rules (CONDITION . ACTION-PLIST), and each condition can be a symbol or string.
A symbol is interpreted as a major-mode; a string, the buffer name or
a regular expression if `:regexp` is present in the action plist.

Additional ACTION-PLIST options:

:action and a function name or lambda:

Function with arguments BUFFER-OR-NAME, ALIST, and PLIST.

:mru and t:

Open BUFFER in the most recently used window

:lru and t:

Open BUFFER in the least recently used window

If the inititial window is not a side window, display BUFFER using `:static`"
  (if (plist-get plist :ignore) 'fail
    (let* ((init-window (selected-window))
           window
           rule-plist)
      (cond
       ((or (util/window-popup-p init-window)
            (util/window-side-p init-window))
        (unless (setq rule-plist (+dynamic-display-buffer--match-action
                                  (window-buffer init-window)
                                  (plist-get plist :dynamic)))
          (setq rule-plist (plist-get plist :static))))
       (t (setq rule-plist (plist-get plist :static))))

      (cond
       ((plist-get rule-plist :same)
        (setq window (display-buffer--maybe-same-window buffer alist)))
       ((plist-get rule-plist :mru)
        (setq window (+display-buffer-in-mru-main-window buffer alist rule-plist)))
       ((plist-get rule-plist :lru)
        (setq window (+display-buffer-in-lru-main-window buffer alist rule-plist)))
       ((setq action (plist-get rule-plist :action))
        (setq window (funcall (plist-get rule-plist :action) buffer alist rule-plist)))
       (t 'fail))
      window)))

(defun +window-display-buffer-in-directed-side-window (buffer alist)
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

(provide 'lib-window-extras)

;;; lib-window-extras.el ends here
