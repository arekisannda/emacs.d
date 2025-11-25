;;; packages-windows.el --- Window Management Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'windmove)
(require 'util-strings)

(setq +wm-right-width 110)
(setq +wm-left-width 40)
(setq +wm-bottom-height 20)

(setq-default windmove-allow-all-windows t)
(setq-default switch-to-buffer-obey-display-actions t)
(setq-default window-combination-limit 'window-size)
(setq-default window-sides-slots '(3 0 3 2))
(setq-default window-sides-vertical t)
(setq-default even-window-sizes nil)
(setq-default window-persistent-parameters
              '((window-slot             . writable)
                (window-side             . writable)
                (window-purpose          . writable)
                (window-popup            . writable)
                (clone-of                . t)
                (no-other-window         . t)
                (no-delete-other-windows . t)
                (window-preserved-size   . t)))

(defmacro +window-split (splitfn)
  "Split window with SPLITFN."
  `(lambda ()
     (interactive)
     (funcall #',splitfn)
     (balance-windows (window-parent))))

(defmacro +window-split-focus-other-window (splitfn)
  "Focus other window after calling SPLITFN."
  `(lambda ()
     (interactive)
     (funcall #',splitfn)
     (other-window 1)))

(defmacro +window-make-frame-with-params (params &rest body)
  "Create new frames with PARAMS and run BODY."
  `(let ((frame (make-frame ,params)))
     (select-frame-set-input-focus frame)
     ,@body))

(defmacro +window-select-frame-with-params (params &rest body)
  "Select frames with PARAMS or create it then run BODY."
  `(let ((frame (cl-find-if
                 (lambda (f)
                   (seq-every-p
                    (lambda (p)
                      (or
                       (equal (frame-parameter f (car p)) (cdr p))
                       (eq (frame-parameter f (car p)) (cdr p))))
                    ,params))
                 (frame-list))))
     (if frame
         (progn
           (select-frame-set-input-focus frame)
           ,@body)
       (+window-make-frame-with-params ,params ,@body))))

(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-dispatch-when-more-than 0)
  (aw-swap-invert nil)
  (aw-dispatch-always nil)
  :custom-face
  (aw-leading-char-face
   ((nil :weight bold
         :height 2.00
         :box (:line-width 5 :style flat-button)
         :foreground ,(doom-color 'red)
         :background unspecified)))
  :hook
  (after-init . ace-window-posframe-mode))

(use-package windex :after ace-window
  :custom
  (windex-window-filter-functions
   '((lambda (window)
       (or (window-parameter window 'window-side)
           (window-parameter window 'window-popup)))))
  (windex-window-aw-filter-functions windex-window-filter-functions)
  :config
  (defmacro function-with-selector-window (selector-fn fn)
    (let ((fn-name (util/function-name fn)))
      `(defun ,(intern (concat fn-name "-with-selector-window")) (&rest args)
         ,(format "Call `%s' with ARGS on window returned by selector." fn-name)
         (interactive)
         (windex-with-selector-window ,selector-fn
           (apply (intern ,fn-name) args)))))

  (windex--enable-ace-window)
  (windex--enable-windmove-in-direction-split))

(defmacro +aw-select-with-filter (filter-list)
  `(lambda (&optional arg)
     (interactive "p")
     (let ((windex-window-aw-filter-functions ,filter-list))
       (pcase arg
         (4 (mapcar #'delete-window (aw-window-list)))
         (_ (ace-select-window))
         ))
     ))

(setq +aw-select-left-filter
      '((lambda (window) (not (equal (window-parameter window 'window-side) 'left)))))

(setq +aw-select-right-filter
      '((lambda (window) (not (equal (window-parameter window 'window-side) 'right)))))

(setq +aw-select-bottom-filter
      '((lambda (window) (not (equal (window-parameter window 'window-side) 'bottom)))))

(use-package windex-purpose
  :custom
  (windex-purpose-alist
   '((edit-main      :activate nil :deactivate nil)
     (edit-general   :activate nil :deactivate nil)
     (view-info      :activate nil :deactivate nil)
     (view-reference :activate nil :deactivate nil)
     (view-log       :activate nil :deactivate nil))))

(use-package windex-layout
  :init
  (defun +windex-layout-list-main-window-buffers ()
    (mapcar
     #'window-buffer
     (seq-filter
      (lambda (win)
        (not (or (window-parameter win 'window-side)
                 (window-parameter win 'window-popup))))
      (window-list nil nil (selected-window)))))

  (defun +windex-layout-list-restore-buffers ()
    (mapcar
     #'window-buffer
     (seq-filter
      (lambda (win)
        (or (window-parameter win 'window-side)
            (window-parameter win 'window-popup)))
      (window-list nil nil (windex-first-live-window (window-main-window))))))

  :custom
  (windex-layout-buffer-list-apply-function #'+windex-layout-list-main-window-buffers)
  (windex-layout-buffer-list-restore-function #'+windex-layout-list-restore-buffers)
  (windex-layout-alist
   '((base :description "1x1 layout."
           :tree (:type buf))
     (col-2 :description "1x2 layout."
            :tree ( :type col
                    :nodes ((:type buf) (:type buf))))
     (col-3 :description "1x3 layout."
            :tree ( :type col
                    :nodes ((:type buf) (:type buf) (:type buf))))
     (row-2 :description "2x1 layout."
            :tree ( :type row
                    :nodes ((:type buf) (:type buf))))
     (col-2-left :description "2x1 layout with 1x2 left column."
                 :tree ( :type col
                         :nodes
                         ((:type row :nodes ((:type buf) (:type buf)))
                          (:type buf))))
     (col-2-right :description "2x1 layout with 1x2 right column."
                  :tree ( :type col
                          :nodes
                          ((:type buf)
                           (:type row :nodes ((:type buf)(:type buf))))))
     (tile :description "2x2 layout."
           :tree ( :type col
                   :nodes
                   (( :type row :nodes ((:type buf) (:type buf)))
                    ( :type row :nodes ((:type buf) (:type buf))))))
     )))

(use-package windex-scroll :after (evil)
  :custom
  (windex-scroll-other-window-selector
   (lambda ()
     (or (windex-window-with-parameters '((window-side . right)) nil t)
         (windex-window-with-parameters '((window-popup . below)) nil t))))

  (windex-scroll-left-function #'evil-scroll-column-left)
  (windex-scroll-right-function #'evil-scroll-column-right)
  (windex-scroll-up-function #'evil-scroll-line-up )
  (windex-scroll-down-function #'evil-scroll-line-down))

(use-package shackle :after windex
  :custom
  (shackle-default-rule nil)
  (shackle-rules
   `((("^\\*Capture\\*$"
       "^\\*Warnings\\*$"
       "^\\*Flymake log\\*$"
       "^\\*Activities (error): .*\\*$")
      :ignore t)

     (("^ \\*transient\\*$"
       "^ \\*CDLaTeX Help\\*")
      :custom +dynamic-display-buffer
      :static ( :action +display-buffer-in-pop-up-window
                :select t)
      :dynamic
      (((".*")
        :if (lambda (window)
              (equal (window-parameter window 'window-side) 'bottom))
        :action +display-buffer-in-side-window
        :side bottom
        :slot 0
        :size ,+wm-bottom-height
        :fixed height
        :select t)

       ((".*")
        :if (lambda (window)
              (equal (window-parameter window 'window-side) 'right))
        :action +display-buffer-in-side-window
        :side right
        :slot 1
        :size ,+wm-right-width
        :fixed width
        :select t
        )

       ((".*")
        :if (lambda (window) (window-parameter window 'window-popup))
        :same t :select t)
       ))

     (("^\\*diff-hl\\*"
       "^\\*diff-hl-revert\\*"
       "^\\*diff-hl-show-hunk-diff-buffer\\*"
       "^\\*diff-hl-show-hunk-buffer\\*"
       "^\\*Deletions\\*$"
       "^ widget-choose$"
       "^\\*Ibuffer confirmation\\*"
       "^\\*Local Variables\\*$"

       backtrace-mode)
      :custom +display-buffer-in-pop-up-window
      :select t)

     ((dashboard-mode
       pdf-view-mode

       "^\\*Org Src.*\\*$"
       "^\\*Org Preview.*\\*$"
       "^\\*Org Select\\*$")
      :same t :select t)

     ((treemacs-mode)
      :custom +display-buffer-in-side-window
      :side left
      :slot 0
      :size ,+wm-left-width
      :fixed width
      :select t)

     (("^COMMIT_EDITMSG$"
       "^CAPTURE-.*\\.org$"
       "^\\*Org .*\\*$"
       "^\\*Dictionary\\*$"
       "^\\*Customize Apropos\\*$"
       "^\\*Shortdoc.*\\*$"
       "^\\*Customize.*\\*$"
       "^\\*Man.*\\*$"
       "^\\*WoMan.*\\*$"
       "^\\*IBuffer\\*$"

       magit-status-mode
       magit-repolist-mode
       magit-diff-mode
       magit-log-mode
       forge-repository-list-mode
       devdocs-mode
       dictionary-mode)
      :custom +display-buffer-in-side-window
      :side right
      :slot 0
      :size ,+wm-right-width
      :fixed width
      :select t)

     (("^\\*org-roam\\*$"
       "^\\*info\\*$"
       "^\\*eldoc.*\\*"
       "^ \\*eglot doc\\*$"
       "^\\*yasnippet-capf-doc\\*$"
       "^\\*corfu doc.*\\*$"
       "^ \\*Agenda Commands\\*$"

       org-roam-mode
       man-common)
      :custom +display-buffer-in-side-window
      :side right
      :slot 0
      :size ,+wm-right-width
      :fixed width)

     (("^\\*Error\\*$"
       "^\\*Dired log\\*$"
       "^\\*Shell Command Output\\*$"
       "^\\*latex-scratch\\*$"
       "^\\*org-scratch\\*$"
       "^\\*remark-notes\\*$"
       "^\\*Org Links\\*$"
       "^\\*shell\\*$"
       "^\\*Command Line\\*$"
       "^\\*\\(.*-\\)?eshell\\*$"
       "^\\*Calculator\\*$"
       "^ \\*EGLOT [^\s]* stderr\\*$"
       "^\\*[^\s]*[\s]+events\\*$"
       "^\\*ChatGPT.*\\*$"

       dired-mode
       calc-mode
       compilation-mode
       eshell-mode
       comint-mode
       ert-results-mode
       grep-mode
       log4e-mode
       messages-buffer-mode
       occur-mode
       xref--xref-buffer-mode
       flymake-diagnostics-buffer-mode
       flymake-project-diagnostics-mode
       lisp-interaction-mode
       term-mode
       vterm-mode
       embark-collect-mode
       calendar-mode
       tabulated-list-mode)
      :custom +display-buffer-in-side-window
      :side bottom
      :slot 0
      :size ,+wm-bottom-height
      :fixed height
      :select t)

     (("^\\*Calc Trail\\*$"
       calc-trail-mode)
      :custom +display-buffer-in-side-window
      :side bottom
      :slot 1
      :size ,+wm-bottom-height
      :fixed height)

     ;;; base mode fallback
     ((Custom-mode
       special-mode
       help-mode
       Info-mode)
      :custom +display-buffer-in-side-window
      :side right
      :slot 0
      :size ,+wm-right-width
      :fixed width
      :select t)

     ((prog-mode
       text-mode
       conf-mode
       outline-mode
       fundamental-mode)
      :custom +dynamic-display-buffer
      :static (:same t :select t)
      :dynamic
      (((org-agenda-mode)
        :if (lambda (&rest _) org-agenda-follow-mode)
        :action +display-buffer-in-side-window
        :side right
        :slot 1
        :size ,+wm-right-width
        :fixed width
        :select t)

       ((".*")
        :if (lambda (window)
              (or (window-parameter window 'window-side)
                  (window-parameter window 'window-popup)))
        :mru t :select t :reuse t)

       ((".*")
        :if (lambda (window)
              (and (not (and (window-parameter window 'window-side)
                             (window-parameter window 'window-popup)))
                   (window-dedicated-p window)))
        :mru t :select t :reuse t)

       (org-roam-mode :mru t :select t)

       ((flymake-project-diagnostics-mode
         flymake-diagnostics-buffer-mode
         lisp-interaction-mode)
        :mru t :select t :reuse t)

       ((embark-collect-mode)
        :mru t :reuse t)

       ((help-mode
         Custom-mode
         dired-mode)
        :mru t :select t :reuse t)

       ((prog-mode
         text-mode
         conf-mode
         outline-mode)
        :same t :select t :reuse t)
       ))
     ))
  :config
  (defun +display-buffer-in-side-window (buffer &optional alist plist)
    "Display BUFFER in side window according to ALIST and PLIST."
    (if (plist-get plist :ignore) 'fail
      (let* ((side (plist-get plist :side))
             (slot (plist-get plist :slot))
             (size (plist-get plist :size))
             (fixed (plist-get plist :fixed))
             (init-window (window-normalize-window nil))
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
                 `(,@alist
                   (direction             . ,(plist-get plist :direction))
                   (side                  . ,side)
                   (slot                  . ,slot)
                   (inhibit-same-window   . t)
                   (window-height         . ,(and (find side '(bottom top)) size))
                   (window-width          . ,(and (find side '(right left)) size))
                   )
                 )))
         (t (user-error "Unable to create side window")))

        (when (plist-get plist :disable-modeline)
          (set-window-parameter window 'mode-line-format 'none))
        (unless (window-parameter window 'quit-restore)
          (set-window-parameter window 'quit-restore `(window window ,init-window ,buffer)))
        (when (plist-get plist :no-other)
          (set-window-parameter window 'no-other-window t))
        (set-window-buffer window buffer)
        (set-window-dedicated-p window (plist-get plist :dedicated))
        (set-window-parameter window 'no-other-window t)
        (window-preserve-size window (not (eq fixed 'height)) t)

        (with-current-buffer buffer
          (setq-local window-size-fixed fixed))

        (if (plist-get plist :select) window init-window))))

  (defun +display-buffer-in-purposed-window (buffer &optional alist plist)
    "Display BUFFER in window with specified purpose according to ALIST and PLIST."
    (if (plist-get plist :ignore) 'fail
      (let* ((purpose-list (plist-get plist :purpose))
             (init-window (window-normalize-window nil))
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
         ((setq window (cl-some (lambda (purpose) (windex-window-with-purpose purpose)) purpose-list))
          (set-window-buffer window buffer))
         (t (user-error "Unable to find window with the requested purpose")))

        (if (plist-get plist :select) window init-window))))

  (defun +display-buffer-in-mru-main-window (buffer &optional alist plist)
    "Display BUFFER in most recently used window according to ALIST and PLIST."
    (if (plist-get plist :ignore) 'fail
      (let* ((init-window (window-normalize-window nil))
             window)
        (cond
         ;; if reuse flag is set and if buffer is visible, reuse the window
         ((and (setq window (get-buffer-window buffer)) (plist-get plist :reuse))
          (select-window window))
         ((setq window (windex-get-mru-in-main))
          (set-window-buffer window buffer))
         (t (user-error "Unable to get main window")))

        (if (plist-get plist :select) window init-window))))

  (defun +display-buffer-in-lru-main-window (buffer &optional alist plist)
    "Display BUFFER in least recently used window according to ALIST and PLIST."
    (if (plist-get plist :ignore) 'fail
      (let* ((init-window (window-normalize-window nil))
             window)
        (cond
         ;; if reuse flag is set and if buffer is visible, reuse the window
         ((and (setq window (get-buffer-window buffer)) (plist-get plist :reuse))
          (select-window window))
         ((setq window (util/window-get-lru-in-main))
          (set-window-buffer window buffer))
         (t (user-error "Unable to get main window")))

        (if (plist-get plist :select) window init-window))))

  (defun +dynamic-display-buffer--match-action (buffer-or-name action-list)
    "Return action from ACTION-LIST for BUFFER-OR-NAME."
    (cl-loop for (condition . plist) in action-list
             when (shackle--match buffer-or-name condition plist)
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
      (let* ((init-window (window-normalize-window nil))
             window
             rule-plist)
        (unless (setq rule-plist (+dynamic-display-buffer--match-action
                                  (window-buffer init-window)
                                  (plist-get plist :dynamic)))
          (setq rule-plist (plist-get plist :static)))

        (cond
         ((plist-get rule-plist :same)
          (setq window (display-buffer-same-window buffer alist)))
         ((and (plist-get rule-plist :reuse) (setq window (get-buffer-window buffer)))
          (setq window (display-buffer-reuse-window buffer alist)))
         ((plist-get rule-plist :mru)
          (setq window (+display-buffer-in-mru-main-window buffer alist rule-plist)))
         ((plist-get rule-plist :lru)
          (setq window (+display-buffer-in-lru-main-window buffer alist rule-plist)))
         ((setq action (plist-get rule-plist :action))
          (setq window (funcall (plist-get rule-plist :action) buffer alist rule-plist)))
         (t 'fail))
        window)))

  (defun +display-buffer-in-pop-up-window (buffer &optional alist plist)
    (let ((frame (shackle--splittable-frame)))
      (when frame
        (if (plist-get plist :ignore) 'fail
          (let* ((init-window (window-normalize-window nil))
                 (alist `(,@alist
                          (window-popup          . bottom)
                          (no-other-window       . t)
                          (dedicated             . t)
                          (window-preserved-size . t)
                          ))
                 parameters
                 window)
            (with-current-buffer buffer
              (if (get-buffer-window buffer)
                  (display-buffer-reuse-window buffer alist)
                (let* ((lines (count-lines (point-min) (point-max)))
                       (window (split-window (frame-root-window frame) (min -20 (max -20 (- lines))))))
                  (window--display-buffer buffer window 'window alist)
                  (set-window-parameter window 'no-other-window t)
                  (window-preserve-size window nil t)
                  (if (plist-get plist :select) window init-window)))
              ))
          ))
      ))

  (defun +shackle-condition-ignore-check (orig-func &rest args)
    (let* ((buffer (get-buffer-create (nth 0 args)))
           (buffer-name (buffer-name buffer))
           (buffer-mode (buffer-local-value 'major-mode buffer)))
      (unless (cl-some
               (lambda (e)
                 (cond
                  ((listp e) (string-match (car e) buffer-name))
                  ((stringp e) (string= buffer-name e))
                  ((symbolp e) (eq buffer-mode e))))
               `(,which-key-buffer-name
                 ,embrace--help-buffer-name))
        (apply orig-func args))))

  (advice-add #'shackle-display-buffer-condition :around #'+shackle-condition-ignore-check)

  (defun +shackle--match (buffer-or-name condition plist)
    "Action match function.
When BUFFER-OR-NAME matches CONDITION, PLIST is returned."
    (let* ((buffer (get-buffer buffer-or-name))
           (buffer-major-mode (buffer-local-value 'major-mode buffer))
           (buffer-name (buffer-name buffer))
           (condition-if (plist-get plist :if)))
      (when (or (not condition-if)
                (and condition-if (funcall condition-if (selected-window))))
        (when (or (and (symbolp condition)
                       (provided-mode-derived-p buffer-major-mode condition))
                  (and (stringp condition)
                       (or (string-match condition buffer-name)
                           (string= condition buffer-name)))
                  (and (consp condition)
                       (or (and (eq (car condition) :custom)
                                (funcall (cadr condition) buffer))
                           (cl-some (lambda (c)(shackle--match buffer-or-name c plist))
                                    condition))))
          plist))))

  (advice-add #'shackle--match :override #'+shackle--match)

  ;; add `shackle-mode` guard to prevent adding duplicates in
  ;; `display-buffer-alist`
  (unless shackle-mode (shackle-mode t)))

(use-package winner
  :custom
  (winner-dont-bind-my-keys t)
  :hook
  (window-setup . winner-mode))

(provide 'packages-windows)

;;; packages-windows.el ends here
