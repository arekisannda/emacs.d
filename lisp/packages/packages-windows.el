;;; packages-windows.el --- Window Management Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package emacs
  :ensure nil
  :custom
  (window-sides-slots '(3 0 3 1))
  (window-sides-vertical t))

(use-package window-purpose :disabled
  :custom
  (purpose-message-on-p nil)
  (purpose-use-default-configuration t)
  (purpose-user-mode-purposes
   '((prog-mode                   . edit)
     (vterm-mode                  . terminal)
     (lisp-interaction-mode       . terminal)
     (comint-mode                 . terminal)
     (eshell-mode                 . terminal)
     (term-mode                   . terminal)
     (magit-mode                  . rtools)
     (magit-repolist-mode         . rtools)
     (elpaca-manager-mode         . rtools)
     (elpaca-ui-mode              . rtools)
     (elpaca-log-mode             . rtools)
     (elpaca-info-mode            . rtools)
     (Custom-mode                 . rtools)
     (help-mode                   . rtools)
     (apropos-mode                . rtools)
     (markdown-view-mode          . rtools)
     (messages-buffer-mode        . terminal)
     (backtrace-mode              . terminal)
     (compilation-mode            . terminal)
     (tabulated-list-mode         . rtools)
     (diff-mode                   . rtools)
     (xref--xref-buffer-mode      . rtools)
     (ibuffer-mode                . rtools)
     (Buffer-menu-mode            . rtools)
     (bookmark-bmenu-mode         . rtools)
     (grep-mode                   . rtools)
     (occur-mode                  . rtools)
     ))
  (purpose-user-name-purposes
   '((".gitignore"                . edit)
     (".hgignore"                 . edit)
     ("COMMIT_EDITMSG"            . terminal)
     ("*shell*"                   . terminal)
     ("*eldoc*"                   . terminal)
     ("*elpaca-manager*"          . rtools)
     ("*remark-notes*"            . rtools)
     ))
  (purpose-user-regexp-purposes
   '(("^ \\*Minibuf-[0-9]*\\*$" . minibuf)
     ("^ \\*log4e-.*\\*$"       . terminal)
     ("\\*Org Agenda.*\\*"      . ltools)
     ("[1-9][0-9]*_\\(0?[1-9]\\|1[0-2]\\)_\\(0?[1-9]\\|[12][0-9]\\|3[01]\\)\\.org$" . edit)))

  (purpose-x-popwin-width 0.33)
  (purpose-x-popwin-height 0.4)
  (purpose-display-at-right-width 0.33)
  (purpose-display-at-left-width 0.25)
  (purpose-display-at-bottom-height 0.4)
  :config
  (purpose-compile-user-configuration)
  (purpose-x-magit-off)
  (purpose-x-popupify-purpose 'terminal #'purpose-display-at-bottom)
  (purpose-x-popupify-purpose 'rtools #'purpose-display-at-right)
  (purpose-x-popupify-purpose 'Magit #'purpose-display-at-right)
  (purpose-x-popupify-purpose 'ltools #'purpose-display-at-left)
  (purpose-x-popwin-update-conf)
  (purpose-x-popwin-setup)
  (purpose-mode t))

(use-package emacs :after telephone-line :disabled
  :ensure nil
  :config
  (telephone-line-defsegment* +telepohone-line-purpose-tag-segment ()
    (let* ((purpose-plist (purpose-window-params))
           (dedicated (plist-get purpose-plist :purpose-dedicated))
           (purpose (plist-get purpose-plist :purpose)))
      (format "%s %s" (if dedicated "" "") purpose)))

  (telephone-line-defsegment* +telepohone-line-purpose-buffer-tag-segment ()
    (if (window-dedicated-p nil) "󰓎"))

  (setq telephone-line-lhs
        '((evil   . (+telepohone-line-purpose-buffer-tag-segment
                     telephone-line-evil-tag-segment))
          (accent . (+telepohone-line-purpose-tag-segment
                     telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-projectile-segment
                     telephone-line-buffer-segment)))))

(setq +wm-terminal-rule-list
      '(vterm-mode
        messages-buffer-mode
        lisp-interaction-mode
        comint-mode
        eshell-mode
        term-mode
        "*shell*"))

(setq +wm-terminal-no-focus-rule-list
      '(backtrace-mode
        compilation-mode
        "*eldoc*"))

(setq +wm-right-rule-list
      '(magit-status-mode
        magit-repolist-mode
        elpaca-manager-mode
        elpaca-ui-mode
        elpaca-log-mode
        elpaca-info-mode
        Custom-mode
        help-mode
        apropos-mode
        markdown-view-mode
        tabulated-list-mode
        diff-mode
        xref--xref-buffer-mode
        ibuffer-mode
        Buffer-menu-mode
        bookmark-bmenu-mode
        grep-mode
        occur-mode
        "*elpaca-manager*"))

(setq +wm-right-rule-list-regex
      '("^\\*Customize.*\\*$"
        "^\\*Google Translate.*\\*$"))

(setq +wm-terminal-no-focus-rule-list-regex
      '("^ \\*log4e-.*\\*$"))

(setq +wm-left-rule-list '())
(setq +wm-left-rule-list-regex '())

(setq +wm-other-rule-list
      '(magit-diff-mode
        "*remark-notes*"
        "COMMIT_EDITMSG"))

(defun +wm-display-buffer-in-side-window (buffer alist)
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

(defun +wm-select-popup (buffer &optional alist plist)
  (let* ((noselect (or (plist-get plist :noselect) nil))
         (caller-window (selected-window))
         (window (+wm-display-popup buffer alist plist)))
    (if (not noselect)
        (select-window window)
      (select-window window)
      (select-window caller-window))))

(defun +wm-display-popup (buffer &optional alist plist)
  (let* ((side (plist-get plist :align))
         (height (plist-get plist :height))
         (width (plist-get plist :width))
         (slot (plist-get plist :slot))
         (direction (or (plist-get plist :direction)
                        'vertical))
         (dedicated (plist-get plist :dedicated))
         (slot-name (cond ((equal slot -1) "a")
                          ((equal slot 0)  "b")
                          ((equal slot 1)  "c")))
         (group-suffix (format "%s-%s" side slot-name))
         (dimension-type (cond ((equal side 'above) 'window-height)
                               ((equal side 'bottom) 'window-height)
                               ((equal side 'left)  'window-width)
                               ((equal side 'right) 'window-width))))
    (with-current-buffer (current-buffer)
      (setq-local +wm-popper-last-group group-suffix))
    (with-current-buffer buffer
      (setq-local +wm-slot group-suffix))
    (display-buffer-in-side-window
     buffer
     (append alist
             `((dedicated         . ,dedicated)
               (side              . ,side)
               (slot              . ,slot)
               (direction         . ,direction))
             (if height
                 `((window-height . ,height)))
             (if width
                 `((window-width  . ,width))))
     )))

(setq-default +wm-slot nil)
(setq-default +wm-popper-last-group nil)

(defun +popper-group-function ()
  (let* ((perspective (cond ((featurep 'persp-mode) (safe-persp-name (get-current-persp)))
                            ((featurep 'perspective) (persp-name (persp-curr))))))
    (if +wm-slot
        (format "%s-%s" perspective +wm-slot)
      (if +wm-popper-last-group
          (format "%s-%s" perspective +wm-popper-last-group)
        (format "%s" perspective)))))

(use-package popper
  :preface
  (defvar +popper-derived-mode-list '())
  (defvar +popper-string-list '())

  (defun +popper-derived-mode-p (buf)
    "Return mode if BUF is derived from mode in `+popper-derived-mode-list`."
    (with-current-buffer buf
      (if (cl-some #'derived-mode-p +popper-derived-mode-list) t nil)))

  (setq +popper-derived-mode-list '())
  (setq +popper-string-list '())
  (dolist (item (append +wm-terminal-rule-list
                        +wm-terminal-no-focus-rule-list
                        +wm-right-rule-list
                        +wm-left-rule-list
                        +wm-terminal-no-focus-rule-list-regex
                        +wm-left-rule-list-regex
                        +wm-right-rule-list-regex
                        +wm-other-rule-list))
    (cond ((symbolp item) (push item +popper-derived-mode-list))
          ((stringp item) (push item +popper-string-list))))

  :custom
  (popper-reference-buffers
   (append '("\\*eldoc.*\\*$"
             "\\*lsp-help\\*$"
             "\\*compilation\\*$"
             "\\*Man.*\\*$"
             "\\*Flycheck.*\\*$"
             "\\*Ediff Control.*\\*$"
             "\\*evil-marks\\*$"
             "\\*Async Shell Command\\*$"
             "\\*EGLOT.*\\*$"
             "\\*diff-hl\\*$"
             +popper-derived-mode-p)
           +popper-string-list))
  (popper-window-height 0.40)
  (popper-mode-line "")
  (popper-display-control nil)
  ;; (popper-display-function #'display-buffer-in-child-frame)
  (popper-group-function #'+popper-group-function)
  :hook
  (elpaca-after-init . popper-mode))

(use-package shackle :after popper
  :custom
  (shackle-default-rule nil)
  (shackle-rules
   `((,+wm-terminal-rule-list
      :popup t :align bottom :height 0.40 :width nil :slot 0
      :custom +wm-select-popup)
     (,+wm-terminal-no-focus-rule-list
      :popup t :align bottom :height 0.40 :width nil :slot 0 :noselect t
      :custom +wm-select-popup)
     (,+wm-left-rule-list
      :popup t :align left :height nil :width 35 :slot 1
      :custom +wm-select-popup)
     (,+wm-right-rule-list
      :popup t :align right :height nil :width 100 :slot 0
      :custom +wm-select-popup)
     (,+wm-terminal-no-focus-rule-list-regex
      :popup t :align bottom :height 0.40 :width nil :regexp t :slot 0 :noselect t
      :custom +wm-select-popup)
     (,+wm-left-rule-list-regex
      :popup t :align left :height nil :width 35 :regexp t :slot 1
      :custom +wm-select-popup)
     (,+wm-right-rule-list-regex
      :popup t :align right :height nil :width 100 :regexp t :slot 0
      :custom +wm-select-popup)
     ("[1-9][0-9]*_\\(0?[1-9]\\|1[0-2]\\)_\\(0?[1-9]\\|[12][0-9]\\|3[01]\\)\\.org$"
      :other t :regexp t)
     ("*remark-notes*"
      :popup t :align right :height 0.40 :width 100 :slot 1 :noselect t
      :custom +wm-select-popup)
     ("^\\*Org Agenda.*\\*$"
      :popup t :align left :height nil :width 35 :regexp t :slot 1 :dedicated t
      :custom +wm-select-popup)
     ((prog-mode) :same t)
     ;; ("COMMIT_EDITMSG"
     ;;  :popup t :align right :height nil :width 80 :slot 0
     ;;  :custom +wm-select-popup)
     (magit-diff-mode
      :popup t :align right :height 0.60 :width 80 :slot -1 :noselect t
      :custom +wm-select-popup)
     ))
  :config
  (defun +shackle--match (buffer-or-name condition plist)
    (let* ((buffer (get-buffer buffer-or-name))
           (buffer-major-mode (buffer-local-value 'major-mode buffer))
           (buffer-name (buffer-name buffer)))
      (when (or (and (symbolp condition)
                     (provided-mode-derived-p buffer-major-mode condition))
                (and (stringp condition)
                     (or (string= condition buffer-name)
                         (and (plist-get plist :regexp)
                              (string-match condition buffer-name))))
                (and (consp condition)
                     (or (and (eq (car condition) :custom)
                              (funcall (cadr condition) buffer))
                         (cl-some (lambda (c) (+shackle--match buffer-or-name
                                                               c plist))
                                  condition))))
        plist)))

  (advice-add #'shackle--match :override #'+shackle--match)
  (shackle-mode t))

(use-package emacs :after telephone-line
  :ensure nil
  :config
  (telephone-line-defsegment* +telepohone-line-buffer-dedicated-tag-segment ()
    (if (window-dedicated-p nil) "" ""))

  (telephone-line-defsegment* +telepohone-line-popper-tag-segment ()
    (if popper-popup-status "󰁊" nil))

  (setq telephone-line-lhs
        '((evil   . (+telepohone-line-buffer-dedicated-tag-segment
                     telephone-line-evil-tag-segment))
          (accent . (+telepohone-line-popper-tag-segment
                     telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-projectile-segment
                     telephone-line-buffer-segment))))
  )

(provide 'packages-windows)

;;; packages-windows.el ends here
