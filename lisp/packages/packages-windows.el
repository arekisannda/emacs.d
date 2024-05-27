;;; packages-windows.el --- Window Management Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'lib-window-extras)

(setq +wm-bottom-rule-list
      '(vterm-mode
        comint-mode
        eshell-mode
        term-mode
        diff-mode
        messages-buffer-mode
        lisp-interaction-mode
        xref--xref-buffer-mode
        grep-mode
        occur-mode
        backtrace-mode
        compilation-mode
        "*shell*"
        "*Shell Command Output*"
        "*diff-hl-show-hunk-diff-buffer*"
        "*diff-hl-show-hunk-buffer*"
        "*remark-notes*"))

(setq +wm-bottom-rule-list-regex
      '("^\\*Google Translate.*\\*$"
        "^ \\*log4e-.*\\*$"))

(setq +wm-right-rule-list
      '(elpaca-manager-mode
        elpaca-ui-mode
        elpaca-log-mode
        elpaca-info-mode
        magit-status-mode
        magit-repolist-mode
        Info-mode
        Custom-mode
        apropos-mode
        tabulated-list-mode
        ibuffer-mode
        Buffer-menu-mode
        bookmark-bmenu-mode
        help-mode
        "*info*"
        "*Ibuffer*"
        "*elpaca-manager*"
        "*Customize Apropos*"))

(setq +wm-right-rule-list-regex
      '("^\\*Shortdoc.*\\*$"
        "^\\*Customize.*\\*$"))

(setq +wm-left-rule-list '())
(setq +wm-left-rule-list-regex '())

(setq +wm-other-rule-list
      '(magit-diff-mode
        "*"
        "COMMIT_EDITMSG"))

(use-package popper
  :preface
  (defvar +popper-derived-mode-list '())
  (defvar +popper-string-list '())

  (defun +popper-group-function ()
    "Popper group function."
    (let* ((perspective (cond ((featurep 'persp-mode) (safe-persp-name (get-current-persp)))
                              ((featurep 'perspective) (persp-name (persp-curr)))))
           (side (window-parameter (selected-window) 'window-side)))
      (when (or (equal side 'bottom)
                (equal side 'right))
        (format "%s-%s" perspective (symbol-name side)))))

  (defun +popper-derived-mode-p (buf)
    "Return mode if BUF is derived from mode in `+popper-derived-mode-list`."
    (with-current-buffer buf
      (if (cl-some #'derived-mode-p +popper-derived-mode-list) t nil)))

  (setq +popper-derived-mode-list '())
  (setq +popper-string-list '())
  (dolist (item (append +wm-bottom-rule-list
                        +wm-bottom-rule-list-regex
                        +wm-left-rule-list
                        +wm-left-rule-list-regex
                        +wm-right-rule-list
                        +wm-right-rule-list-regex))
    (cond ((symbolp item) (push item +popper-derived-mode-list))
          ((stringp item) (push item +popper-string-list))))

  :custom
  (popper-reference-buffers
   (append '(+popper-derived-mode-p)
           +popper-string-list))
  (popper-window-height 0.40)
  (popper-mode-line "")
  (popper-display-control nil)
  (popper-group-function #'+popper-group-function)
  :hook
  (elpaca-after-init . popper-mode)

  :config
  (defun +popper-cycle (&optional num)
    (interactive "p")

    (unless (equal (window-parameter (selected-window) 'window-side) 'bottom)
      (user-error "Not popper window"))

    (let* ((group (when (and popper-group-function
                             (not (equal num 0)))
                    (funcall popper-group-function))))
      ;; cycle through buffers
      (let ((bufs (cdr (assoc group popper-buried-popup-alist))))
        (when (> (length bufs) 0)
          (setf (alist-get group popper-buried-popup-alist nil nil 'equal)
                (if (> num 0)
                    (append (cdr bufs) (cons (car bufs) nil))
                  (append (last bufs) (butlast bufs))))))
      (popper-open-latest group)))

  (defun +popper-cycle-backwards (&optional num)
    (interactive "p")
    (+popper-cycle (- num))))

(setq +wm-terminal-width nil)
(setq +wm-right-width 100)
(setq +wm-left-width 35)
(setq +wm-bottom-height 30)

(use-package emacs
  :ensure nil
  :custom
  (window-sides-slots '(3 0 3 1))
  (window-sides-vertical t)
  (window-persistent-parameters '((window-slot . writable) ;
                                  (window-side . writable)
                                  (window-purpose . t)
                                  (clone-of . t)
                                  (no-delete-other-windows . t))))

(use-package shackle :after popper
  :custom
  (shackle-default-rule nil)
  (shackle-rules
   `((,+wm-bottom-rule-list
      :align bottom :slot 0
      :height ,+wm-bottom-height :width nil
      :fixed height
      :custom +display-buffer-in-side-window)

     (,+wm-left-rule-list
      :align left :slot 1
      :height nil :width ,+wm-left-width
      :custom +display-buffer-in-side-window)
     (,+wm-left-rule-list-regex
      :regexp t
      :align left :slot 1
      :height nil :width ,+wm-left-width
      :custom +display-buffer-in-side-window)

     (,+wm-right-rule-list
      :align right :slot 0
      :height nil :width ,+wm-right-width
      :fixed width
      :custom +display-buffer-in-side-window)
     (,+wm-right-rule-list-regex
      :regexp t
      :align right :slot 0
      :height nil :width ,+wm-right-width
      :fixed width
      :custom +display-buffer-in-side-window)

     ;; update to use buffer file name to determine if display is in side or main window
     (("^\\*Org Agenda\\*$"
       "^\\*Org Agenda(a)\\*$"
       "^\\*Org Select\\*$")
      :regexp t :other t
      :align right)

     ((calendar-mode)
      :same t :other t
      :align right)

     ((magit-diff-mode)
      :regexp t :noselect t
      :align right :slot 1
      :height nil :width ,+wm-right-width
      :fixed width
      :custom +display-buffer-in-side-window)
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
    (when (boundp 'popper-popup-status)
      (if popper-popup-status "󰁊" nil)))

  (setq telephone-line-lhs
        '((evil   . (+telepohone-line-buffer-dedicated-tag-segment
                     telephone-line-evil-tag-segment))
          (accent . (+telepohone-line-popper-tag-segment
                     telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-projectile-segment
                     telephone-line-buffer-segment))))

  (setq telephone-line-rhs
        '((nil    . (telephone-line-flycheck-segment
                     telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment)))
        )
  )

(provide 'packages-windows)

;;; packages-windows.el ends here
