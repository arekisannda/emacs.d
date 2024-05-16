;;; packages-windows.el --- Window Management Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package popper :after shackle :disabled
  :preface
  (defvar +popper-derived-mode-alist '())

  (defun +popper-derived-mode-p (buf)
    "Return mode if BUF is derived from mode in `+popper-derived-mode-alist`."
    (with-current-buffer buf
      (if (cl-some #'derived-mode-p +popper-derived-mode-alist) t nil)))

  (setq +popper-derived-mode-alist
        '(messages-buffer-mode
          xref--xref-buffer-mode
          help-mode
          compilation-mode
          backtrace-mode
          lisp-interaction-mode
          Custom-mode
          vterm-mode
          grep-mode
          diff-mode
          elpaca-ui-mode
          elpaca-info-mode
          tabulated-list-mode))
  :custom
  (popper-reference-buffers '("\\*eldoc.*\\*$"
                              "\\*lsp-help\\*$"
                              "\\*compilation\\*$"
                              "\\*Man.*\\*$"
                              "\\*Flycheck.*\\*$"
                              "\\*Ediff Control.*\\*$"
                              "\\*evil-marks\\*$"
                              "^magit.*$"
                              "\\*Async Shell Command\\*$"
                              "\\*EGLOT.*\\*$"
                              "\\*diff-hl\\*$"
                              +popper-derived-mode-p))
  (popper-window-height 30)
  (popper-mode-line "")
  (popper-display-control nil)
  (popper-group-function #'(lambda () (cond ((featurep 'persp-mode) (safe-persp-name (get-current-persp)))
                                            ((featurep 'perspective) (persp-name (persp-curr))))))
  :hook
  (elpaca-after-init . popper-mode))

(use-package window-purpose
  :custom
  (purpose-message-on-p nil)
  (purpose-use-default-configuration t)
  (purpose-user-mode-purposes
   '((prog-mode                   . code)
     (vterm-mode                  . terminal)
     (lisp-interaction-mode       . terminal)
     (comint-mode                 . terminal)
     (eshell-mode                 . terminal)
     (term-mode                   . terminal)
     (magit-mode                  . tool)
     (magit-repolist-mode         . tool)
     (elpaca-manager-mode         . tool)
     (elpaca-ui-mode              . tool)
     (elpaca-log-mode             . tool)
     (elpaca-info-mode            . tool)
     (Custom-mode                 . tool)
     (help-mode                   . help-doc)
     (apropos-mode                . help-doc)
     (markdown-view-mode          . docs)
     (messages-buffer-mode        . logs)
     (backtrace-mode              . logs)
     (compilation-mode            . logs)
     (tabulated-list-mode         . search)
     (diff-mode                   . search)
     (xref--xref-buffer-mode      . search)
     (ibuffer-mode                . search)
     (Buffer-menu-mode            . search)
     (bookmark-bmenu-mode         . search)
     (grep-mode                   . search)
     (occur-mode                  . search)
     ))
  (purpose-user-name-purposes
   '((".gitignore"                . code)
     (".hgignore"                 . code)
     ("COMMIT_EDITMSG"            . terminal)
     ("*shell*"                   . terminal)
     ("*eldoc*"                   . terminal)
     ("*elpaca-manager*"          . tool)
     ("*remark-notes*"            . remark)
     ))
  (purpose-user-regexp-purposes
   '(("^ \\*Minibuf-[0-9]*\\*$" . minibuf)
     ("^ \\*log4e-.*\\*$"       . logs)
     ("\\*Org Agenda.*\\*"      . agenda-view)
     ("[1-9][0-9]*_\\(0?[1-9]\\|1[0-2]\\)_\\(0?[1-9]\\|[12][0-9]\\|3[01]\\)\\.org$" . agenda)))
  :config
  (purpose-compile-user-configuration)
  (purpose-x-magit-single-on)
  (purpose-mode t))

(use-package shackle :after window-purpose
  :custom
  (shackle-default-rule '(:same t))
  (shackle-rules '((terminal    :popup t :align below :size 0.4)
                   (logs        :popup t :align below :size 0.4)
                   (search      :popup t :align below :size 0.4)
                   (tool        :popup t :align right :size 0.3)
                   (help-doc    :popup t :align right :size 0.3)
                   (remark      :popup t :align right :size 0.3)
                   (agenda-view :popup t :align right :size 0.3)
                   ;; (Buffer-menu-mode   :popup t :align below :size 0.3)
                   ;; (elpaca-ui-mode :popup t :align right :size 0.3)
                   ;; (elpaca-log-mode :popup t :align right :size 0.3)
                   ;; (elpaca-manager-mode :popup t :align below :size 0.4)
                   ))
  :config
  (defun +shackle--match (buffer-or-name condition plist)
    (let* ((buffer (get-buffer buffer-or-name))
           (buffer-major-mode (buffer-local-value 'major-mode buffer))
           (buffer-purpose (purpose-buffer-purpose buffer-or-name))
           (buffer-name (buffer-name buffer)))
      (when (or (and (symbolp condition)
                     (provided-mode-derived-p buffer-major-mode condition))
                ;; compare buffer purpose
                (and (symbolp condition)
                     (equal buffer-purpose condition))
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

  (defun +maybe-display-shackle (buffer alist)
    (and (shackle-display-buffer-condition buffer alist)
         (shackle-display-buffer-action buffer alist)))

  (setq purpose-action-sequences
        '((switch-to-buffer    . (purpose-display-reuse-window-buffer
                                  purpose-display-reuse-window-purpose
                                  +maybe-display-shackle
                                  purpose-display-maybe-same-window
                                  purpose-display-maybe-other-window
                                  purpose-display-maybe-other-frame
                                  purpose-display-maybe-pop-up-window
                                  purpose-display-maybe-pop-up-frame))
          (prefer-same-window  . (purpose-display-maybe-same-window
                                  purpose-display-reuse-window-buffer
                                  purpose-display-reuse-window-purpose
                                  +maybe-display-shackle
                                  purpose-display-maybe-other-window
                                  purpose-display-maybe-other-frame
                                  purpose-display-maybe-pop-up-window
                                  purpose-display-maybe-pop-up-frame
                                  ))
          (force-same-window   . (purpose-display-maybe-same-window))
          (prefer-other-window . (purpose-display-reuse-window-buffer
                                  purpose-display-reuse-window-purpose
                                  +maybe-display-shackle
                                  purpose-display-maybe-pop-up-frame
                                  purpose-display-maybe-pop-up-window
                                  purpose-display-maybe-other-window
                                  purpose-display-maybe-other-frame
                                  purpose-display-maybe-same-window
                                  ))
          (prefer-other-frame  . (purpose-display-reuse-window-buffer-other-frame
                                  purpose-display-reuse-window-purpose-other-frame
                                  +maybe-display-shackle
                                  purpose-display-maybe-other-frame
                                  purpose-display-maybe-pop-up-frame
                                  purpose-display-maybe-other-window
                                  purpose-display-maybe-pop-up-window
                                  purpose-display-reuse-window-buffer
                                  purpose-display-reuse-window-purpose
                                  purpose-display-maybe-same-window))))
  (shackle-mode t))

(use-package emacs :after telephone-line
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


(provide 'packages-windows)

;;; packages-windows.el ends here
