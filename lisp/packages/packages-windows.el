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
        elpaca-log-mode
        log4e-mode
        messages-buffer-mode
        xref--xref-buffer-mode
        grep-mode
        occur-mode
        compilation-mode
        "*remark-notes*"))

(setq +wm-bottom-rule-list-regex
      '("^\\*Google Translate.*\\*$"
        "^ \\*log4e-.*\\*$"))

(setq +wm-bottom-side-rule-list
      '(backtrace-mode
        "*Org Links*"
        "*diff-hl*"
        "*diff-hl-show-hunk-diff-buffer*"
        "*diff-hl-show-hunk-buffer*"
        "*shell*"
        "*Shell Command Output*"))

(setq +wm-right-rule-list
      '(elpaca-manager-mode
        elpaca-ui-mode
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
        calendar-mode

        "*info*"
        "*Ibuffer*"
        "*elpaca-manager*"
        "*Customize Apropos*"))

(setq +wm-right-rule-list-regex
      '("^\\*Shortdoc.*\\*$"
        "^\\*Customize.*\\*$"
        "^\\*Org Agenda.*\\*$"
        "^\\*Org Select\\*$"))

(setq +wm-left-rule-list '())
(setq +wm-left-rule-list-regex '())

(setq +wm-skip-rule-match-list
      '(treemacs-mode
        ("^ \\*Treemacs.*\\*$" :regexp t)))

(setq +wm-right-width 90)
(setq +wm-left-width 35)
(setq +wm-bottom-height 20)

(use-package shackle
  :custom
  (aw-ignored-buffers
   (append '(treemacs-mode
             "*Calc Trail*"
             " *LV*")
           +wm-right-rule-list
           +wm-bottom-rule-list))
  (shackle-default-rule nil)
  (shackle-rules
   `((,+wm-bottom-rule-list
      :custom +display-buffer-in-popup-window
      :side below :size ,+wm-bottom-height
      :fixed height
      :select t)

     (,+wm-bottom-side-rule-list
      :custom +display-buffer-in-side-window
      :side bottom :slot 0
      :select t)

     (,+wm-left-rule-list
      :custom +display-buffer-in-side-window
      :side left :slot 1 :size ,+wm-left-width)

     (,+wm-left-rule-list-regex
      :custom +display-buffer-in-side-window
      :side left :slot 1 :size ,+wm-left-width
      :regexp t)

     (,+wm-right-rule-list
      :custom +display-buffer-in-side-window
      :side right :slot 0 :size ,+wm-right-width
      :fixed width
      :select t)

     (,+wm-right-rule-list-regex
      :custom +display-buffer-in-side-window
      :side right :slot 0 :size ,+wm-right-width
      :fixed width
      :select t
      :regexp t)

     (magit-diff-mode
      :custom +display-buffer-in-side-window
      :side right :slot 1 :size ,+wm-right-width
      :fixed width)

     ((lisp-interaction-mode
       "*org-scratch*"
       ert-results-mode)
      :select t
      :custom +display-buffer-in-side-window
      :side right :slot 1 :size ,+wm-right-width
      :fixed width)

     (prog-mode
      :custom +dynamic-display-buffer
      :static (:mru t :select t)
      :dynamic
      ((help-mode
        :action +display-buffer-in-side-window
        :side right :slot 1 :size ,+wm-right-width
        :fixed width
        :select t)

       (embark-collect-mode :mru t :select t)

       ((backtrace-mode
         compilation-mode)
        :action +display-buffer-in-side-window
        :side right :slot 0 :size ,+wm-right-width
        :fixed width
        :select t)))

     (outline-mode
      :custom +dynamic-display-buffer
      :static (:mru t :select t)
      :dynamic
      ((org-agenda-mode
        :if (lambda () org-agenda-follow-mode)
        :action +display-buffer-in-side-window
        :side right :slot 1 :size ,+wm-right-width
        :fixed width
        :select t)))
     ))
  :config
  (defun +shackle-condition-ignore-check (orig-func &rest args)
    (let* ((buffer (get-buffer-create (nth 0 args)))
           (buffer-name (buffer-name buffer))
           (buffer-mode (buffer-local-value 'major-mode buffer)))
      (unless (cl-some
               (lambda (e)
                 (cond
                  ((listp e) (string-match (car e) (buffer-name buffer)))
                  ((stringp e) (equal buffer-name e))
                  ((symbolp e) (eq buffer-mode e))))
               +wm-skip-rule-match-list)
        (apply orig-func args))))

  (advice-add #'shackle-display-buffer-condition :around #'+shackle-condition-ignore-check)
  (advice-add #'shackle--match :override #'+window--action-match)

  ;; add `shackle-mode` guard to prevent adding duplicates in
  ;; `display-buffer-alist`
  (unless shackle-mode (shackle-mode t)))

(use-package emacs :after telephone-line
  :ensure nil
  :config
  (telephone-line-defsegment* +telepohone-line-buffer-dedicated-tag-segment ()
    (if (window-dedicated-p nil) "" ""))

  (setq telephone-line-lhs
        '((evil   . (+telepohone-line-buffer-dedicated-tag-segment
                     telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-projectile-segment
                     telephone-line-buffer-segment))))

  (setq telephone-line-rhs
        '((nil    . (telephone-line-flycheck-segment
                     telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment)))
        ))

(provide 'packages-windows)

;;; packages-windows.el ends here
