;;; packages-windows.el --- Window Management Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'lib-window-extras)

(setq +wm-right-width 95)
(setq +wm-left-width 40)
(setq +wm-bottom-height 20)

(use-package shackle
  :custom
  (shackle-default-rule nil)
  (shackle-rules
   `((("*Capture*") :ignore t)
     (("\\*Org Src.*\\*$") :same t :regexp t)

     ((flymake-diagnostics-buffer-mode
       flymake-project-diagnostics-mode
       elpaca-log-mode)
      :custom +display-buffer-in-popup-window
      :side below :size ,+wm-bottom-height
      :dedicated right
      :fixed height
      :select t)

     ((backtrace-mode)
      :custom +display-buffer-in-side-window
      :side bottom :slot 0
      :fixed height
      :select t)

     ((elpaca-ui-mode
       magit-status-mode
       calc-mode

       "*Ibuffer*"
       "*elpaca-manager*"
       "*Customize Apropos*")
      :custom +display-buffer-in-side-window
      :side right :slot 0 :size ,+wm-right-width
      :dedicated right
      :fixed width
      :select t)

     (("^\\*Shortdoc.*\\*$"
       "^\\*Customize.*\\*$"
       "^\\*ChatGPT.*\\*$"
       "\\*Org .*\\*$")
      :custom +display-buffer-in-side-window
      :side right :slot 0 :size ,+wm-right-width
      :dedicated right
      :fixed width
      :select t
      :regexp t)

     ((help-mode
       Info-mode
       Man-mode

       "*info*"
       " *Agenda Commands*")
      :custom +display-buffer-in-side-window
      :side right :slot 0 :size ,+wm-right-width
      :fixed width)

     (("^\\*eldoc.*\\*"
       "\\*eglot doc\\*")
      :custom +display-buffer-in-side-window
      :side right :slot 0 :size ,+wm-right-width
      :fixed width
      :regexp t)

     ((magit-log-mode
       magit-diff-mode
       calc-trail-mode)
      :custom +display-buffer-in-side-window
      :side right :slot 1 :size ,+wm-right-width
      :fixed width
      :select t)

     ((vterm-mode
       comint-mode
       eshell-mode
       term-mode
       log4e-mode
       messages-buffer-mode
       xref--xref-buffer-mode
       grep-mode
       occur-mode
       compilation-mode
       org-roam-mode
       lisp-interaction-mode
       ert-results-mode

       "*latex-scratch*"
       "*org-scratch*"
       "*Org Select*"
       "*remark-notes*"
       "*Error*")
      :custom +display-buffer-in-popup-window
      :side below :size ,+wm-bottom-height
      :dedicated bottom
      :fixed height
      :select t)

     (("*Org Links*"
       "*diff-hl*"
       "*diff-hl-show-hunk-diff-buffer*"
       "*diff-hl-show-hunk-buffer*"
       "*shell*"
       "*Shell Command Output*")
      :custom +display-buffer-in-side-window
      :side bottom :slot 0
      :fixed height
      :select t)

     ;; base modes {{{

     ((prog-mode
       conf-mode)
      :custom +dynamic-display-buffer
      :static (:mru t :select t)
      :dynamic
      (((help-mode
         prog-mode)
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
        :select t)
       (org-roam-mode :mru t :select t)))

     ((Custom-mode
       tabulated-list-mode
       special-mode)
      :custom +display-buffer-in-side-window
      :side right :slot 0 :size ,+wm-right-width
      :dedicated right
      :fixed width
      :select t)

     ((calendar-mode)
      :custom +display-buffer-in-popup-window
      :side below :size ,+wm-bottom-height
      :dedicated bottom
      :select t)

     ;; }}}
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
               '(treemacs-mode
                 ("^ \\*Treemacs.*\\*$" :regexp t)))
        (apply orig-func args))))

  (advice-add #'shackle-display-buffer-condition :around #'+shackle-condition-ignore-check)
  (advice-add #'shackle--match :override #'+window--action-match)

  ;; add `shackle-mode` guard to prevent adding duplicates in
  ;; `display-buffer-alist`
  (unless shackle-mode (shackle-mode t)))

(provide 'packages-windows)

;;; packages-windows.el ends here
