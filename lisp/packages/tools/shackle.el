;;; tools/shackle.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'util-strings)
(require 'util-windows)

(defvar-local +shackle-frame-init-buffer nil)

(defun +shackle-get-dimensions (side)
  (let ((min-width (or (and (eq side 'left) util/windows-min-left-width)
                       (and (eq side 'right) util/windows-min-right-width)))
        (max-width (or (and (eq side 'left) util/windows-min-left-width)
                       (and (eq side 'right) util/windows-max-width)))
        (min-height util/windows-min-bottom-height)
        (avail-width (let ((edges (window-edges (frame-root-window))))
                       (- (nth 2 edges) (nth 0 edges) util/windows-min-left-width))))
    `((window-width . ,(and (cl-find side '(left right))
                            (min max-width
                                 (max min-width
                                      (/ avail-width (/ (frame-width) util/windows-max-width))))))
      (window-height . ,(and (cl-find side '(bottom top)) min-height))
      )))

(defun +shackle-switch-function (buffer-or-name action-list)
  "Return action from ACTION-LIST for BUFFER-OR-NAME."
  (cl-loop for (condition . plist) in action-list
           when (shackle--match buffer-or-name condition plist)
           return plist
           finally return nil))

(defcustom shackle-disable-list nil
  "List of conditions to be disabled by `shackle'."
  :type '(choice :tag "Condition"
                 (symbol :tag "Major mode")
                 (string :tag "Buffer name")
                 (repeat (choice
                          (symbol :tag "Major mode")
                          (string :tag "Buffer name")))
                 (list :tag "Custom function"
                       (const :tag "Custom" :custom) function)))

(defun +shackle-bottom-preset-size-0 ()
  `( :custom util/windows-display-buffer-in-side-window
     :side bottom
     :slot 0
     :dedicated t
     :size ,util/windows-min-bottom-height
     :fixed height))

(defun +shackle-bottom-select-preset-size-0 ()
  `(,@(+shackle-bottom-preset-size-0) :select t))

(defun +shackle-bottom-preset-size-1 ()
  `( :custom util/windows-display-buffer-in-side-window
     :side bottom
     :slot 1
     :size ,util/windows-min-bottom-height
     :flags (enable-only-buffer-tab-line)
     :fixed height))

(defun +shackle-bottom-select-preset-size-1 ()
  `(,@(+shackle-bottom-preset-size-1) :select t))

(defun +shackle-left-preset-size-0 ()
  `( :custom util/windows-display-buffer-in-side-window
     :side left
     :slot 0
     :size ,util/windows-min-left-width
     :fixed width))

(defun +shackle-right-preset-0 ()
  `( :custom util/windows-display-buffer-in-side-window
     :side right
     :slot 0
     :flags (enable-only-buffer-tab-line)
     :dedicated t
     :fixed width))

(defun +shackle-right-select-preset-0 ()
  `(,@(+shackle-right-preset-0) :select t))

(defun +shackle-right-preset-1 ()
  `( :custom util/windows-display-buffer-in-side-window
     :side right
     :slot 1
     :flags (disable-tab-line)
     :dedicated t
     :fixed width))

(defun +shackle-right-select-preset-1 ()
  `(,@(+shackle-right-preset-1) :select t))

(use-package shackle :after windex
  :custom
  (util/windows-display-buffer-by-condition-switch-function #'+shackle-switch-function)
  (util/windows-max-width 100)
  (util/windows-min-bottom-height 25)
  (util/windows-min-left-width 60)
  (treemacs-width util/windows-min-left-width)
  (shackle-default-rule nil)
  (shackle-disable-list
   `("^ \\*which-key\\*$"
     treemacs-mode
     leetcode--problems-mode
     leetcode--problem-detail-mode))

  (shackle-rules
   `((("^\\*Capture\\*$"
       "^\\*Warnings\\*$"
       "^\\*Flymake log\\*$"
       "^\\*Activities (error): .*\\*$"
       "^\\*leetcode-result-.*\\*$"
       "^\\*leetcode-testcase-.*\\*$"
       "^ \\*http.*\\*")
      :ignore t)

     ((scad-preview-mode
       code-review-mode
       pr-review-mode)
      :same t)

     ((magit-mode
       forge-repository-list-mode)
      :custom util/windows-display-buffer-by-condition
      :fallback ( :action (lambda (buffer &optional alist plist)
                            (with-current-buffer buffer
                              (setq-local +shackle-frame-init-buffer t))

                            (windex-frame-display-buffer
                             buffer
                             `(,@alist
                               (name . ,(format "Emacs Tool")))
                             )))
      :conditions
      (((magit-mode)
        :same t :select t)
       ))

     (("^\\*Shell Command Output\\*$"
       "^\\*shell\\*$"

       compilation-mode)
      :if (lambda (window) (not compilation-display-buffer))
      :ignore t)

     (("^\\*Shell Command Output\\*$"
       "^\\*shell\\*$"

       compilation-mode)
      :if (lambda (window) compilation-display-buffer)
      ,@(+shackle-bottom-preset-size-0))

     (("^ \\*transient\\*$"
       "^ \\*CDLaTeX Help\\*"
       "^\\*Command Line\\*$"
       "^\\*trace-output\\*$"

       calendar-mode
       evil-command-window-mode)
      :custom util/windows-display-buffer-by-condition
      :fallback ( :action util/windows-display-buffer-in-pop-up-window
                  :select t)
      :conditions
      (((".*")
        :if (lambda (window) (window-parameter window 'window-popup))
        :same t :select t)
       ))

     (("^\\*diff-hl\\*"
       "^\\*diff-hl-revert\\*"
       "^\\*diff-hl-show-hunk-diff-buffer\\*"
       "^\\*diff-hl-show-hunk-buffer\\*"
       "^\\*Deletions\\*$"
       "^ widget-choose$"
       "^ \\*which-key\\*$"
       "^\\*Ibuffer confirmation\\*"
       "^\\*Local Variables\\*$"
       "^\\*Completions\\*$"
       "^ \\*Agenda Commands\\*$"
       "^\\*Backtrace\\*$"

       backtrace-mode)
      :custom util/windows-display-buffer-in-pop-up-window
      :select t)

     (("^\\*Dictionary\\*$"
       "^\\*Customize Apropos\\*$"
       "^\\*Customize .*\\*$"
       "^\\*Shortdoc.*\\*$"
       "^\\*Customize.*\\*$"
       "^\\*Man.*\\*$"
       "^\\*WoMan.*\\*$"
       "^\\*eww\\*$"
       "^\\*w3m\\*$"
       "^\\*Org Agenda .*\\*$"
       "^\\*Org Agenda\\*$"
       "^\\*ChatGPT.*\\*$"
       "^\\*Claude.*\\*$"
       "^\\*Org Select\\*$"

       Custom-mode
       calc-mode
       org-agenda-mode
       w3m-mode
       eww-mode
       devdocs-mode
       dictionary-mode)
      ,@(+shackle-right-select-preset-0)
      :size +shackle-get-dimensions)

     (("^\\*eldoc.*\\*"
       "^ \\*eglot doc\\*$"
       "^\\*yasnippet-capf-doc\\*$"
       "^\\*corfu doc.*\\*$"
       "^\\*org-roam\\*$"
       "\\*Gnuplot Commands\\*"
       "\\*Gnuplot Trail\\*"

       calc-trail-mode
       org-roam-mode)
      ,@(+shackle-right-preset-1)
      :size +shackle-get-dimensions)

     (("^\\*Org Preview.*\\*$"
       "^\\*Org Src.*\\*$"
       "^CAPTURE-.*\\.org$"

       dashboard-mode
       pdf-view-mode)
      :same t :select t)

     (("^\\*Error\\*$"
       "^\\*Dired log\\*$"
       "^\\*latex-scratch\\*$"
       "^\\*org-scratch\\*$"
       "^\\*remark-notes\\*$"
       "^\\*Org Links\\*$"
       "^\\*\\(.*-\\)?eshell\\*$"
       "^ \\*.* stderr\\*$"
       "^\\*.* events\\*$"
       "^\\*Code Review Comment\\*$"
       "^COMMIT_EDITMSG$"
       "^\\*detached-session-info\\*$"
       "^\\*detached-list\\*$"
       "^\\*IBuffer\\*$"
       "^\\*envrc\\*$"

       git-rebase-mode
       detached-list-mode
       detached-log-mode
       ert-results-mode
       code-review-comment-mode
       pr-review-input-mode
       forge-post-mode
       dired-mode
       eshell-mode
       comint-mode
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
       tabulated-list-mode)
      ,@(+shackle-bottom-select-preset-size-0))

     (("^\\*Edit Formulas\\*")
      ,@(+shackle-bottom-select-preset-size-1))

     (("^\\*Org .*\\*$")
      :custom util/windows-display-buffer-in-pop-up-window
      :select t)

     ;;; base mode fallback
     ((help-mode)
      ,@(+shackle-right-select-preset-1)
      :size +shackle-get-dimensions)

     ((special-mode
       Info-mode)
      ,@(+shackle-right-select-preset-0)
      :size +shackle-get-dimensions)

     ((prog-mode
       text-mode
       conf-mode
       outline-mode
       fundamental-mode)
      :custom util/windows-display-buffer-by-condition
      :fallback (:same t :select t)
      :conditions
      (((org-agenda-mode)
        :if (lambda (&rest _) org-agenda-follow-mode)
        :action util/windows-display-buffer-in-side-window
        ,@(+shackle-right-select-preset-1)
        :flags (disable-tab-line)
        :size +shackle-get-dimensions)

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

       ((prog-mode
         text-mode
         conf-mode
         outline-mode)
        :same t :select t)
       ))
     ))
  :config
  (defun +shackle-condition-ignore-check (orig-func &rest args)
    (let* ((buffer (get-buffer-create (nth 0 args)))
           (buffer-name (buffer-name buffer))
           (buffer-mode (buffer-local-value 'major-mode buffer)))
      (unless (cl-some
               (lambda (e)
                 (cond
                  ((and (stringp e) (string-match-p e buffer-name)) t)
                  ((stringp e) (string= buffer-name e))
                  ((symbolp e) (eq buffer-mode e))))
               shackle-disable-list)
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

  (defun +shackle--display-buffer-same (buffer alist)
    "Display BUFFER in the currently selected window.
ALIST is passed to `shackle--window-display-buffer' internally."
    (unless (window-minibuffer-p)
      (let ((window (display-buffer-same-window buffer alist)))
        (prog1 window
          (when shackle-inhibit-window-quit-on-same-windows
            (shackle--inhibit-window-quit window))))))

  (advice-add #'shackle--display-buffer-same :override #'+shackle--display-buffer-same)

  (defun +shackle-quit-restore-window-around (fn &optional window bury-or-kill)
    (let ((buffer (window-buffer window)))
      (with-current-buffer buffer
        (if +shackle-frame-init-buffer
            (delete-frame)
          (funcall fn window bury-or-kill)))
      ))

  (advice-add #'quit-restore-window :around #'+shackle-quit-restore-window-around)

  ;; add `shackle-mode` guard to prevent adding duplicates in
  ;; `display-buffer-alist`
  (unless shackle-mode (shackle-mode t)))
