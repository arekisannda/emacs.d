;;; tools/shackle.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'util-strings)
(require 'util-windows)

(setq +shackle-max-width 120)
(setq +shackle-min-right-width 110)
(setq +shackle-min-left-width 40)
(setq +shackle-min-bottom-height 20)

(defun +shackle-get-dimensions (side)
  (let ((min-width (or (and (eq side 'left) +shackle-min-left-width)
                       (and (eq side 'right) +shackle-min-right-width)))
        (min-height +shackle-min-bottom-height)
        (avail-width (let ((edges (window-edges (frame-root-window))))
                       (- (nth 2 edges) (nth 0 edges) 40))))
    `((window-width . ,(min +shackle-max-width
                            (max min-width
                                 (/ avail-width (/ (frame-width) +shackle-max-width)))))
      (window-height . ,(and (cl-find side '(bottom top)) min-height))
      )))

(defun +shackle-switch-function (buffer-or-name action-list)
  "Return action from ACTION-LIST for BUFFER-OR-NAME."
  (cl-loop for (condition . plist) in action-list
           when (shackle--match buffer-or-name condition plist)
           return plist
           finally return nil))

(use-package shackle :after windex
  :custom
  (util/windows-display-buffer-by-condition-switch-function #'+shackle-switch-function)
  (shackle-default-rule nil)
  (shackle-rules
   `((("^\\*Capture\\*$"
       "^\\*Warnings\\*$"
       "^\\*Flymake log\\*$"
       "^\\*Activities (error): .*\\*$"
       "^ \\*http.*\\*")
      :ignore t)

     ((scad-preview-mode)
      :same t)

     ((treemacs-mode)
      :custom util/windows-display-buffer-in-side-window
      :side left
      :slot 0
      :size +shackle-get-dimensions
      :fixed width
      :select t)

     ((magit-mode)
      :custom
      (lambda (buffer &optional alist plist)
        (windex-frame-display-buffer buffer `(,@alist (title . "Magit")))))

     (("^\\*Shell Command Output\\*$"
       "^\\*shell\\*$"
       "^\\*Command Line\\*$"

       compilation-mode)
      :if (lambda (window) (not compilation-display-buffer))
      :ignore t)

     (("^\\*Shell Command Output\\*$"
       "^\\*shell\\*$"
       "^\\*Command Line\\*$"

       compilation-mode)
      :if (lambda (window) compilation-display-buffer)
      :custom util/windows-display-buffer-in-side-window
      :side bottom
      :slot 0
      :size ,+shackle-min-bottom-height
      :fixed height)

     (("^ \\*transient\\*$"
       "^ \\*CDLaTeX Help\\*")
      :custom util/windows-display-buffer-by-condition
      :fallback ( :action util/windows-display-buffer-in-pop-up-window
                  :select t)
      :conditions
      (((".*")
        :if (lambda (window)
              (equal (window-parameter window 'window-side) 'bottom))
        :action util/windows-display-buffer-in-side-window
        :side bottom
        :slot 0
        :size ,+shackle-min-bottom-height
        :fixed height
        :select t)

       ((".*")
        :if (lambda (window)
              (equal (window-parameter window 'window-side) 'right))
        :action util/windows-display-buffer-in-side-window
        :side right
        :slot 1
        :size +shackle-get-dimensions
        :fixed width
        :dedicated t
        :select t)

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
      :custom util/windows-display-buffer-in-pop-up-window
      :select t)

     ((dashboard-mode
       pdf-view-mode

       "^\\*Org Src.*\\*$"
       "^\\*Org Preview.*\\*$"
       "^\\*Org Select\\*$")
      :same t :select t)

     (("^CAPTURE-.*\\.org$"
       "^\\*Org .*\\*$"
       "^\\*Dictionary\\*$"
       "^\\*Customize Apropos\\*$"
       "^\\*Shortdoc.*\\*$"
       "^\\*Customize.*\\*$"
       "^\\*Man.*\\*$"
       "^\\*WoMan.*\\*$"
       "^\\*IBuffer\\*$"

       devdocs-mode
       dictionary-mode)
      :custom util/windows-display-buffer-in-side-window
      :side right
      :slot 0
      :size +shackle-get-dimensions
      :fixed width
      :select t)

     (("^\\*org-roam\\*$"
       "^\\*eldoc.*\\*"
       "^ \\*eglot doc\\*$"
       "^\\*yasnippet-capf-doc\\*$"
       "^\\*corfu doc.*\\*$"
       "^ \\*Agenda Commands\\*$"

       org-roam-mode
       man-common)
      :custom util/windows-display-buffer-in-side-window
      :side right
      :slot 0
      :size +shackle-get-dimensions
      :fixed width)

     (("^\\*Error\\*$"
       "^\\*Dired log\\*$"
       "^\\*latex-scratch\\*$"
       "^\\*org-scratch\\*$"
       "^\\*remark-notes\\*$"
       "^\\*Org Links\\*$"
       "^\\*\\(.*-\\)?eshell\\*$"
       "^\\*Calculator\\*$"
       "^ \\*EGLOT .* stderr\\*$"
       "^\\*EGLOT .* events\\*$"
       "^\\*ChatGPT.*\\*$"
       "^\\*Code Review Comment\\*$"
       "^COMMIT_EDITMSG$"

       ert-results-mode
       code-review-comment-mode
       forge-post-mode
       dired-mode
       calc-mode
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
       calendar-mode
       tabulated-list-mode)
      :custom util/windows-display-buffer-in-side-window
      :side bottom
      :slot 0
      :size ,+shackle-min-bottom-height
      :fixed height
      :select t)

     (("^\\*Edit Formulas\\*")
      :custom util/windows-display-buffer-in-side-window
      :side bottom
      :slot 1
      :size ,+shackle-min-bottom-height
      :fixed height
      :select t)

     (("^\\*Calc Trail\\*$"
       calc-trail-mode)
      :custom util/windows-display-buffer-in-side-window
      :side bottom
      :slot 1
      :size ,+shackle-min-bottom-height
      :fixed height)

     ;;; base mode fallback
     ((Custom-mode
       special-mode
       help-mode
       Info-mode)
      :custom util/windows-display-buffer-in-side-window
      :side right
      :slot 0
      :size +shackle-get-dimensions
      :fixed width
      :select t)

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
        :side right
        :slot 1
        :size +shackle-get-dimensions
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

       ((help-mode
         Custom-mode
         dired-mode)
        :mru t :select t :reuse t)

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
                  ((listp e) (string-match (car e) buffer-name))
                  ((stringp e) (string= buffer-name e))
                  ((symbolp e) (eq buffer-mode e))))
               `(,which-key-buffer-name
                 ,embrace--help-buffer-name
                 ,code-review-buffer-name
                 eww-mode))
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
