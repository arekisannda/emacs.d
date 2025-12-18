;;; org/core.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'util-strings)
(require 'util-lang)
(require 'mule-util)

(defcustom +org-auto-hide-block-languages '()
  "List of languages to auto hide."
  :type '(repeat string)
  :group 'org)

(defun +org-fold-auto-hide-block-languages ()
  "Fold blocks matching languages in `+org-auto-hide-block-languages'."
  (interactive)
  (org-block-map
   (lambda ()
     (let* ((element (org-element-at-point))
            (lang (org-element-property :language element))
            (params (org-element-property :parameters element))
            (hidden (and params (string-match-p ":hidden t" params))))
       (if (or hidden (cl-some (lambda (l) (equal lang l)) +org-auto-hide-block-languages))
           (org-fold--hide-wrapper-toggle element 'block 'hide nil))))))

(defun +org-mode-setup ()
  "Setup to run for `org-mode` major modes."
  (setq-local face-font-rescale-alist
              `(("-cdac$" . 1.3)
                (,(font-spec :family "Source Han Sans") . 1.3))
              )
  (let ((font-family (org-entry-get (point-min) "font-family" t))
        (font-height (org-entry-get (point-min) "font-height" t)))
    (when font-family
      (setq-local buffer-face-mode-face `(:family ,font-family)))
    (when font-height
      (setq-local buffer-face-mode-face `(:height ,(string-to-number font-height)))))

  (visual-line-mode 1)
  (org-modern-mode 1)
  (flyspell-mode 1)
  (completion-preview-mode 1)
  (yas-minor-mode 1)

  (util/add-capf-hooks
   #'yasnippet-capf
   #'cape-file
   #'cape-tex
   #'cape-elisp-block
   #'cape-keyword)

  (util/remove-capf-hooks
   #'pcomplete-completions-at-point
   t))

(use-package org
  :custom
  (+org-auto-hide-block-languages '("mermaid"))

  (org-pretty-entities-include-sub-superscripts nil)
  (org-use-tag-inheritance nil)
  (org-link-descriptive t)
  (org-link-frame-setup '((file . find-file)))
  (org-startup-with-inline-images t)
  (org-image-align 'center)
  (org-image-actual-width nil)
  (org-startup-indented t)
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-cycle-level-faces nil)
  (org-read-date-popup-calendar nil)
  (org-read-date-display-live t)

  (org-src-preserve-indentation nil)
  (org-src-window-setup 'current-window)
  (org-edit-src-persistent-message nil)
  (org-edit-src-content-indentation 0)

  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis (concat " " (truncate-string-ellipsis) " ")) ;; folding symbol
  (org-use-sub-superscripts '{})

  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)

  (org-todo-keywords
   '((sequence "TODO" "ONGOING" "|" "DONE" "CANCELLED" )))

  (org-todo-keyword-faces
   `(("TODO"      . (
                     :inherit default
                     :weight bold
                     :foreground ,(doom-darken (doom-color 'red) 0.2)))
     ("ONGOING"   . (
                     :inherit default
                     :weight bold
                     :foreground ,(doom-darken (doom-color 'orange) 0.2)))
     ("CANCELLED" . (
                     :inherit default
                     :weight bold
                     :foreground ,(doom-darken (doom-color 'fg-alt) 0.3)))
     ("DONE"      . (
                     :inherit default
                     :weight bold
                     :foreground ,(doom-color 'fg-alt) 0.2))))

  (org-src-block-faces nil)
  (org-confirm-babel-evaluate nil)
  (org-plantuml-exec-mode 'plantuml)

  (org-default-notes-file "todo.org")
  (org-expiry-inactive-timestamps t)
  (org-capture-templates
   '(("t" "Todo"
      entry
      (file+headline
       "todo.org"
       (lambda ()
         (let* ((categories (+org-agenda-get-categories "todo.org"))
                (choice (completing-read "Category: " categories)))
           choice)))
      "* TODO %?\n  %i\n"
      :unnarrowed t)
     ("s" "Schedule"
      entry (file+olp+datetree "schedule.org")
      "%T %?\n"
      :time-prompt t
      :tree-type month
      :unnarrowed t)
     ("d" "Schedule Deadline"
      entry (file+olp+datetree "schedule.org")
      "%T %?\nDEADLINE: %^{DEADLINE}T\n"
      :time-prompt t
      :tree-type month
      :unnarrowed t)
     ("j" "Journal"
      entry (file+olp+datetree "journal.org")
      "%T %?\n %i\n"
      :unnarrowed t)))
  :hook
  (org-mode . +org-mode-setup)
  (org-mode . +org-fold-auto-hide-block-languages))

(use-package org-contrib :after org
  :config
  (require 'ox-extra)
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))
