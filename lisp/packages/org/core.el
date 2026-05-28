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

  (util/add-capf-hooks t
    #'yasnippet-capf
    #'cape-file
    #'cape-tex
    #'cape-elisp-block
    #'cape-keyword)

  (util/remove-capf-hooks t
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
  (org-startup-with-link-previews t)
  (org-image-align 'left)
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

  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis (concat " " (truncate-string-ellipsis) " ")) ;; folding symbol
  (org-use-sub-superscripts '{})

  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)

  (org-src-block-faces nil)
  (org-confirm-babel-evaluate nil)
  (org-plantuml-exec-mode 'plantuml)

  (org-expiry-inactive-timestamps t)

  (org-todo-keywords
   '((sequence "TODO" "ONGOING" "TESTING" "PENDING" "|" "DONE" "VOID" )))

  (org-todo-keyword-faces
   `(("TODO"    . (
                   :inherit default
                   :weight bold
                   :foreground ,(doom-darken (doom-color 'red) 0.2)))
     ("ONGOING" . (
                   :inherit default
                   :weight bold
                   :foreground ,(doom-darken (doom-color 'orange) 0.2)))
     ("TESTING" . (
                   :inherit default
                   :weight bold
                   :foreground ,(doom-darken (doom-color 'yellow) 0.2)))
     ("PENDING" . (
                   :inherit default
                   :weight bold
                   :foreground ,(doom-darken (doom-color 'yellow) 0.2)))
     ("DONE"    . (
                   :inherit default
                   :weight bold
                   :foreground ,(doom-darken (doom-color 'green) 0.2)))
     ("VOID"    . (
                   :inherit default
                   :weight bold
                   :foreground ,(doom-darken (doom-color 'fg-alt) 0.2)))
     ))

  (org-agenda-deadline-faces
   '((1.00001 . org-warning)
     (1.00000 . org-imminent-deadline)
     (0.50000 . org-upcoming-deadline)
     (0.00000 . org-upcoming-distant-deadline)))

  :custom-face
  (org-imminent-deadline
   ((nil :inherit unspecifed :foreground ,(doom-color 'fg))))
  (org-upcoming-deadline
   ((nil :inherit unspecifed :foreground ,(doom-color 'fg-alt))))
  (org-upcoming-distant-deadline
   ((nil :inherit unspecifed :foreground ,(doom-color 'grey))))

  :hook
  (org-mode . +org-mode-setup)
  (org-mode . +org-fold-auto-hide-block-languages))

(use-package org-contrib :after org
  :config
  (require 'ox-extra)
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))
