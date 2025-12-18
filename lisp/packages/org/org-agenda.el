;;; org/org-agenda.el -*- lexical-binding: t; -*-

(defun +org-agenda-configure ()
  "Org-agenda configuration."
  (setq-local window-size-fixed 'width))

(defun +org-agenda-get-categories (file)
  "Return list of categories in org FILE."
  (with-current-buffer (org-capture-target-buffer file)
    (let (categories)
      (org-map-entries
       (lambda ()
         (add-to-list 'categories (org-get-category) t)))
      categories)))

(use-package org-agenda :after org
  :custom
  (org-agenda-window-setup 'current-window)
  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid '((daily today require-timed)
                          (800 1000 1200 1400 1600 1800 2000)
                          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  :hook
  (org-agenda-mode . +org-agenda-configure))

(use-package org-super-agenda :after org-agenda
  :custom
  (org-agenda-custom-commands
   '(("n" "Today View"
      ((agenda "" ((org-agenda-span 'day)
                   (org-super-agenda-groups
                    '(( :name "Today"
                        :time-grid t
                        :scheduled today
                        :discard (:deadline t)
                        :order 0)
                      ( :habit t)
                      ( :name "Due Today"
                        :deadline today
                        :order 2)
                      ( :name "Due Soon"
                        :deadline future
                        :order 8)
                      ( :name "Overdue"
                        :deadline past
                        :order 7)
                      ))))
       ))
     ("t" "Todo View"
      ((todo "" ((org-agenda-overriding-header "Tasks")
                 (org-super-agenda-groups
                  '((:auto-category t)
                    (:discard (:todo "CANCELLED"))
                    ))))
       ))
     ))
  :hook
  (after-init . org-super-agenda-mode))
