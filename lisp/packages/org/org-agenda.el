;;; org/org-agenda.el -*- lexical-binding: t; -*-

(defun +org-agenda-configure ()
  "Org-agenda configuration."
  (setq-local window-size-fixed 'width))

(defun +org-agenda-get-categories (&rest files)
  "Return list of categories in org FILE."
  (let (categories)
    (dolist (file files)
      (with-current-buffer (org-capture-target-buffer file)
        (org-with-wide-buffer
         (org-map-entries
          (lambda ()
            (pcase (org-get-category)
              ("???" nil)
              (category (add-to-list 'categories category t)))
            ))
         ))
      )
    (delete-dups categories)))

(use-package org-agenda :after org
  :custom
  (org-agenda-window-setup 'current-window)
  (org-agenda-sticky nil)
  (org-agenda-compact-blocks nil)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-include-deadlines t)
  (org-agenda-include-diary nil)
  (org-agenda-block-separator t)

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
   `(
     ("t" "All Todo View"
      alltodo ""
      ((org-agenda-buffer-tmp-name "*Org Agenda TODO*")
       (org-super-agenda-groups
        '(( :auto-map
            (lambda (item)
              (-when-let* ((marker (or (get-text-property 0 'org-marker item)
                                       (get-text-property 0 'org-hd-marker item)))
                           (file-path (->> marker marker-buffer buffer-file-name))
                           (directory-name (->> file-path file-name-directory directory-file-name)))
                (concat (capitalize (file-name-nondirectory directory-name)) ": " (file-name-nondirectory file-path)))))
          ( :ancestor-with-todo t)
          ( :discard (:todo "CANCELLED"))
          ))
       ))

     ("T" "Task View"
      todo ""
      ((org-agenda-buffer-tmp-name "*Org Agenda Tasks*")
       (org-agenda-files '("todo.org"))
       (org-super-agenda-groups
        '(( :order-multi
            (2
             ,@(mapcar
                (lambda (category)
                  (list :name (capitalize category) :category category))
                (apply #'+org-agenda-get-categories (org-agenda-files)))
             ))
          ( :discard (:todo "CANCELLED"))
          ))
       ))

     ("k" "Kanban View"
      todo "CANCELLED|DONE|TESTING|ONGOING|TODO"
      ((org-agenda-buffer-tmp-name "*Org Agenda Kanban*")
       (org-agenda-overriding-header "Kanban Board")
       (org-super-agenda-groups
        '(( :name "To Do"     :todo "TODO")
          ( :name "Ongoing"   :todo "ONGOING")
          ( :name "Testing"   :todo "TESTING")
          ( :name "Done"      :todo "DONE")
          ( :name "Cancelled" :todo "CANCELLED")
          ))
       ))

     ("A" "Today View"
      agenda ""
      ((org-agenda-buffer-tmp-name "*Org Agenda Today*")
       (org-agenda-span 'day)
       (org-super-agenda-groups
        '(( :name "Today"
            :time-grid t
            :scheduled today
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
          (:auto-category t)
          ))
       ))

     ("a" "Week View"
      agenda ""
      ((org-agenda-buffer-tmp-name "*Org Agenda*")
       (org-agenda-span 'fortnight)
       (org-agenda-start-on-weekday 0)
       (org-super-agenda-groups
        '(( :name "Today"
            :time-grid t)
          ( :name "Due Today"
            :deadline today)
          ( :name "Overdue"
            :deadline past)
          ( :name "Due This Week"
            :deadline future)
          ( :name "Scheduled"
            :scheduled future)
          ( :auto-category t)))))
     ))
  :hook
  (window-setup . org-super-agenda-mode))
