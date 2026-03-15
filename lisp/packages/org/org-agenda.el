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

(defun +org-agenda--get-headings (level &rest files)
  "Return list of categories in org FILE."
  (let (headings)
    (dolist (file files)
      (with-current-buffer (org-capture-target-buffer file)
        (org-with-wide-buffer
         (org-map-entries
          (lambda ()
            (when (= (org-outline-level) level)
              (pcase (org-get-heading t t t t)
                (heading (add-to-list 'headings (cons (substring-no-properties heading) (org-element-at-point)) t)))
              )))
         )))
    headings))

(defun +org-agenda-get-headings (filename &optional level)
  (let* ((headings (+org-agenda--get-headings (or level 1) filename))
         (choice (completing-read "Heading: " headings)))
    (or (assoc choice headings)
        (cons choice nil))))

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

  (org-default-notes-file "todo.org")
  (org-capture-templates
   '(("t" "Todo"
      entry
      (file+headline
       "todo.org"
       (lambda ()
         (let* ((categories (+org-agenda-get-categories "todo.org"))
                (choice (completing-read "Category: " categories)))
           choice)))
      "TODO %?\n  %i\n"
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
      :unnarrowed t)
     ("a" "Agenda"
      entry
      (function
       (lambda ()
         (let* ((filename (org-capture-expand-file (org-get-org-file))))
           (set-buffer (find-file-noselect filename))
           (pcase-let ((`(,headline . ,props)
                        (+org-agenda-get-headings filename 1)))
             (if props
                 (goto-char (org-element-property :begin props))
               (goto-char (point-max))
               (org-insert-heading nil t 1)
               (insert headline))
             ))
         ))
      "TODO %?\n  %i\n"
      :unnarrowed t)
     ))

  :hook
  (org-agenda-mode . +org-agenda-configure))

(use-package org-super-agenda :after org
  :config
  (setq org-agenda-custom-commands
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
