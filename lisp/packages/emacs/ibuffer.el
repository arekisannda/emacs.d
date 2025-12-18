;;; emacs/ibuffer.el -*- lexical-binding: t; -*-

(use-package ibuffer-project :after project
  :preface
  (defun +ibuffer-list ()
    (interactive)
    (ibuffer nil nil nil t nil nil nil))
  :hook
  (ibuffer . (lambda ()
               (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
               (unless (eq ibuffer-sorting-mode 'project-file-relative)
                 (ibuffer-do-sort-by-project-file-relative)))))
