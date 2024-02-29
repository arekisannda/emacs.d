;;; packages-project.el --- Project Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package project
  :ensure nil
  :custom
  (project-vc-extra-root-markers '(".dir-locals.el"))
  (project-vc-include-untracked nil)
  (project-vc-merge-submodules nil))

(use-package ibuffer-project
  :preface
  (require 'ibuf-ext)
  :hook
  (ibuffer . (lambda () (setq ibuffer-filter-groups
                              (ibuffer-project-generate-filter-groups)))))

(provide 'packages-project)

;;; packages-project.el ends here
