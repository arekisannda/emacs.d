;;; emacs/project.el -*- lexical-binding: t; -*-

(use-package project
  :custom
  (project-vc-extra-root-markers '(".dir-locals.el"))
  (project-vc-include-untracked t)
  (project-vc-merge-submodules nil))

(with-eval-after-load 'orderless
  (keymap-substitute project-prefix-map #'project-find-regexp #'consult-ripgrep)
  (cl-nsubstitute-if
   '(consult-ripgrep "Find regexp")
   (pcase-lambda (`(,cmd _)) (eq cmd #'project-find-regexp))
   project-switch-commands)
    )
