;;; emacs/compile.el -*- lexical-binding: t; -*-

(use-package compile
  :defer t
  :custom
  (compilation-ask-about-save nil)
  :config
  (defun +colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point)))

  (defun compilation-read-command-override (command)
    (read-shell-command
     (concat (pcase this-command
               ((or 'project-compile
                    'project-recompile
                    ) "[project] ")
               ((or 'detached-project-compile
                    'detached-project-recompile
                    ) "[detach project] ")
               ((or 'detached-compile-recompile
                    'detached-compile
                    ) "[detach] ")
               (_ ""))
             "Compile command: ")
     command
     (if (equal (car compile-history) command)
         '(compile-history . 1)
       'compile-history)))

  (advice-add 'compilation-read-command :override #'compilation-read-command-override)
  :hook
  (compilation-filter . +colorize-compilation-buffer))
