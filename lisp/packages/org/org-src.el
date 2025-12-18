;;; org/org-src.el -*- lexical-binding: t; -*-

(use-package org-src :after org
  :custom
  (org-babel-default-header-args:go '((:wrap . "example")))
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (awk . t)
     (calc . t)
     (ditaa . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (go . t)
     (js . t)
     (kotlin . t)
     (latex . t)
     (mermaid . t)
     (octave . t)
     (plantuml . t)
     (python . t)
     (rust . t)
     (R . t)
     (shell . t)
     (sql . t)
     (sqlite . t)
     (typescript . t)))

  (util/update-alist
   'org-src-lang-modes
   '(("C"          . c-or-c++)
     ("C++"        . c-or-c++)
     ("cpp"        . c-or-c++)
     ("cmake"      . cmake)
     ("bash"       . sh)
     ("desktop"    . conf-desktop)
     ("dot"        . graphviz-dot)
     ("elisp"      . emacs-lisp)
     ("go"         . go)
     ("javascript" . javascript)
     ("kotlin"     . kotlin)
     ("mermaid"    . mermaid)
     ("ocaml"      . tuareg)
     ("python"     . python)
     ("rust"       . rust)
     ("screen"     . shell-script)
     ("shell"      . sh)
     ("sqlite"     . sql)
     ("toml"       . conf-toml)
     ("gnuplot"    . gnuplot)
     ("typescript" . typescript)))
  :config
  (defun +org-src-get-lang-mode (lang)
    (let ((mode (intern
                 (concat
                  (let ((l (or (cdr (assoc lang org-src-lang-modes)) lang)))
                    (if (symbolp l) (symbol-name l) l))
                  "-mode"))))
      mode))

  (advice-add #'org-src-get-lang-mode :override #'+org-src-get-lang-mode)
  :hook
  (org-babel-after-execute . org-link-preview-refresh))
