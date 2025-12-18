;;; completion/cape.el -*- lexical-binding: t; -*-

(use-package cape
  :config
  (require 'cape-char)

  (util/add-capf-hooks
   #'cape-dabbrev
   #'cape-file
   #'cape-keyword)

  (plist-put cape--tex-properties :exit-function nil)

  (defcustom with-capf-extras-functions '()
    "Functions to replace in command `with-capf-extras'"
    :type '(repeat function)
    :group 'convenience)

  (defun with-capf-extras-command ()
    (interactive)
    (let ((completion-at-point-functions
           (list
            #'yasnippet-capf)))
      (completion-at-point)))
  :hook
  (eglot--managed-mode
   . (lambda ()
       (pcase major-mode
         ('org-mode (remove-hook 'completion-at-point-functions #'eglot-completion-at-point t)))
       )))
