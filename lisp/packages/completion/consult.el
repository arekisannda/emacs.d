;;; completion/consult.el -*- lexical-binding: t; -*-

(use-package consult
  :custom
  (consult-preview-key nil)
  (consult-narrow-key "<")
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  ;; Disable preview for consult-grep commands
  (consult-customize
   consult-ripgrep
   consult-git-grep
   consult-buffer
   consult-project-buffer
   consult-grep
   consult-recent-file
   :preview-key nil)

  (defun +consult--source-recentf-items ()
    (let ((ht (consult--buffer-file-hash))
          file-name-handler-alist ;; No Tramp slowdown please.
          items)
      (dolist (file recentf-list (nreverse items))
        ;; Emacs 29 abbreviates file paths by default, see
        ;; `recentf-filename-handlers'.
        (unless (eq (aref file 0) ?/)
          (setq file (expand-file-name file)))
        (unless (gethash file ht)
          (push (propertize
                 (file-name-nondirectory file)
                 'multi-category `(file . ,file))
                items)))))

  (plist-put consult--source-recent-file
             :items #'+consult--source-recentf-items)
  :hook
  (completion-list-mode . consult-preview-at-point-mode))

(use-package consult-dir :after consult)
