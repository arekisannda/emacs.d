;;; tools-vc.el --- Version Control Tools Configurations  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'packages-init)

(defun tools/vc--diff-hl-magit-setup ()
  "Set up for `diff-hl` and `magit` integration."
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(provide 'tools-vc)

;;; tools-vc.el ends here
