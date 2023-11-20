;;; init.el --- Emacs configuration tility methods -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defun dedup-add-to-list (list-var element &optional append compare-fn)
  "Helper method to add to a list without creating duplicates."
  (unless (member element (symbol-value list-var))
    (add-to-list list-var element append compare-fn)))

(provide 'config-utils)

;;; init.el ends here
