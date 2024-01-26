;;; init.el --- Emacs configuration tility methods -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defun configs--dedup-add-to-list (list-var element &optional append compare-fn)
  "Helper method to add an ELEMENT to LIST-VAR without creating duplicates."
  (unless (member element (symbol-value list-var))
    (add-to-list list-var element append compare-fn)))

(defun configs--pad-string (str width &optional pad-char)
  "Pad STR with PAD-CHAR to the specified WIDTH"
  (format (format "%%-%ds" width) (or str "") (or pad-char ?\s)))

(provide 'config-utils)
;;; init.el ends here
