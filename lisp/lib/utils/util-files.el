;;; util-files.el --- Emacs file utility functions -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'simple)
(require 'cl-lib)

(defcustom util/files-read-only-rules
  (list (format "^%s" (expand-file-name package-user-dir))
        "^/usr/share/emacs/")
  "List of read-only file prefixes."
  :group 'convenience
  :type '(list :element-type string))

(defun util/files-set-read-only ()
  (read-only-mode 1))

(defun util/files-set-read-only-by-rules ()
  "Enable `read-only-mode` if buffer matches one of `+emacs-read-only-rules`."
  (when (and buffer-file-name
             (cl-loop for rule in util/files-read-only-rules
                      thereis (string-match-p rule buffer-file-name)))
    (util/files-set-read-only)))

(defun util/files-create-directory-on-save ()
  (when buffer-file-name
    (let ((dir (file-name-directory buffer-file-name)))
      (when (and (not (file-exists-p dir))
                 (y-or-n-p (format "Directory %s does not exist; Create it?" dir)))
        (make-directory dir t)))))


(provide 'util-files)

;;; util-files.el ends here
