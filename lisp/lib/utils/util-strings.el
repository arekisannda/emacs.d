;;; util-strings.el --- String Utility Functions -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defun util/strings-from-file (file)
  "Read entire content of FILE to string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun util/strings-pad-string (str width &optional pad-char)
  "Pad STR with PAD-CHAR to the specified WIDTH."
  (format (format "%%-%ds" width) (or str "") (or pad-char ?\s)))

(defun util/strings-blank-or-nil-p (str)
  "Return true if STR is blank or nil."
  (or (not str) (string-blank-p str)))

(defun util/strings-remove-suffix (str suffix)
  "Remove SUFFIX from STR."
  (if (string-suffix-p suffix str)
      (substring str 0 (- (length str) (length suffix)))
    str))

(defun util/strings-add-font-lock (str face)
  "Add property FACE to STR."
  (prog1 str (add-face-text-property 0 (length str) face nil str)))

(defun util/function-name (fn)
  "Return FN function name."
  (cond
   ((symbolp fn) (symbol-name fn))
   ((and (consp fn) (eq (car fn) #'function)) (symbol-name(cadr fn)))
   ((stringp fn) fn)
   ))

(provide 'util-strings)

;;; util-strings.el ends here
