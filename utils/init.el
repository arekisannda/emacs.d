;;; init.el --- Emacs configuration tility methods -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defun configs--dedup-add-to-list (list-var element &optional append compare-fn)
  "Helper method to add an ELEMENT to LIST-VAR without creating duplicates."
  (unless (member element (symbol-value list-var))
    (add-to-list list-var element append compare-fn)))

(defun configs--pad-string (str width &optional pad-char)
  "Pad STR with PAD-CHAR to the specified WIDTH."
  (format (format "%%-%ds" width) (or str "") (or pad-char ?\s)))

(defun configs--insert-fold-start (comment-str info)
  "Add start fold comment using COMMENT-STR and INFO."
  (let ((description (if (or (not info) (string-blank-p info))
                         " "
                       (concat " " info " "))))
    (insert-before-markers (concat comment-str description "{{{"))))

(defun configs--insert-fold-end(comment-str)
  "Add end fold comment using COMMENT-STR."
  (insert-before-markers (concat comment-str " }}}")))

(defun configs--add-fold-inline (rbegin rend comment-str &optional info)
  "Add inline braces using COMMENT-STR to region between RBEGIN and REND.
Optional INFO description for the fold block."
  (unless rbegin (error "Invalid region begin"))
  (unless rend (error "Invalid region end"))
  (if (or (not comment-str) (string-blank-p comment-str))
      (error "Invalid comment start"))

  (save-excursion
    (goto-char rbegin)
    (end-of-line)
    (if (not (string-blank-p (thing-at-point 'line t)))
        (insert-before-markers (spaces-string 1)))
    (configs--insert-fold-start comment-str info)
    (goto-char rend)
    (end-of-line)
    (newline)
    (configs--insert-fold-end comment-str)
    (indent-region rbegin rend)
    (goto-char rend)
    (forward-line 1)
    (indent-according-to-mode)))

(defun configs--add-fold-surround (rbegin rend comment-str &optional info)
  "Add surrounding braces using COMMENT-STR to region between RBEGIN and REND.
Optional INFO description for the fold block."
  (unless rbegin (error "Invalid region begin"))
  (unless rend (error "Invalid region end"))
  (if (or (not comment-str) (string-blank-p comment-str))
      (error "Invalid comment start"))

  (save-excursion
    (goto-char rbegin)
    (configs--insert-fold-start comment-str info)
    (newline)
    (goto-char rend)
    (end-of-line)
    (newline)
    (configs--insert-fold-end comment-str)
    (indent-region rbegin rend)
    (goto-char rend)
    (forward-line 1)
    (indent-according-to-mode)))

(defun configs--get-fold-comment-start ()
  "Helper method for retrieve."
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (concat comment-start comment-start))
   (t comment-start)))

(defun configs-add-fold-surround (begin end)
  "Add surround folds comments to region."
  (interactive "r")
  (if (use-region-p)
      (let ((comment-str (configs--get-fold-comment-start))
            (info (read-string "Fold info: ")))
        (configs--add-fold-surround begin end comment-str info))))

(defun configs-add-fold-inline (begin end)
  "Add inline folds comments to region from BEGIN to END."
  (interactive "r")
  (message "%d begin value" begin)
  (message "%d end value" end)
  (if (use-region-p)
      (let ((comment-str (configs--get-fold-comment-start))
            (info (read-string "Fold info: ")))
        (configs--add-fold-inline begin end comment-str info))))

(defun configs-indent-buffer ()
  "Indent BUFFER."
  (interactive)
  (save-excursion
    (origami-open-all-nodes (current-buffer))
    (mark-whole-buffer)
    (indent-region (region-beginning) (region-end))))

(provide 'config-utils)
;;; init.el ends here
