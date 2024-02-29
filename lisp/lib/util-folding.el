;;; util-folding.el --- Code Folcing Utility Functdions -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-strings)

(defun util/folding--insert-fold-start (comment-str &optional info annotate)
  "Add start fold comment using COMMENT-STR.
If ANNOTATE is present, annotate comment with INFO."
  (let ((description (if (or (not annotate)
                             (util/strings-blank-or-nil-p info))
                         " "
                       (format " %s " info))))
    (insert-before-markers (concat comment-str description "{{{"))))

(defun util/folding--insert-fold-end (comment-str &optional info annotate)
  "Add end fold comment using COMMENT-STR.
If ANNOTATE is present, annotate comment with INFO."
  (let ((description (if (or (not annotate)
                             (util/strings-blank-or-nil-p info))
                         ""
                       (format " %s" info))))
    (insert-before-markers (concat comment-str " }}}" description ))))

(defun util/folding--add-fold-inline (rbegin rend comment-str &optional info &key annotate-end)
  "Add inline braces using COMMENT-STR to region between RBEGIN and REND.
Optional INFO description for the fold block."
  (unless rbegin (error "Invalid region begin"))
  (unless rend (error "Invalid region end"))
  (if (util/strings-blank-or-nil-p comment-str)
      (error "Invalid comment start"))

  (save-excursion
    (save-restriction
      (narrow-to-region rbegin rend)
      (goto-char (point-min))
      (end-of-line)
      (if (not (string-blank-p (thing-at-point 'line t)))
          (insert-before-markers (spaces-string 1)))
      (util/folding--insert-fold-start comment-str info (not annotate-end))
      (goto-char (point-max))
      (if (not (s-ends-with-p "\n" (thing-at-point 'line t)))
          (newline))
      (util/folding--insert-fold-end comment-str info annotate-end)
      (newline)
      (indent-according-to-mode))))

(defun util/folding--add-fold-surround (rbegin rend comment-str &optional info &key annotate-end)
  "Add surrounding braces using COMMENT-STR to region between RBEGIN and REND.
Optional INFO description for the fold block."
  (unless rbegin (error "Invalid region begin"))
  (unless rend (error "Invalid region end"))
  (if (util/strings-blank-or-nil-p comment-str)
      (user-error "Invalid comment start"))

  (save-excursion
    (save-restriction
      (narrow-to-region rbegin rend)
      (goto-char (point-min))
      (util/folding--insert-fold-start comment-str info (not annotate-end))
      (newline)
      (goto-char (point-max))
      (if (not (s-ends-with-p "\n" (thing-at-point 'line t)))
          (newline))
      (util/folding--insert-fold-end comment-str info annotate-end)
      (newline)
      (indent-according-to-mode))))

(defun util/folding--get-fold-comment-start ()
  "Helper method for retrieving `comment-start` characters."
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (concat comment-start comment-start))
   (t comment-start)))

(eval-when-compile
  (defmacro util/folding--add-fold (fn &key annotate-end)
    `(defun ,(intern (concat (replace-regexp-in-string "--" "-" (symbol-name fn))
                             (if annotate-end "-end")))
         (rbegin rend)
       "Add fold comments to region."
       (interactive "r")
       (if (use-region-p)
           (let ((comment-str (util/folding--get-fold-comment-start))
                 (info (read-string "Fold info: ")))
             (funcall ',fn rbegin rend comment-str info :annotate-end ,annotate-end))
         (message "No region."))))
  )

(util/folding--add-fold util/folding--add-fold-inline :annotate-end nil)
(util/folding--add-fold util/folding--add-fold-surround :annotate-end nil)
(util/folding--add-fold util/folding--add-fold-inline :annotate-end t)
(util/folding--add-fold util/folding--add-fold-surround :annotate-end t)

(provide 'util-folding)

;;; util-folding.el ends here
