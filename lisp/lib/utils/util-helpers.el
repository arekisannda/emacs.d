;;; util-helpers.el --- Miscellaneous Helper Functions -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl-lib)

(defmacro util/quiet (&rest body)
  `(let ((message-log-max nil))
     ,@body))

(defun util/dedup-add-to-list (list-var element &optional append compare-fn)
  "Add ELEMENT to LIST-VAR without creating duplicates."
  (unless (member element (symbol-value list-var))
    (add-to-list list-var element append compare-fn)))

(defun util/update-alist (orig-alist overwrite-alist)
  "Overwrite or create ORIG-ALIST with values from OVERWRITE-ALIST."
  (cl-loop for (element . value) in overwrite-alist do
           (setf (symbol-value orig-alist)
                 (cons (cons element value)
                       (assoc-delete-all element (symbol-value orig-alist))))))

(defun util/alist-contains-value (alist value)
  "Check if ALIST contain VALUE."
  (let ((found nil))
    (catch 'found
      (dolist (pair alist)
        (when (equal (cdr pair) value)
          (setq found t)
          (throw 'found nil))))
    found))

(defun util/unload-feature (feature)
  "Unload FEATURE if present."
  (and (featurep feature) (unload-feature feature t)))

(defvar-local util/fold-type nil)
(defvar-local util/fold-show-all nil)
(defvar-local util/fold-hide nil)
(defvar-local util/fold-show nil)

(defun util/get-fold-overlays ()
  "Retrieve fold overlays."
  (when (and (boundp 'util/fold-type) util/fold-type)
    (cl-remove-if-not
     (lambda (ov) (eq (overlay-get ov 'invisible) util/fold-type))
     (overlays-in (point-min) (point-max)))))

(defun util/indent-buffer ()
  "Indent BUFFER."
  (interactive)
  (save-excursion
    (let ((overlay-starts (mapcar #'overlay-start (util/get-fold-overlays))))
      (and (functionp util/fold-show-all) (funcall util/fold-show-all))
      (indent-region (point-min) (point-max))
      (when (functionp util/fold-hide)
        (dolist (starts overlay-starts)
          (goto-char starts)
          (funcall util/fold-hide)
          )))))

(defmacro util/custom-faces (&rest faces)
  "Customize FACES."
  `(mapc (lambda (spec) (apply #'face-spec-set spec))
         (backquote ,faces)))

(defun util/apply-rcs-patch-to-buffer (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        (line-offset 0)
        (column (current-column)))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (goto-char (point-min))
                (forward-line (1- (- from line-offset)))
                (setq line-offset (+ line-offset len))
                (let ((beg (point)))
                  (forward-line len)
                  (delete-region (point) beg))))
             (t
              (error "Invalid rcs patch")))))))
    (move-to-column column)))

(defun util/call-diff (diff-cmd outputfile patchbuf &optional start end)
  "Call diff command to generate patch between current buffer and OUTPUTFILE.
PATCHBUF is the buffer where the diff output will be written."
  (let ((start-point (or start (point-min)))
        (end-point (or end (point-max)))
        (local-copy (file-local-copy outputfile)))
    (unwind-protect
        (call-process-region
         start-point
         end-point
         diff-cmd nil patchbuf nil
         "-n" "--strip-trailing-cr" "-" (or local-copy outputfile))
      (when local-copy (delete-file local-copy)))
    ))

(defun util/region-is-whole-line-p (&optional start end)
  "Return t if region spans the entire line, nil if partial."
  (and (use-region-p)
       (save-excursion
         (goto-char (or start (region-beginning)))
         (bolp))
       (save-excursion
         (goto-char (or end (region-end)))
         (or (eolp) (bolp)))))

(provide 'util-helpers)

;;; util-helpers.el ends here
