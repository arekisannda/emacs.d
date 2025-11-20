;;; util-helpers.el --- Miscellaneous Helper Functions -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'hideshow)
(require 'cl-lib)

(defun util/read-file-to-string (file)
  "Read entire content of FILE to string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

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

(defun util/with-minibuffer-keymap (keymap)
  "Create function with minibuffer KEYMAP."
  (lambda (fn &rest args)
    (minibuffer-with-setup-hook
        (lambda ()
          (use-local-map
           (make-composed-keymap keymap (current-local-map))))
      (apply fn args))))

(defun util/unload-feature (feature)
  "Unload FEATURE if present."
  (and (featurep feature) (unload-feature feature t)))

(defvar-local util/fold-type nil)
(defvar-local util/fold-show-all nil)
(defvar-local util/fold-hide nil)

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

(provide 'util-helpers)

;;; util-helpers.el ends here
