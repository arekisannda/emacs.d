;;; util-helpers.el --- Miscellaneous Helper Functions -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'hideshow)

(defun util/dedup-add-to-list (list-var element &optional append compare-fn)
  "Add ELEMENT to LIST-VAR without creating duplicates."
  (unless (member element (symbol-value list-var))
    (add-to-list list-var element append compare-fn)))

(defun util/update-alist (alist element value)
  "Overwrite or create ELEMENT with VALUE in ALIST."
  (setq alist (assoc-delete-all element alist))
  (setq alist (cons (cons element value) alist)))

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

(defun util/indent-buffer ()
  "Indent BUFFER."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(eval-when-compile
  (defmacro util/if-daemon-run-after-make-frame-else-add-hook (fn alt-hook)
    `(if (daemonp)
         (add-hook 'after-make-frame-functions
                   (lambda (frame) (with-selected-frame frame ,fn)))
       (add-hook ,alt-hook (lambda () ,fn))))

  (defmacro util/if-daemon-run-after-make-frame-else-run (fn)
    `(if (daemonp)
         (add-hook 'after-make-frame-functions
                   (lambda (frame) (with-selected-frame frame ,fn)))
       ,fn))
  )

(provide 'util-helpers)

;;; util-helpers.el ends here
