;;; util-frames.el --- Emacs frame utility functions -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defmacro util/frames-make-frame-with-params (params &rest body)
  "Create new frames with PARAMS and run BODY."
  (declare (indent 1))
  `(let ((frame (make-frame ,params)))
     (select-frame-set-input-focus frame)
     ,@body))

(defmacro util/frames-select-frame-with-params (params &rest body)
  "Select frames with PARAMS or create it then run BODY."
  (declare (indent 1))
  `(let ((frame (cl-find-if
                 (lambda (f)
                   (seq-every-p
                    (lambda (p)
                      (or
                       (equal (frame-parameter f (car p)) (cdr p))
                       (eq (frame-parameter f (car p)) (cdr p))))
                    ,params))
                 (frame-list))))
     (if frame
         (progn
           (select-frame-set-input-focus frame)
           ,@body)
       (util/frames-make-frame-with-params ,params ,@body))))

(provide 'util-frames)

;;; util-frames.el ends here
