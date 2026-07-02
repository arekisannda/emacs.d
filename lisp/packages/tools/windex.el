;;; tools/windex.el -*- lexical-binding: t; -*-

(require 'util-windows)

(use-package windex :after ace-window
  :custom
  (windex-window-filter-functions
   '((lambda (window)
       (or (member (window-parameter window 'window-side) '(left right bottom))
           (member (window-parameter window 'window-aux) '(aux))
           (window-parameter window 'window-popup)
           (window-minibuffer-p window)))))
  (windex-window-aw-filter-functions windex-window-filter-functions)
  :config
  (defmacro function-with-selector (frame-fn window-fn fn)
    (let ((fn-name (util/function-name fn)))
      `(defun ,(intern (concat fn-name "-with-selector-window")) (&rest args)
         ,(format "Call `%s' with ARGS on window returned by selector." fn-name)
         (interactive)
         (windex-with-selector ,frame-fn ,window-fn
           (apply (intern ,fn-name) args)))))

  (windex--enable-ace-window)
  (windex--enable-windmove-in-direction-split))

(use-package windex-purpose
  :custom
  (windex-purpose-alist
   '((edit-main      :activate nil :deactivate nil)
     (edit-general   :activate nil :deactivate nil)
     (view-info      :activate nil :deactivate nil)
     (view-reference :activate nil :deactivate nil)
     (view-log       :activate nil :deactivate nil))))

(use-package windex-layout
  :custom
  (windex-layout-restore-window-state-filter-function
   (lambda (window)
     (not (or (util/windows-side-window-p window)
              (util/windows-popup-window-p window)
              (util/windows-aux-window-p window)))
     ))
  (windex-layout-alist
   '((base :description "1x1 layout."
           :tree (:type buf))
     (col-2 :description "1x2 layout."
            :tree ( :type col
                    :nodes ((:type buf) (:type buf))))
     (col-3 :description "1x3 layout."
            :tree ( :type col
                    :nodes ((:type buf) (:type buf) (:type buf))))
     (row-2 :description "2x1 layout."
            :tree ( :type row
                    :nodes ((:type buf) (:type buf))))
     (col-2-left :description "2x1 layout with 1x2 left column."
                 :tree ( :type col
                         :nodes
                         ((:type row :nodes ((:type buf) (:type buf)))
                          (:type buf))))
     (col-2-right :description "2x1 layout with 1x2 right column."
                  :tree ( :type col
                          :nodes
                          ((:type buf)
                           (:type row :nodes ((:type buf)(:type buf))))))
     (tile :description "2x2 layout."
           :tree ( :type col
                   :nodes
                   (( :type row :nodes ((:type buf) (:type buf)))
                    ( :type row :nodes ((:type buf) (:type buf))))))
     )))

(use-package windex-scroll :after (evil)
  :custom
  (windex-scroll-frame-selector nil)
  (windex-scroll-window-selector
   (lambda () (util/windows-get-aux-window (selected-window))))

  (windex-scroll-left-function #'evil-scroll-column-left)
  (windex-scroll-right-function #'evil-scroll-column-right)
  (windex-scroll-up-function #'evil-scroll-line-up)
  (windex-scroll-down-function #'evil-scroll-line-down))

(use-package windex-posframe :after posframe
  :custom
  (windex-posframe-border-width 1)
  (windex-posframe-poshandler #'posframe-poshandler-frame-center)
  (windex-posframe-min-width (ceiling (* (frame-width) 0.8)))
  (windex-posframe-min-height (ceiling (* (frame-height) 0.6)))
  :custom-face
  (windex-posframe-border
   ((nil :inherit popup-border
         :background unspecified
         :foreground unspecified))))

(use-package windex-frame)
