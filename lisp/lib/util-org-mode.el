;;; util-org-mode.el --- Org-mode helper functions -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(eval-when-compile
  (defmacro util/org-remark-height-face (height)
    `(list t :height ,height))

  (defmacro util/org-remark-event-face (face height)
    `(list t :inherit ,face :height ,height))

  (defmacro util/org-remark-highlight-face (face)
    `(list t :inherit ,face :inverse-video t))

  (defmacro util/org-remark-color-face (face)
    `(list t :inherit ,face))
  )

(provide 'util-org-mode)

;;; util-org-mode.el ends here
