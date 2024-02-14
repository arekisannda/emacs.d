;;; ui-mode-line.el --- Emacs Mode-Line Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'telephone-line-utils)
(require 'popper)

(telephone-line-defsegment* ui/mode-line--popper-tag-segment ()
  (if popper-popup-status "" nil))

(provide 'ui-mode-line)

;;; ui-mode-line.el ends here
