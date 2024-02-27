;;; ui-tooltip.el --- Emacs Tooltip Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'packages-init)

(defun ui/tooltip--enable-posframe ()
  "Use posframe."
  (add-hook 'flycheck-mode-hook 'flycheck-posframe-mode)
  (ace-window-posframe-mode 1)

  (require 'mozc-cand-posframe)
  (setq mozc-candidate-style 'posframe))

(defun ui/tooltip--enable-popup ()
  "Use popup."
  (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))

(defun ui/tooltip--get-popup-type ()
  "Retrieve popup dependency package."
  (if (display-graphic-p) 'posframe 'popup))

(defun ui/tooltip--load-deferred-configurations ()
  "Load deferred package configurations."
  (let ((popup-type (ui/tooltip--get-popup-type)))
    (cl-case popup-type
      (posframe (ui/tooltip--enable-posframe))
      (popup (ui/tooltip--enable-popup))
      (otherwise (message "Unable to configure popup")))))

(defun ui/tooltip-setup ()
  "Set up tooltip configurations."
  (global-flycheck-mode 1)

  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame) (with-selected-frame frame (ui/tooltip--load-deferred-configurations))))
    (ui/tooltip--load-deferred-configurations)))

(provide 'ui-tooltip)

;;; ui-tooltip.el ends here
