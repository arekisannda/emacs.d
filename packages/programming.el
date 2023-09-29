;;; packages/programming --- summary:
;;; Emacs programming packages and configurations
;;; commentary:

;;; code:
;; syntax check
(use-package flycheck-pos-tip
  :ensure t)

(use-package flycheck-popup-tip
  :ensure t)

(defun config/load-flycheck-popup ()
  "load the proper flycheck popup util depending on the available graphics property"

  (if (display-graphic-p)
      (flycheck-pos-tip-mode 1)
    (flycheck-popup-tip-mode 1)))

(use-package flycheck
  :after (flycheck-pos-tip flycheck-popup-tip)
  :ensure t
  :init
  (setq flycheck-indication-mode nil
	    flycheck-mode-line nil)
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions
		        (lambda (frame) (with-selected-frame frame (config/load-flycheck-popup))))
    (config/load-flycheck-popup))
  (global-flycheck-mode 1))

;; completion
(use-package company
  :init
  (setq company-idle-delay nil
	    company-manual-begin t)
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-flx
  :after company
  :config
  (company-flx-mode +1))

(use-package company-quickhelp
  :init
  (setq company-quickhelp-delay 0.5
	    company-quickhelp-max-lines 20)
  :config
  (company-quickhelp-mode))

(provide 'packages-programming)
;;; programming.el ends here
