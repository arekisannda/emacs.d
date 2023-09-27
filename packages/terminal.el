;;; packages/terminal --- summary:
;;; Emacs terminal packages and configurations
;;; commentary:

;;; code:

(defvar disable-vterm-mode-map (make-sparse-keymap)
  "Empty keymap to disable vterm-mode keybindings.")

(define-minor-mode custom-vterm-mode
  "Custom vterm mode with no keybindings."
  :init-value nil
  :lighter "Term"
  :keymap disable-vterm-mode-map)

(use-package vterm
  :config
  (add-hook 'vterm-mode-hook #'custom-vterm-mode))

(use-package multi-vterm
  :after vterm)

;; (use-package eterm-256color
;;   :hook (vterm-mode . eterm-256color-mode))

;; (use-package vterm-toggle
;;   :after vterm
;;   :config
;;   (setq vterm-toggle-fullscreen-p nil)
;;   (add-to-list 'display-buffer-alist
;; 	       '((lambda (buffer-or-name _)
;; 		   (let ((buffer (get-buffer buffer-or-name)))
;; 		     (with-current-buffer buffer
;; 		       (or (equal major-mode 'vterm-mode)
;; 			   (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
;; 		 (display-buffer-reuse-window display-buffer-at-bottom)
;; 		 ;;(display-buffer-reuse-window display-buffer-in-direction)
;; 		 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
;; 		 ;;(direction . bottom)
;; 		 ;;(dedicated . t) ;dedicated is supported in emacs27
;; 		 (reusable-frames . visible)
;; 		 (window-height . 0.3))))

(provide 'packages-terminal)
;;; terminal.el ends here
