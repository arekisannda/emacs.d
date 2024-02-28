;;; packages-vc.el --- Emacs Magit/vC Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-helpers)

;; Unload seq before continuing the elpaca build,
;; then continue to build the recipe E.
(elpaca `(seq :build ,(append (butlast (if (file-exists-p
                                            (expand-file-name "seq" elpaca-builds-directory))
                                           elpaca--pre-built-steps
                                         elpaca-build-steps))
                              (list #'(lambda (e)
                                        (util/unload-feature 'seq)
                                        (elpaca--continue-build e))
                                    #'elpaca--activate-package))))

(use-package magit
  :init
  (setq magit-auto-revert-mode nil))

(use-package forge :after magit :disabled)

(use-package diff-hl :after magit
  :ensure (:type git :host github :repo "arekisannda/diff-hl" :branch "master")
  :init
  (setq diff-hl-show-hunk-map (make-sparse-keymap))
  (setq diff-hl-show-staged-changes nil)
  (setq diff-hl-inline-popup-transient-mode-map (make-sparse-keymap))
  (setq diff-hl-side-margin-width 3)
  (setq diff-hl-flydiff-delay 0.1)
  :config
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)

  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (unless (display-graphic-p)
                      (diff-hl-margin-mode 1))))))

  (diff-hl-flydiff-mode 1)
  (global-diff-hl-mode nil))

(provide 'packages-vc)

;;; packages-vc.el ends here
