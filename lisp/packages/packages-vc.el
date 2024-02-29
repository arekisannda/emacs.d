;;; packages-vc.el --- Emacs Magit/VC Package Configurations -*- lexical-binding: t; -*-
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
(use-package diff-hl
  :ensure (:type git :host github :repo "arekisannda/diff-hl" :branch "master")
  :custom
  (diff-hl-show-staged-changes nil)
  (diff-hl-side-margin-width 3)
  (diff-hl-flydiff-delay 0.1)
  :init
  (setq diff-hl-show-hunk-map (make-sparse-keymap)
        diff-hl-inline-popup-transient-mode-map (make-sparse-keymap))
  :hook
  (global-diff-hl-mode . diff-hl-flydiff-mode)
  (elpaca-after-init . global-diff-hl-mode))

(use-package emacs :after diff-hl :disabled
  :ensure nil
  :if (daemonp)
  :config
  (add-hook 'after-make-frame-functions
            (lambda (frame) (with-selected-frame frame
                              (unless (display-graphic-p)
                                (diff-hl-margin-mode 1))))))

(use-package magit :after diff-hl
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package forge :after magit :disabled)

(provide 'packages-vc)

;;; packages-vc.el ends here
