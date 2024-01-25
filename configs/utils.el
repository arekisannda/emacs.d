;;; utils.el --- Emacs utlity packages and configurations -*- lexical-binding: t; origami-fold-style: triple-braces; -*-
;;; Commentary:

;;; Code:

;;; general {{{
(use-package shut-up :ensure t)
(use-package no-littering :ensure t)
;;; }}}

;;; dashboard {{{
(use-package dashboard
  :ensure t
  :init
  (setq dashboard-banner-logo-title nil
        dashboard-startup-banner (expand-file-name "logo.png" configs--user-emacs-directory)
        dashboard-center-content t
        dashboard-show-shortcuts t
        dashboard-set-footer nil
        dashboard-display-icons-p t
        dashboard-icon-type 'nerd-icons
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-projects-backend 'project-el
        dashboard-items '((agenda . 8)  (bookmarks . 8) (projects . 16) (recents . 32))
        initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (dashboard-setup-startup-hook))
;;; }}}

;;; keybindings {{{
(use-package general)
(use-package hydra
  :init
  (setq hydra-key-doc-function nil))
;;; }}}

;;; navigation {{{
(use-package ranger
  :config
  (ranger-override-dired-mode t))

(use-package ace-window
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
;;; }}}

;;; version control {{{
(defun configs--elpaca-unload-seq (e)
  "Unload seq before continuing the elpaca build, then continue to build the recipe E."
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))
(elpaca `(seq :build ,(append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                                           elpaca--pre-built-steps
                                         elpaca-build-steps))
                              (list 'configs--elpaca-unload-seq 'elpaca--activate-package))))

(use-package magit)
(use-package forge
  :after magit)
;;; }}}

;;; diff tool {{{
(defun configs--set-vc-diff-hl-mode ()
  "Config method to set diff-hl mode on frame create."
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1)))

(use-package diff-hl
  :ensure t
  :elpaca(:type git :host github :repo "arekisannda/diff-hl")
  :init
  ;; clear diff-hl default keybinds
  (setq diff-hl-show-hunk-map (make-sparse-keymap))
  (setq diff-hl-inline-popup-transient-mode-map (make-sparse-keymap))
  (setq diff-hl-side-margin-width 3)
  (setq diff-hl-flydiff-delay 0.1)
  (setq diff-hl-show-hunk--current-footer
        "(q)Quit  (j)Next  (k)Previous  (s)Stage  (d)Revert  (c)Copy original")
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame) (with-selected-frame frame (configs--set-vc-diff-hl-mode)))))
  (diff-hl-flydiff-mode 1)
  (global-diff-hl-mode nil))

;; diff tool configurations
;; (defun ediff-setup-windows-custom (buffer-A buffer-B buffer-C control-buffer))
(setq ediff-setup-windows-function 'ediff-setup-windows-merge)
;;; }}}

;;; editor {{{
(use-package embrace
  :init
  (setq embrace-show-help-p t))

(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dirs '("~/.config/emacs-snippets"))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)
;;; }}}

(provide 'configs-utils)
;;; utils.el ends here
