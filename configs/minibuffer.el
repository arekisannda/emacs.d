;;; minibuffer.el --- Emacs minibuffer packages and configurations -*- lexical-binding: t; origami-fold-style: triple-braces; -*-
;;; Commentary:

;;; Code:

(defun configs--sort-directories-first (files) ;; {{{
  "Sort FILES by first by directories."
  (setq files (vertico-sort-history-length-alpha files))
  (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
         (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))
;; }}}

(use-package vertico ;; {{{
  :ensure t
  :init
  (setq savehist-file "/tmp/emacs/savehist")
  (setq vertico-sort-override-function #'configs--sort-directories-first)
  :config
  (vertico-mode 1)
  (savehist-mode 1))
;; }}}
(use-package orderless ;; {{{
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))
;; }}}
(use-package marginalia ;; {{{
  :ensure t
  :config
  (marginalia-mode 1))
;; }}}
(use-package embark :ensure t)
(use-package consult ;; {{{
  :ensure t
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq consult-narrow-key "<"
        register-preview-delay 0.5
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))
;; }}}
(use-package embark-consult ;; {{{
  :after '(embark consult)
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))
;; }}}

(provide 'configs-minibuffer)
;;; minibuffer.el ends here
