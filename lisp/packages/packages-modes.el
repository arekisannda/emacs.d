;;; packages-modes.el --- Emacs Mode Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-lang)

(defun +lang-prog-mode-setup ()
  "Prog-mode setup."
  (setq-local truncate-lines t)
  (visual-line-mode -1)

  (setq-local origami-fold-style 'triple-braces)
  (origami-close-all-nodes (current-buffer)))

;;; prog-mode

;; prog-mode {{{

(use-package emacs
  :ensure nil
  :hook
  (prog-mode . +lang-prog-mode-setup)
  (prog-mode . diff-hl-mode)
  (prog-mode . display-line-numbers-mode)
  (prog-mode . rainbow-delimiters-mode)
  (prog-mode . origami-mode)
  (prog-mode . flyspell-prog-mode)
  (prog-mode . indent-bars-mode)
  (prog-mode . +lang-prog-mode-setup))

;; }}}

;; rust {{{

(use-package cargo)

(use-package rust-mode
  :mode
  ("\\.rs\\'" . rust-ts-mode)
  :hook
  (rust-ts-mode . util/lsp-ensure))

;; }}}

;; go {{{

(use-package go-mode
  :custom
  (go-ts-mode-indent-offset 4)
  :mode
  ("\\.go\\'" . go-ts-mode)
  :hook
  (go-ts-mode . util/lsp-ensure))

;; }}}

;; C/C++ {{{

(defun +lang-clang-mode-setup ()
  "C or C++ mode setup."
  (setq c-ts-mode-indent-style 'linux))

(use-package emacs
  :ensure nil
  :hook
  (c-ts-base-mode . util/lsp-ensure)
  (c-ts-base-mode . +lang-clang-mode-setup))

;; }}}

;; python {{{

(use-package emacs
  :ensure nil
  :custom
  (python-indent-offset 4)
  :hook
  (python-ts-mode . util/lsp-ensure))

;; }}}

;; csharp {{{

(use-package emacs
  :ensure nil
  :hook
  (csharp-ts-mode . util/lsp-ensure))

;; }}}

;; javascript/typescript {{{

(use-package typescript-mode
  :mode
  ("\\.tsx\\'" . tsx-ts-mode)
  ("\\.ts\\'" . typescript-ts-mode))

;; }}}

;; lua-mode {{{

(defun +lang-lua-mode-setup ()
  "Lua-mode setup."
  (setq-local lua-indent-level 2)
  (setq-local lua-indent-nested-block-content-align nil)
  (setq-local lua-indent-close-paren-align nil)

  (advice-add
   #'lua-calculate-indentation-block-modifier
   :around #'(lambda (old-function &rest arguments)
               (let ((old-res (apply old-function arguments)))
                 (if (> old-res lua-indent-level) lua-indent-level old-res)))))

(use-package lua-mode
  :hook
  (lua-mode . util/lsp-ensure)
  (lua-mode . +lang-lua-mode-setup))

;; }}}

;; kotlin {{{

(use-package kotlin-mode)

(use-package kotlin-ts-mode
  :mode
  ("\\.kt\\'" . kotlin-ts-mode))

;; }}}

;; typst {{{

(use-package typst-ts-mode
  :ensure (:type git :host sourcehut :repo "meow_king/typst-ts-mode" :files (:defaults "*.el"))
  :custom
  (typst-ts-mode-indent-offset 2)
  (typst-ts-mode-watch-options "--open")
  (typst-ts-mode-enable-raw-blocks-highlight t)
  (typst-ts-mode-highlight-raw-blocks-at-startup t)
  :hook
  (typst-ts-mode . diff-hl-mode)
  (typst-ts-mode . display-line-numbers-mode)
  (typst-ts-mode . rainbow-delimiters-mode)
  (typst-ts-mode . origami-mode)
  (typst-ts-mode . flyspell-prog-mode)
  (typst-ts-mode . indent-bars-mode)
  (typst-ts-mode . +lang-prog-mode-setup))

;; }}}

;; sh-mode {{{

(use-package emacs
  :ensure nil
  :custom
  (sh-basic-offset 2))

;;; }}}

;; emacs-lisp-mode {{{

(defun +lang-elisp-exec-on-save ()
  "Operations to be executed on buffer save."
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max)))

(defun +lang-elisp-mode-setup ()
  "Setup to run for `emacs-lisp-mode` modes."

  (add-hook 'before-save-hook #'+lang-elisp-exec-on-save nil 'local)
  (util/lang--add-to-capf-list (list #'cape-dabbrev
                                     #'cape-file
                                     #'cape-elisp-symbol
                                     #'cape-keyword)))

(use-package elisp-mode
  :ensure nil
  :hook
  (emacs-lisp-mode . hs-minor-mode)
  (emacs-lisp-mode . +lang-elisp-mode-setup))

;; }}}

(use-package plantuml-mode)

;;; conf-mode

(defun +lang-conf-mode-setup ()
  "Conf-mode setup."
  (setq-local truncate-lines t)
  (visual-line-mode -1))

;; conf-mode {{{

(use-package conf-mode
  :ensure nil
  :custom
  (json-ts-mode-indent-offset 2)
  :mode
  ("Dockerfile\\'" . dockerfile-ts-mode)
  ("\\.dockerfile\\'" . dockerfile-ts-mode)
  ("\\.jsonc\\'" . json-ts-mode)
  :hook
  (conf-mode . diff-hl-mode)
  (conf-mode . display-line-numbers-mode)
  (conf-mode . rainbow-delimiters-mode)
  (conf-mode . +lang-conf-mode-setup))

;; }}}

;; i3wm-config-mode {{{

(use-package i3wm-config-mode
  :mode
  ("\\.sway\\'" . i3wm-config-mode)
  ("\\.i3\\'" . i3wm-config-mode)
  :hook
  (i3wm-config-mode . display-line-numbers-mode)
  (i3wm-config-mode . rainbow-delimiters-mode))

;; }}}

;; yaml-ts-mode {{{

(use-package yaml-pro)

(use-package emacs
  :ensure nil
  :custom
  (yaml-indent-offset 2)
  :mode
  ("\\.ya?ml\\'" . yaml-ts-mode)
  :hook
  (yaml-ts-mode . diff-hl-mode)
  (yaml-ts-mode . display-line-numbers-mode)
  (yaml-ts-mode . rainbow-delimiters-mode)
  (yaml-ts-mode . yaml-pro-ts-mode)
  (yaml-ts-mode . +lang-conf-mode-setup))

;; }}}

;; special-mode

(defun +lang-special-mode-setup ()
  "Setup to run for `special-mode` major modes."
  (cond
   ((string-match-p "\\*eldoc.*\\*" (buffer-name))
    (visual-line-mode 1))))

(use-package emacs
  :ensure nil
  :hook
  (special-mode . +lang-special-mode-setup))

(use-package markdown-mode
  :config
  (util/update-alist
   'markdown-code-lang-modes
   '(
     ("go"         . go-mode)
     ("rust"       . rust-mode)
     ("diff"       . diff-mode)
     ("python"     . python-mode)
     ("javascript" . javascript-mode)
     ("typescript" . typescript-mode)
     ("kotlin"     . kotlin-mode)
     )))

(use-package emacs
  :ensure nil
  :config
  (util/update-alist
   'major-mode-remap-alist
   '(
     (c++-mode        . c++-ts-mode)
     (c-mode          . c-ts-mode)
     (c-or-c++-mode   . c-or-c++-ts-mode)
     (csharp-mode     . csharp-ts-mode)
     (css-mode        . css-ts-mode)
     (go-dot-mod-mode . go-mod-ts-mode)
     (go-mode         . go-ts-mode)
     (javascript-mode . js-ts-mode)
     (js-json-mode    . json-ts-mode)
     (kotlin-mode     . kotlin-ts-mode)
     (python-mode     . python-ts-mode)
     (sh-mode         . bash-ts-mode)
     (typescript-mode . typescript-ts-mode)
     )))

(provide 'packages-modes)

;;; packages-modes.el ends here
