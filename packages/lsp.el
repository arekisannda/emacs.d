;;; lsp.el --- Emacs lsp/language package and configurations -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; tree-sitter language grammar
(setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash.git"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c.git"))
        (cmake . ("https://github.com/uyha/tree-sitter-cmake.git"))
        (c-sharp . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp.git"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css.git"))
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        (go . ("https://github.com/tree-sitter/tree-sitter-go.git"))
        (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html.git"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript.git"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json.git"))
        (latex . ("https://github.com/latex-lsp/tree-sitter-latex.git"))
        (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python.git"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust.git"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "typescript/src"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml.git"))))

(defun configs--install-treesit-grammar (language)
  "Helper method to install treesit grammar that are not installed."
  (unless (treesit-language-available-p language)
    (treesit-install-language-grammar language)))

(mapcar #'configs--install-treesit-grammar (mapcar #'car treesit-language-source-alist))

(dedup-add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(dedup-add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(dedup-add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(dedup-add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(dedup-add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(dedup-add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-ts-mode))
(dedup-add-to-list 'auto-mode-alist '("\\.dockerfile\\'" . dockerfile-ts-mode))
(dedup-add-to-list 'auto-mode-alist '("\\.jsonc\\'" . json-ts-mode))
(dedup-add-to-list 'auto-mode-alist '("\\.sway\\'" . i3wm-config-mode))

(setq major-mode-remap-alist
      '((c++-mode . c++-ts-mode)
        (c-mode . c-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)
        (csharp-mode . csharp-ts-mode)
        (css-mode . css-ts-mode)
        (javascript-mode . js-ts-mode)
        (python-mode . python-ts-mode)
        (sh-mode . bash-ts-mode)
        (js-json-mode . json-ts-mode)))

(defun configs--lsp-config ()
  "Config method for lsp general configurations."
  ;; feature disable
  (setq lsp-keymap-prefix nil
        lsp-enable-indentation nil
        lsp-enable-symbol-highlighting nil
        lsp-headerline-breadcrumb-enable nil
        lsp-ui-doc-show-with-mouse nil
        lsp-lens-enable nil
        lsp-ui-sideline-enable nil
        lsp-eldoc-enable-hover nil
        lsp-signature-auto-activate nil
        lsp-completion-show-detail nil
        lsp-completion-show-kind nil
        lsp-ui-doc-enable nil
        lsp-enable-on-type-formatting nil)
  ;; performance tuning
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq gc-cons-threshold (* 100 1024 1024))
  (setq lsp-idle-delay 0.1))

(defun configs--lsp-go-config ()
  "Config method for golang lsp configurations."
  (setq lsp-go-analyses '((shadow . t)
                          (simplifycompositelit . :json-false))))

;; (defun configs--lsp-python-config ()
;;   "Config method for python lsp configurations."
;;   (setq lsp-clients-pylsp-library-directories))

;; (defun configs--lsp-cpp-config ()
;;   "Config method for c++ lsp configurations"
;;   ())

(use-package lsp-mode
  :ensure t
  :init
  (configs--lsp-config)
  (configs--lsp-go-config)
  ;; (configs--lsp-python-config)

  :hook
  (go-ts-mode . lsp-deferred)
  (c++-ts-mode . lsp-deferred)
  (c-ts-mode . lsp-deferred)
  (c-or-c++-ts-mode . lsp-deferred)
  :commands (lsp lsp-mode lsp-deferred)
  :config
  (setq lsp-command-map (make-sparse-keymap))
  (add-hook 'before-save-hook #'lsp-format-buffer)
  (add-hook 'before-save-hook #'lsp-organize-imports))

(use-package dap-mode
  :ensure t
  :init
  (setq dap-print-io nil)
  (setq dap-auto-configure-features '())
  :config
  (require 'dap-lldb)
  (require 'dap-dlv-go))

(provide 'packages-lsp)

;;; lsp.el ends here
