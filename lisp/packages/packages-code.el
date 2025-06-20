;;; packages-code.el --- Coding Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'util-folding)

(use-package treesit :ensure nil
  :custom
  (treesit-font-lock-level 4)
  :config
  (setq treesit-language-source-alist
        '((bash            . ("https://github.com/tree-sitter/tree-sitter-bash.git" "v0.23.3"))
          (c               . ("https://github.com/tree-sitter/tree-sitter-c.git"))
          (c-sharp         . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
          (cmake           . ("https://github.com/uyha/tree-sitter-cmake.git"))
          (cpp             . ("https://github.com/tree-sitter/tree-sitter-cpp.git" "v0.21.0"))
          (css             . ("https://github.com/tree-sitter/tree-sitter-css.git"))
          (dockerfile      . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (elisp           . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (go              . ("https://github.com/tree-sitter/tree-sitter-go.git" "v0.23.4"))
          (gomod           . ("https://github.com/camdencheek/tree-sitter-go-mod.git" "v1.0.2"))
          (html            . ("https://github.com/tree-sitter/tree-sitter-html.git"))
          (javascript      . ("https://github.com/tree-sitter/tree-sitter-javascript.git"))
          (json            . ("https://github.com/tree-sitter/tree-sitter-json.git"))
          (kotlin          . ("https://github.com/fwcd/tree-sitter-kotlin"))
          (latex           . ("https://github.com/latex-lsp/tree-sitter-latex.git" "v0.3.0" "src"))
          (lua             . ("https://github.com/Azganoth/tree-sitter-lua"))
          (make            . ("https://github.com/alemuller/tree-sitter-make"))
          (markdown        . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.3.2" "tree-sitter-markdown/src"))
          (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.3.2" "tree-sitter-markdown-inline/src"))
          (nix             . ("https://github.com/nix-community/tree-sitter-nix" "v0.0.2"))
          (python          . ("https://github.com/tree-sitter/tree-sitter-python.git"))
          (rust            . ("https://github.com/tree-sitter/tree-sitter-rust.git"))
          (toml            . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (tsx             . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "tsx/src"))
          (typescript      . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "typescript/src"))
          (typst           . ("https://github.com/uben0/tree-sitter-typst"))
          (yaml            . ("https://github.com/ikatyang/tree-sitter-yaml.git"))))

  (cl-loop for (lang) in treesit-language-source-alist do
           (unless (treesit-language-available-p lang)
             (treesit-install-language-grammar lang)
             (message "Treesit parser installed: %s" lang))))

(use-package leetcode
  :custom
  (leetcode-prefer-language "golang")
  (leetcode-prefer-sql "mysql")
  (leetcode-save-solutions t)
  :config
  (defun +leetcode--solving-window-layout-override ()
    (delete-other-windows)
    (setq leetcode--description-window (selected-window))
    (setq leetcode--code-window (split-root-window-right))
    (setq leetcode--testcase-window (split-window-below))
    (other-window 1)
    (setq leetcode--result-window (split-window-below))
    (select-window leetcode--code-window))

  (defun +leetcode--display-result-override (buffer &optional alist)
    (set-window-buffer leetcode--result-window buffer)
    leetcode--result-window)

  (defun +leetcode--display-testcase-override (buffer &optional alist)
    (set-window-buffer leetcode--testcase-window buffer)
    leetcode--testcase-window)

  (defun +leetcode--display-detail-override (buffer &optional alist)
    (set-window-buffer leetcode--description-window buffer)
    leetcode--description-window)

  (defun +leetcode--display-code-override (buffer &optional alist)
    (set-window-buffer leetcode--code-window buffer)
    leetcode--code-window)

  (advice-add #'leetcode--solving-window-layout :override #'+leetcode--solving-window-layout-override)
  (advice-add #'leetcode--display-result :override #'+leetcode--display-result-override)
  (advice-add #'leetcode--display-testcase :override #'+leetcode--display-testcase-override)
  (advice-add #'leetcode--display-detail :override #'+leetcode--display-detail-override)
  (advice-add #'leetcode--display-code :override #'+leetcode--display-code-override)
  :hook
  (leetcode-solution-mode . (lambda () (eglot--managed-mode -1))))

(use-package exercism
  :ensure (exercism :type git :host github :repo "arekisannda/exercism.el")
  :custom
  (exercism-enable-log-to-message-buffer nil)
  (exercism-open-url-on-submit nil)
  :hook
  (elpaca-after-init . exercism-setup))

(use-package rfc-mode)

(use-package treesit-fold
  :ensure (treesit-fold :type git :host github :repo "abougouffa/treesit-fold"))

(use-package hideshow
  :ensure nil
  :preface
  (setq +fold-replacement "  ")
  (defun +hs-mode-fold-overlay (ov)
    "Format fold overlay OV."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (propertize +fold-replacement
                               'face
                               `((nil :foreground ,(doom-color 'grey)
                                      :box nil
                                      :weight bold))))))
  (defvar +hs-mode-overlay-fold-function #'+hs-mode-fold-overlay)
  :custom
  (treesit-fold-replacement +fold-replacement)
  (hs-set-up-overlay +hs-mode-overlay-fold-function)
  :custom-face
  (treesit-fold-replacement-face
   ((nil :foreground ,(doom-color 'grey)
         :box nil
         :weight bold))))

(use-package origami
  :config
  (setq origami-fold-style 'triple-braces))

(use-package prettier)

(provide 'packages-code)

;;; packages-code.el ends here
