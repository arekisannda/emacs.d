;;; packages-treesit.el --- Emacs Treesit Packages Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package emacs
  :ensure nil
  :config
  (require 'cl-lib)
  (require 'treesit)

  (setq treesit-language-source-alist
        '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash.git"))
          (c          . ("https://github.com/tree-sitter/tree-sitter-c.git"))
          (c-sharp    . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
          (cmake      . ("https://github.com/uyha/tree-sitter-cmake.git"))
          (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp.git" "v0.21.0"))
          (css        . ("https://github.com/tree-sitter/tree-sitter-css.git"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (go         . ("https://github.com/tree-sitter/tree-sitter-go.git" "v0.20.0"))
          (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git" "v1.0.2"))
          (html       . ("https://github.com/tree-sitter/tree-sitter-html.git"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript.git"))
          (json       . ("https://github.com/tree-sitter/tree-sitter-json.git"))
          (kotlin     . ("https://github.com/fwcd/tree-sitter-kotlin"))
          (latex      . ("https://github.com/latex-lsp/tree-sitter-latex.git" "v0.3.0" "src"))
          (lua        . ("https://github.com/Azganoth/tree-sitter-lua"))
          (make       . ("https://github.com/alemuller/tree-sitter-make"))
          (markdown   . ("https://github.com/ikatyang/tree-sitter-markdown"))
          (python     . ("https://github.com/tree-sitter/tree-sitter-python.git"))
          (rust       . ("https://github.com/tree-sitter/tree-sitter-rust.git"))
          (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "typescript/src"))
          (typst      . ("https://github.com/uben0/tree-sitter-typst"))
          (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml.git"))))

  (cl-loop for (lang) in treesit-language-source-alist do
           (unless (treesit-language-available-p lang)
             (treesit-install-language-grammar lang)
             (message "Treesit parser installed: %s" lang))))

(use-package treesit-fold
  :elpaca (treesit-fold :type git :host github :repo "abougouffa/treesit-fold"))

(provide 'packages-treesit)

;;; packages-treesit.el ends here
