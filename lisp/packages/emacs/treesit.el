;;; emacs/treesit.el -*- lexical-binding: t; -*-

(use-package treesit
  :custom
  (treesit-font-lock-level 4)
  :config
  (setq treesit-language-source-alist
        '((bash            . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3"))
          (c               . ("https://github.com/tree-sitter/tree-sitter-c"))
          (c-sharp         . ("https://github.com/tree-sitter/tree-sitter-c-sharp"))
          (cmake           . ("https://github.com/uyha/tree-sitter-cmake"))
          (cpp             . ("https://github.com/tree-sitter/tree-sitter-cpp" "v0.21.0"))
          (css             . ("https://github.com/tree-sitter/tree-sitter-css"))
          (dockerfile      . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (elisp           . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (go              . ("https://github.com/tree-sitter/tree-sitter-go" "v0.25.4"))
          (gomod           . ("https://github.com/camdencheek/tree-sitter-go-mod" "v1.0.2"))
          (html            . ("https://github.com/tree-sitter/tree-sitter-html"))
          (javascript      . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json            . ("https://github.com/tree-sitter/tree-sitter-json"))
          (kotlin          . ("https://github.com/fwcd/tree-sitter-kotlin"))
          (latex           . ("https://github.com/latex-lsp/tree-sitter-latex" "v0.3.0" "src"))
          (lua             . ("https://github.com/tree-sitter-grammars/tree-sitter-lua" "v0.4.0"))
          (make            . ("https://github.com/alemuller/tree-sitter-make"))
          (markdown        . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.3.2" "tree-sitter-markdown/src"))
          (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.3.2" "tree-sitter-markdown-inline/src"))
          (nix             . ("https://github.com/nix-community/tree-sitter-nix"))
          (python          . ("https://github.com/tree-sitter/tree-sitter-python"))
          (rust            . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (toml            . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (tsx             . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
          (typescript      . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
          (typst           . ("https://github.com/uben0/tree-sitter-typst"))
          (yaml            . ("https://github.com/ikatyang/tree-sitter-yaml"))))

  (cl-loop for (lang) in treesit-language-source-alist do
           (unless (treesit-language-available-p lang)
             (treesit-install-language-grammar lang)
             (message "Treesit parser installed: %s" lang))))
