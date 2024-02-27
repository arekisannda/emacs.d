;;; tools-treesit.el --- Emacs Treesit Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'treesit)

(setq-default treesit-language-source-alist
              '((bash . ("https://github.com/tree-sitter/tree-sitter-bash.git"))
                (c . ("https://github.com/tree-sitter/tree-sitter-c.git"))
                (c-sharp . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
                (cmake . ("https://github.com/uyha/tree-sitter-cmake.git"))
                (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp.git"))
                (css . ("https://github.com/tree-sitter/tree-sitter-css.git"))
                (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
                (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
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
                (yaml . ("https://github.com/ikatyang/tree-sitter-yaml.git"))
                (make . ("https://github.com/alemuller/tree-sitter-make"))
                (markdown . ("https://github.com/ikatyang/tree-sitter-markdown"))
                (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))))

(cl-loop for (lang) in treesit-language-source-alist do
         (unless (treesit-language-available-p lang)
           (treesit-install-language-grammar lang)))

(provide 'tools-treesit)

;;; tools-treesit.el ends here
