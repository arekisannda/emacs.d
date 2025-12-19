;;; emacs/files.el -*- lexical-binding: t; -*-

(require 'util-files)

(use-package files
  :custom
  (util/files-read-only-rules
   (append util/files-read-only-rules
           '("/node_modules/"
             "/vendor/"
             "/_deps/"
             "^/nix/store/")))
  :hook
  (find-file   . util/files-set-read-only-by-rules)
  (before-save . util/files-create-directory-on-save))
