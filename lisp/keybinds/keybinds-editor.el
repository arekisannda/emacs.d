;;; keybinds-editor.el --- Emacs Editor Keybindings -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'general)
(require 'util-folding)
(require 'keybinds-org-mode)

(general-define-key
 :states        '(normal insert visual emacs motion)
 :prefix-map    '+keybinds-editor--map
 :prefix        ","
 :global-prefix "C-,")

(general-create-definer +keybinds-editor
  :keymaps '+keybinds-editor--map)

(general-create-definer +keybinds-editor-exec
  :prefix-command '+keybinds-editor--exec-command)

(general-create-definer +keybinds-editor-text-edit
  :prefix-command '+keybinds-editor--text-edit-command)

(general-create-definer +keybinds-editor-motion
  :prefix-command '+keybinds-editor--motion-command)

(general-create-definer +keybinds-editor-cursor
  :prefix-command '+keybinds-editor--cursor-command)

(general-create-definer +keybinds-editor-lsp
  :prefix-command '+keybinds-editor--lsp-command)

(general-create-definer +keybinds-editor-syntax-check
  :prefix-command '+keybinds-editor--syntax-check-command)

(general-create-definer +keybinds-editor-fold
  :prefix-command '+keybinds-editor--fold-command)

(general-create-definer +keybinds-editor-input
  :prefix-command '+keybinds-editor--input-command)

(general-create-definer +keybinds-editor-translate
  :prefix-command '+keybinds-editor--translate-command)

(general-create-definer +keybinds-editor-git
  :prefix-command '+keybinds-editor--git-command)

(+keybinds-editor
  "C-c" '("cursor"               . +keybinds-editor--cursor-command)

  "!"   '("syntax check"         . +keybinds-editor--syntax-check-command)
  "g"   '("git"                  . +keybinds-editor--git-command)
  "i"   '("input method"         . +keybinds-editor--input-command)
  "m"   '("motion"               . +keybinds-editor--motion-command)
  "o"   '("fold"                 . +keybinds-editor--fold-command)
  "t"   '("translate"            . +keybinds-editor--translate-command)

  "C-," (general-predicate-dispatch nil
          (derived-mode-p 'prog-mode) '("lsp/doc"  . +keybinds-editor--lsp-command))

  ","   (general-predicate-dispatch nil
          (derived-mode-p 'org-mode)  '("org edit" . +keybinds-org-mode--edit-command)
          t                           '("edit"     . +keybinds-editor--text-edit-command))

  "C-." (general-predicate-dispatch nil
          (derived-mode-p 'org-mode)  '("org exec" . +keybinds-org-mode--exec-command)
          t                           '("exec"     . +keybinds-editor--exec-command)))

(+keybinds-editor-exec
  "C-," '("embark act"           . embark-act))

(+keybinds-editor-text-edit
  "!"   '("fix typo"             . ispell-word)
  "+"   '("sort lines"           . sort-lines)
  "="   '("indent"               . util/indent-buffer)

  "c"   '("comment line"         . evilnc-comment-or-uncomment-lines)
  "C"   '("comment block"        . evilnc-comment-or-uncomment-paragraphs)

  "s"   '("add surround"         . embrace-add)
  "S"   '("change surround"      . embrace-change)
  "d"   '("delete surround"      . embrace-delete)

  "k"   '("align"                . align)
  "l"   '("align left"           . evil-lion-left)
  "L"   '("align right"          . evil-lion-right)

  "r"   '("find/replace"         . query-replace-regexp)
  "R"   '("project find/replace" . project-query-replace-regexp))

(+keybinds-editor-cursor
  "C-q" '("clear cursors"        . evil-mc-undo-all-cursors)
  "C-a" '("add cursors beg"      . evil-mc-make-cursor-in-visual-selection-beg)
  "C-e" '("add cursors end"      . evil-mc-make-cursor-in-visual-selection-end))

(+keybinds-editor-motion
  "j"   '("to char below"        . evilem-motion-find-char)
  "J"   '("til char below"       . evilem-motion-find-char-to)
  "k"   '("to char above"        . evilem-motion-find-char-backward)
  "K"   '("til char above"       . evilem-motion-find-char-to-backward)

  "s"   '("snipe to next"        . evil-snipe-s)
  "S"   '("snipe to prev"        . evil-snipe-S)
  "x"   '("snipe til next"       . evil-snipe-x)
  "X"   '("snipe til prev"       . evil-snipe-X)

  "b"   '("line to bottom"       . evil-scroll-line-to-bottom)
  "m"   '("line to center"       . evil-scroll-line-to-center)
  "t"   '("line to top"          . evil-scroll-line-to-top))

(+keybinds-editor-fold
  "!"   '("hide fold level"      . hs-hide-level)

  "a"   '("inline beg comment"   . util/folding-add-fold-inline)
  "A"   '("block beg comment"    . util/folding-add-fold-surround)
  "e"   '("inline end comment"   . util/folding-add-fold-inline-end)
  "E"   '("block end comment"    . util/folding-add-fold-surround-end)

  "c"   '("hide fold"            . hs-hide-block)
  "C"   '("hide fold all"        . hs-hide-all)
  "X"   '("show fold all"        . hs-show-all)
  "x"   '("show fold"            . hs-show-block)
  "o"   '("toggle fold"          . hs-toggle-hiding))

(+keybinds-editor-lsp
  "d"   '("definition"           . xref-find-definitions)
  "D"   '("definition other"     . xref-find-definitions-other-window)
  "i"   '("describe"             . eldoc)
  "I"   '("implementation"       . eglot-find-implementation)
  "r"   '("references"           . xref-find-references)
  "R"   '("rename"               . eglot-rename)
  "t"   '("type definition"      . eglot-find-typeDefinition))

(+keybinds-editor-syntax-check
  "!"   '("check error"          . flycheck-display-error-at-point)
  "@"   '("check syntax"         . flycheck-buffer))

(+keybinds-editor-git
  "i"   '("inspect hunk"         . +hydra-git-hunk/diff-hl-show-hunk)
  "s"   '("stage hunk"           . diff-hl-stage-current-hunk)
  "d"   '("delete hunk"          . diff-hl-revert-hunk))

(+keybinds-editor-input
  "c"   '("insert char"          . insert-char)
  "t"   '("insert template"      . yas-insert-snippet)
  "n"   '("insert nerd-icon"     . nerd-icons-insert))

(+keybinds-editor-translate
  "t"   '("translate"         . google-translate-smooth-translate))

(provide 'keybinds-editor)

;;; keybinds-editor.el ends here
