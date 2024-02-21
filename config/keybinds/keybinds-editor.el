;;; keybinds-editor.el --- Emacs Local Keybindings -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'packages-init)
(require 'util-folding)
(require 'tools-input-method)

(general-define-key
 :states        '(normal insert visual emacs motion)
 :prefix-map    'keybinds/editor--map
 :prefix        ","
 :global-prefix "C-,")

(general-create-definer keybinds/editor
  :keymaps 'keybinds/editor--map)

(general-create-definer keybinds/editor-stage-two
  :prefix-command 'keybinds/editor--stage-two-command)

(general-create-definer keybinds/editor-text
  :prefix-command 'keybinds/editor--text-command)

(general-create-definer keybinds/editor-motion
  :prefix-command 'keybinds/editor--motion-command)

(general-create-definer keybinds/editor-cursor
  :prefix-command 'keybinds/editor--cursor-command)

(general-create-definer keybinds/editor-lsp
  :prefix-command 'keybinds/editor--lsp-command)

(general-create-definer keybinds/editor-syntax-check
  :prefix-command 'keybinds/editor--syntax-check-command)

(general-create-definer keybinds/editor-fold
  :prefix-command 'keybinds/editor--fold-command)

(general-create-definer keybinds/editor-input
  :prefix-command 'keybinds/editor--input-command)

(general-create-definer keybinds/editor-translate
  :prefix-command 'keybinds/editor--translate-command)

(general-create-definer keybinds/editor-git
  :prefix-command 'keybinds/editor--git-command)

(keybinds/editor
  ","   '("text"                 . keybinds/editor--text-command)
  "m"   '("motion"               . keybinds/editor--motion-command)
  "o"   '("fold"                 . keybinds/editor--fold-command)
  "g"   '("git"                  . keybinds/editor--git-command)
  "i"   '("input method"         . keybinds/editor--input-command)
  "t"   '("translate"            . keybinds/editor--translate-command)
  "!"   '("syntax check"         . keybinds/editor--syntax-check-command)

  "C-," '("lsp/doc"              . keybinds/editor--lsp-command)
  "C-." '("stage two"            . keybinds/editor--stage-two-command)
  "C-c" '("cursor"               . keybinds/editor--cursor-command))

(keybinds/editor-stage-two
  "C-," '("embark act"           . embark-act))

(keybinds/editor-text
  "="   '("indent"               . util/indent-buffer)
  "+"   '("sort lines"           . sort-lines)
  "k"   '("align"                . align)
  "l"   '("align left"           . evil-lion-left)
  "L"   '("align right"          . evil-lion-right)
  "c"   '("comment line"         . evilnc-comment-or-uncomment-lines)
  "C"   '("comment block"        . evilnc-comment-or-uncomment-paragraphs)
  "s"   '("add surround"         . embrace-add)
  "S"   '("change surround"      . embrace-change)
  "d"   '("delete surround"      . embrace-delete)
  "r"   '("find/replace"         . query-replace-regexp)
  "R"   '("project find/replace" . project-query-replace-regexp)
  "!"   '("fix typo"             . ispell-word))

(keybinds/editor-cursor
  "C-q" '("clear cursors"        . evil-mc-undo-all-cursors)
  "C-a" '("add cursors beg"      . evil-mc-make-cursor-in-visual-selection-beg)
  "C-e" '("add cursors end"      . evil-mc-make-cursor-in-visual-selection-end))

(keybinds/editor-motion
  "j"   '("to char below"        . evilem-motion-find-char)
  "k"   '("to char above"        . evilem-motion-find-char-backward)
  "J"   '("til char below"       . evilem-motion-find-char-to)
  "K"   '("til char above"       . evilem-motion-find-char-to-backward)
  "s"   '("snipe to next"        . evil-snipe-s)
  "S"   '("snipe to prev"        . evil-snipe-S)
  "x"   '("snipe til next"       . evil-snipe-x)
  "X"   '("snipe til prev"       . evil-snipe-X)
  "t"   '("line to top"          . evil-scroll-line-to-top)
  "b"   '("line to bottom"       . evil-scroll-line-to-bottom)
  "m"   '("line to center"       . evil-scroll-line-to-center))

(keybinds/editor-fold
  "a"   '("inline beg comment"   . util/folding-add-fold-inline)
  "A"   '("block beg comment"    . util/folding-add-fold-surround)
  "e"   '("inline end comment"   . util/folding-add-fold-inline-end)
  "E"   '("block end comment"    . util/folding-add-fold-surround-end)
  "x"   '("show fold"            . hs-show-block)
  "X"   '("show fold all"        . hs-show-all)
  "c"   '("hide fold"            . hs-hide-block)
  "C"   '("hide fold all"        . hs-hide-all)
  "o"   '("toggle fold"          . hs-toggle-hiding)
  "!"   '("hide fold level"      . hs-hide-level))

(keybinds/editor-lsp
  "r"   '("lsp rename"           . lsp-rename)
  "i"   '("lsp describe"         . lsp-describe-thing-at-point)
  "I"   '("lsp implementation"   . lsp-find-implementation)
  "t"   '("lsp type definition"  . lsp-find-type-definition)
  "d"   '("lsp definition"       . lsp-find-definition)
  "r"   '("lsp references"       . lsp-find-references))

(keybinds/editor-syntax-check
  "!"   '("check error"          . flycheck-display-error-at-point)
  "@"   '("check syntax"         . flycheck-buffer))

(keybinds/editor-git
  "i"   '("inspect hunk"         . hydra-git-hunk/diff-hl-show-hunk)
  "s"   '("stage hunk"           . diff-hl-stage-current-hunk)
  "d"   '("delete hunk"          . diff-hl-revert-hunk))

(keybinds/editor-input
  "c"   '("insert char"          . insert-char)
  "n"   '("insert nerd-icon"     . nerd-icons-insert)
  "i"   '("english input"        . tools/input-method--set-english-input-method)
  "j"   '("japanese input"       . tools/input-method--set-japanese-input-method))

(keybinds/editor-translate
  "t"   '("translate to"         . google-translate-at-point)
  "T"   '("translate from"       . google-translate-at-point-reverse))

(provide 'keybinds-editor)

;;; keybinds-editor.el ends here
