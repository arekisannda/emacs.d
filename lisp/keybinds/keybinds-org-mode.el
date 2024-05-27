;;; keybinds-org-mode.el --- Org-mode/LaTeX Keybindings -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'general)

(general-create-definer +keybinds-org-mode-edit
  :prefix-command '+keybinds-org-mode--edit-command)

(general-create-definer +keybinds-org-mode-exec
  :prefix-command '+keybinds-org-mode--exec-command)

(general-create-definer +keybinds-org-mode-table
  :prefix-command '+keybinds-org-mode--table-command)

(general-create-definer +keybinds-org-mode-goto
  :prefix-command '+keybinds-org-mode--goto-command)

(general-create-definer +keybinds-org-mode-remark
  :prefix-command '+keybinds-org-mode--remark-command)

(general-create-definer +keybinds-org-mode-remark-highlight
  :prefix-command '+keybinds-org-mode--remark-highlight-command)

(general-create-definer +keybinds-org-mode-remark-size
  :prefix-command '+keybinds-org-mode--remark-size-command)

(general-create-definer +keybinds-org-mode-remark-color
  :prefix-command '+keybinds-org-mode--remark-color-command)

(general-create-definer +keybinds-org-mode
  :keymaps 'org-mode-map)

(general-unbind org-mode-map
  "C-c C-x C-t"
  "C-c C-x t t"
  "C-c C-x t c")

(+keybinds-org-mode
  "C-M-<return>" #'org-insert-subheading
  "C-M-S-<return>" #'org-insert-todo-subheading)

(+keybinds-org-mode-edit
  "!"   '("fix typo"           . ispell-word)

  ","   '("toggle checkbox"    . org-toggle-checkbox)
  "*"   '("cycle bullet"       . org-cycle-list-bullet)
  "l"   '("toggle item"        . org-toggle-item)

  "<"   '("promote"            . outline-promote)
  ">"   '("demote"             . outline-demote)

  "s"   '("add surround"       . embrace-add)
  "S"   '("change surround"    . embrace-change)
  "d"   '("delete surround"    . embrace-delete)

  "e"   '("emphasis"           . org-emphasize)
  "g"   '("goto"               . +keybinds-org-mode--goto-command)
  "t"   '("tables"             . +keybinds-org-mode--table-command)

  "m"   '("remark"             . +keybinds-org-mode--remark-command)

  "r"   '("replace"            . query-replace-regexp)
  "R"   '("project replace"    . project-query-replace-regexp))

(+keybinds-org-mode-goto
  "d"   '("open"               . org-open-at-point))

(+keybinds-org-mode-table
  "l"   '("insert table hline" . org-table-insert-hline)
  "="   '("align table"        . org-table-align))

(+keybinds-org-mode-exec
  "t"   '("typst toggle"       . org-typst-preview)
  "C-t" '("typst render"       . +org-typst-preview-render)

  "!"   '("toggle modern mode" . org-modern-mode))

(+keybinds-org-mode-remark
  "o"   '("view"               . +hydra-org-remark-motion/org-remark-view)

  "d"   '("delete"             . org-remark-delete)
  "h"   '("highlight"          . +keybinds-org-mode--remark-highlight-command)
  "s"   '("size"               . +keybinds-org-mode--remark-size-command)
  "c"   '("color"              . +keybinds-org-mode--remark-color-command)

  "!"   '("warn"               . org-remark-mark-warn)
  "@"   '("error"              . org-remark-mark-error)
  "["   '("prev remark"        . +hydra-org-remark-motion/org-remark-prev)
  "]"   '("next remark"        . +hydra-org-remark-motion/org-remark-next))

(+keybinds-org-mode-remark-size
  "1"   '("1.25"               . org-remark-mark-size-125)
  "2"   '("1.50"               . org-remark-mark-size-150)
  "3"   '("1.75"               . org-remark-mark-size-175)
  "4"   '("2.00"               . org-remark-mark-size-200)

  "6"   '("0.25"               . org-remark-mark-size-025)
  "7"   '("0.50"               . org-remark-mark-size-050)
  "8"   '("0.75"               . org-remark-mark-size-075))

(+keybinds-org-mode-remark-highlight
  "y"   '("yellow"             . org-remark-mark-hl-yellow)
  "o"   '("orange"             . org-remark-mark-hl-orange)
  "r"   '("red"                . org-remark-mark-hl-red)
  "m"   '("magenta"            . org-remark-mark-hl-magenta)
  "b"   '("blue"               . org-remark-mark-hl-blue)
  "g"   '("green"              . org-remark-mark-hl-green)
  "c"   '("cyan"               . org-remark-mark-hl-cyan)
  "v"   '("violet"             . org-remark-mark-hl-violet)
  "p"   '("purple"             . org-remark-mark-hl-purple)
  "g"   '("gray"               . org-remark-mark-hl-gray))

(+keybinds-org-mode-remark-color
  "y"   '("yellow"             . org-remark-mark-yellow)
  "o"   '("orange"             . org-remark-mark-orange)
  "r"   '("red"                . org-remark-mark-red)
  "m"   '("magenta"            . org-remark-mark-magenta)
  "b"   '("blue"               . org-remark-mark-blue)
  "g"   '("green"              . org-remark-mark-green)
  "c"   '("cyan"               . org-remark-mark-cyan)
  "v"   '("violet"             . org-remark-mark-violet)
  "p"   '("purple"             . org-remark-mark-purple)
  "g"   '("gray"               . org-remark-mark-gray))

(provide 'keybinds-org-mode)

;;; keybinds-org-mode.el ends here
