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

(general-create-definer +keybinds-org-mode
  :keymaps 'org-mode-map)

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

  "r"   '("find/replace"         . query-replace-regexp)
  "R"   '("project find/replace" . project-query-replace-regexp))

(+keybinds-org-mode-goto
  "d"   '("open"               . org-open-at-point))

(+keybinds-org-mode-table
  "l"   '("insert table hline" . org-table-insert-hline)
  "="   '("align table"        . org-table-align))

(+keybinds-org-mode-exec
  "C-t" '("toggle modern mode" . org-modern-mode)
  "!"   '("org ctrl-c"         . org-ctrl-c-ctrl-c))

(provide 'keybinds-org-mode)

;;; keybinds-org-mode.el ends here
