;;; keybinds-org-mode.el --- Org-mode/LaTeX Keybindings -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'general)

(general-create-definer keybinds/org-mode-edit
  :prefix-command 'keybinds/org-mode--edit-command)

(general-create-definer keybinds/org-mode-exec
  :prefix-command 'keybinds/org-mode--exec-command)

(general-create-definer keybinds/org-mode-table
  :prefix-command 'keybinds/org-mode--table-command)

(general-create-definer keybinds/org-mode-goto
  :prefix-command 'keybinds/org-mode--goto-command)

(keybinds/org-mode-edit
  "!"   '("fix typo"             . ispell-word)

  ","   '("toggle checkbox"    . org-toggle-checkbox)
  "*"   '("cycle bullet"       . org-cycle-list-bullet)
  "l"   '("toggle item"        . org-toggle-item)

  "<"   '("promote"            . outline-promote)
  ">"   '("demote"             . outline-demote)

  "e"   '("emphasis"           . org-emphasize)
  "g"   '("goto"               . keybinds/org-mode--goto-command)
  "t"   '("tables"             . keybinds/org-mode--table-command))

(keybinds/org-mode-goto
  "d"   '("open"               . org-open-at-point))

(keybinds/org-mode-table
  "l"   '("insert table hline" . org-table-insert-hline)
  "="   '("align table"        . org-table-align))

(keybinds/org-mode-exec
  "C-t" '("toggle modern mode" . org-modern-mode)
  "!"   '("org ctrl-c"         . org-ctrl-c-ctrl-c))

(provide 'keybinds-org-mode)

;;; keybinds-org-mode.el ends here
