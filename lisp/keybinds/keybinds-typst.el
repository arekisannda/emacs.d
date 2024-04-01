;;; keybinds-typst.el --- Typst Keybindsings -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'general)

(general-create-definer +keybinds-typst-mode-exec
  :prefix-command '+keybinds-typst-mode--exec-command)

(+keybinds-typst-mode-exec
  "C-w" '("toggle watch" . typst-ts-mode-watch-toggle)
  "C-c" '("compile"      . typst-ts-mode-compile))

(provide 'keybinds-typst)

;;; keybinds-typst.el ends here
