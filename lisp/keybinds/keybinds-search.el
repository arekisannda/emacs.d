;;; keybinds-search.el --- Emacs Search Keybindings -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'packages-init)

(general-define-key
 :states        '(normal insert visual emacs motion)
 :prefix-map    'keybinds/search--map
 :prefix        ";"
 :global-prefix "C-;")

(general-create-definer keybinds/search
  :keymaps 'keybinds/search--map)

(keybinds/search
  "g"   '("find ripgrep" . consult-ripgrep)
  "l"   '("find line"    . consult-line)
  "y"   '("find yank"    . consult-yank-replace)

  "d"   '("find dir"     . consult-dir)
  "r"   '("find recent"  . consult-recent-file)
  "f"   '("find file"    . affe-find)
  "F"   '("find dfile"   . find-file)
  "p"   '("find pfile"   . project-find-file)
  "b"   '("find buffer"  . consult-buffer)
  "B"   '("find pbuffer" . consult-project-buffer)

  "C-m" '("find man"     . consult-man)
  "C-k" '("find kmacro"  . consult-kmacro)
  "C-c" '("find command" . consult-mode-command)
  "C-h" '("find history" . consult-history)
  "C-i" '("find info"    . consult-info))

(keybinds/search :major-mode 'org-mode
  "h"   '("find heading" . consult-org-heading))

(provide 'keybinds-search)

;;; keybinds-search.el ends here
