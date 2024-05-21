;;; keybinds-search.el --- Emacs Search Keybindings -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'general)
(require 'util-helpers)

(general-define-key
 :states         '(normal insert visual emacs motion)
 :prefix-map     '+keybinds-search--map
 :prefix         ";"
 :global-prefix  "C-;")

(general-create-definer +keybinds-search
  :keymaps '+keybinds-search--map)

(+keybinds-search
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
  "C-i" '("find info"    . consult-info)

  "C-r" '("find reg"     . consult-register)
  "C-b" '("find marks"   . consult-bookmark)

  "h"   (general-predicate-dispatch nil
          (derived-mode-p 'prog-mode) '("imenu"        . consult-imenu)
          (derived-mode-p 'org-mode)  '("find heading" . consult-org-heading)
          t                           '("find line"    . consult-line))

  "s"   (general-predicate-dispatch nil
          (eglot-managed-p)           '("symbols"      . consult-eglot-symbols)))

(provide 'keybinds-search)

;;; keybinds-search.el ends here
