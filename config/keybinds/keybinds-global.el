;;; keybinds-global.el --- Emacs G eneral Keybindings-*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'packages-init)
(require 'keybinds-custom)

(general-unbind
  "<escape>"
  "<escape> <escape>"
  "<escape> <escape> <escape>")

(general-create-definer keybinds/global)

(general-create-definer keybinds/global-help
  :keymaps 'help-map)

(general-create-definer keybinds/global-minibuffer-local
  :keymaps 'minibuffer-local-map)

(general-create-definer keybinds/global-veritco
  :keymaps 'vertico-map)

(general-create-definer keybinds/global-veritco-embark
  :keymaps 'packages/vertico-embark-prompter-map)

(general-create-definer keybinds/global-embark)

(keybinds/global
  "C-<left>"  '("focus left"               . evil-window-left)
  "C-<right>" '("focus right"              . evil-window-right)
  "C-<up>"    '("focus up"                 . evil-window-up)
  "C-<down>"  '("focus down"               . evil-window-down)

  "M-<prior>" `("owindow scroll up"        . ,(keybinds/custom--scroll-other-window -10))
  "M-<next>"  `("owindow scroll down"      . ,(keybinds/custom--scroll-other-window 10))

  "M-<left>"  `("scroll left"              . ,(keybinds/custom--scroll-column -5))
  "M-<right>" `("scroll right"             . ,(keybinds/custom--scroll-column 5))
  "M-<up>"    `("scroll up"                . ,(keybinds/custom--scroll-line -5))
  "M-<down>"  `("scroll down"              . ,(keybinds/custom--scroll-line 5))

  "M-["       '("prev tab"                 . tab-previous)
  "M-]"       '("next tab"                 . tab-next)
  "M-{"       '("prev popup"               . popper-cycle-backwards)
  "M-}"       '("next popup"               . popper-cycle)
  "M-\\"      '("ace-window"               . ace-window)
  "M-0"       '("focus treemacs"           . treemacs-select-window))

(keybinds/global-help
  "g"         '("Emacs Docs"               . keybinds/custom--emacs-doc))

(keybinds/global-minibuffer-local
  "<escape>"  '("abort"                    . abort-minibuffers))

(keybinds/global-minibuffer-local
  :prefix "C-,"
  "C-a"       '("cycle annotators"         . marginalia-cycle)
  "C-r"       '("history"                  . consult-history)
  "C-,"       '("embark-act"               . embark-act)
  "C-s"       '("embark-collect"           . embark-collect)
  "C-e"       '("embark-export"            . embark-export)
  "C-d"       '("insert dir"               . consult-dir)
  "C-D"       '("goto file"                . consult-dir-jump-file)
  "C-f"       '("insert file"              . consult-find))

(keybinds/global-veritco-embark
 "<escape>"   '("abort"                    . abort-minibuffers)
 "C-<tab>"    '("toggle prompter"          . abort-recursive-edit))

(keybinds/global-veritco
  "<escape>"  '("abort"                    . abort-minibuffers)
  "C-<tab>"   '("toggle prompter"          . packages/vertico-embark-act-with-completing-read))

(keybinds/global-embark embark-file-map
  "o"         '("embark-ace file"          . packages/vertico--embark-ace-find-file))

(keybinds/global-embark embark-buffer-map
  "o"         '("embark-ace buffer"        . packages/vertico--embark-ace-switch-to-buffer))

(keybinds/global-embark embark-bookmark-map
  "o"         '("embark-ace bookmark"      . packages/vertico--embark-ace-bookmark-jump))

(keybinds/global-embark embark-general-map
  "M-r"       '("project find and replace" . project-query-replace-regexp))

(provide 'keybinds-global)

;;; keybinds-global.el ends here
