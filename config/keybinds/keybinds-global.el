;;; keybinds-global.el --- Emacs G eneral Keybindings-*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'packages-init)
(require 'keybinds-custom)

(general-unbind
  "<escape>"
  "<escape> <escape>"
  "<escape> <escape> <escape>"
  "C-?")

(general-create-definer keybinds/global)

(keybinds/global override
  "C-<left>"  '("focus left"          . evil-window-left)
  "C-<right>" '("focus right"         . evil-window-right)
  "C-<up>"    '("focus up"            . evil-window-up)
  "C-<down>"  '("focus down"          . evil-window-down)

  "M-<left>"  '("scroll left"         . keybinds/custom--scroll-left)
  "M-<right>" '("scroll right"        . keybinds/custom--scroll-right)
  "M-<up>"    '("scroll up"           . keybinds/custom--scroll-up)
  "M-<down>"  '("scroll down"         . keybinds/custom--scroll-down)

  "M-["       '("prev tab"            . tab-previous)
  "M-]"       '("next tab"            . tab-next)
  "M-{"       '("prev popup"          . popper-cycle-backwards)
  "M-}"       '("next popup"          . popper-cycle)
  "M-\\"      '("ace-window"          . ace-window)
  "M-0"       '("focus treemacs"      . treemacs-select-window))

(keybinds/global global
  "M-<prior>" '("owindow scroll up"   . keybinds/custom--scroll-other-up)
  "M-<next>"  '("owindow scroll down" . keybinds/custom--scroll-other-down))

(keybinds/global help-map
  "g"         '("Emacs Docs"          . keybinds/custom--emacs-doc))

(keybinds/global minibuffer-local-map
  "M-<prior>" '("owindow scroll up"   . keybinds/custom--minibuffer-scroll-other-up)
  "M-<next>"  '("owindow scroll down" . keybinds/custom--minibuffer-scroll-other-down)
  "<escape>"  '("abort"               . abort-minibuffers))

(keybinds/global minibuffer-local-map :prefix "C-,"
  "C-a"       '("cycle annotators"    . marginalia-cycle)
  "C-r"       '("history"             . consult-history)
  "C-,"       '("embark-act"          . embark-act)
  "C-s"       '("embark-collect"      . embark-collect)
  "C-e"       '("embark-export"       . embark-export)
  "C-d"       '("insert dir"          . consult-dir)
  "C-D"       '("goto file"           . consult-dir-jump-file)
  "C-f"       '("insert file"         . consult-find)
  "C-."       '("consult narrow"      . consult-narrow))

(keybinds/global packages/vertico-embark-prompter-map
  "<escape>"  '("abort"               . abort-minibuffers)
  "C-<tab>"   '("toggle prompter"     . abort-recursive-edit))

(keybinds/global vertico-map
  "<escape>"  '("abort"               . abort-minibuffers)
  "C-<tab>"   '("toggle prompter"     . packages/vertico-embark-act-with-completing-read))

(keybinds/global embark-file-map
  "o"         '("embark-ace file"     . packages/vertico--embark-ace-find-file))

(keybinds/global embark-buffer-map
  "o"         '("embark-ace buffer"   . packages/vertico--embark-ace-switch-to-buffer))

(keybinds/global embark-bookmark-map
  "o"         '("embark-ace bookmark" . packages/vertico--embark-ace-bookmark-jump))

(provide 'keybinds-global)

;;; keybinds-global.el ends here
