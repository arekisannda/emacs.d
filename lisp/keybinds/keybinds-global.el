;;; keybinds-global.el --- Emacs G eneral Keybindings-*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'general)
(require 'disable-mouse)
(require 'dashboard)
(require 'keybinds-custom)

(general-unbind
  "<escape>"
  "<escape> <escape>"
  "<escape> <escape> <escape>"
  "C-<tab>"
  "C-S-<tab>"
  "C-S-<iso-lefttab>"
  "C-M-\\"
  "C-\\"
  "C-?")

(mapc #'disable-mouse-in-keymap
      (list dashboard-mode-map))

(general-create-definer +keybinds-global)

(+keybinds-global help-map
  "g"           '("Emacs Docs"          . +keybinds--emacs-doc))

(+keybinds-global override
  "C-<left>"    '("focus left"          . evil-window-left)
  "C-<right>"   '("focus right"         . evil-window-right)
  "C-<up>"      '("focus up"            . evil-window-up)
  "C-<down>"    '("focus down"          . evil-window-down)

  "M-["         '("prev tab"            . tab-previous)
  "M-]"         '("next tab"            . tab-next)
  "M-{"         '("prev popup"          . popper-cycle-backwards)
  "M-}"         '("next popup"          . popper-cycle)
  "M-\\"        '("ace-window"          . ace-window)
  "M-0"         '("focus treemacs"      . treemacs)

  "C-M-\\"      '("toggle input"        . toggle-input-method))

(+keybinds-global global
  "C-<tab>"         '("toggle popper"       . popper-toggle)
  "C-<iso-lefttab>" '("toggle popper type"  . popper-toggle-type)

  "S-<next>"    '("scroll left"         . +keybinds--scroll-left)
  "S-<prior>"   '("scroll right"        . +keybinds--scroll-right)
  "<prior>"     '("scroll up"           . +keybinds--scroll-up)
  "<next>"      '("scroll down"         . +keybinds--scroll-down)

  "M-<prior>"   '("owindow scroll up"   . +keybinds--scroll-other-up)
  "M-<next>"    '("owindow scroll down" . +keybinds--scroll-other-down))

(+keybinds-global vterm-mode-map
  "<escape>"    '("escape"              . vterm--self-insert)
  "C-S-<next>"  '("scroll left"         . +keybinds--scroll-left)
  "C-S-<prior>" '("scroll right"        . +keybinds--scroll-right)
  "C-<prior>"   '("scroll up"           . +keybinds--scroll-up)
  "C-<next>"    '("scroll down"         . +keybinds--scroll-down)

  "M-<prior>"   '("owindow scroll up"   . +keybinds--scroll-other-up)
  "M-<next>"    '("owindow scroll down" . +keybinds--scroll-other-down))

(+keybinds-global minibuffer-local-map
  "M-<prior>"   '("owindow scroll up"   . +keybinds--minibuffer-scroll-other-up)
  "M-<next>"    '("owindow scroll down" . +keybinds--minibuffer-scroll-other-down)
  "<escape>"    '("abort"               . abort-minibuffers))

(+keybinds-global minibuffer-local-map :prefix "C-,"
  "C-a"         '("cycle annotators"    . marginalia-cycle)
  "C-r"         '("history"             . consult-history)
  "C-,"         '("embark-act"          . embark-act)
  "C-s"         '("embark-collect"      . embark-collect)
  "C-e"         '("embark-export"       . embark-export)
  "C-d"         '("insert dir"          . consult-dir)
  "C-S-d"       '("goto file"           . consult-dir-jump-file)
  "C-f"         '("insert file"         . find-file))

(+keybinds-global +vertico-embark-prompter-map
  "<escape>"    '("abort"               . abort-minibuffers)
  "C-<tab>"     '("toggle prompter"     . abort-recursive-edit))

(+keybinds-global vertico-map
  "<escape>"    '("abort"               . abort-minibuffers)
  "C-<tab>"     '("toggle prompter"     . +vertico-embark-act-with-completing-read))

(+keybinds-global embark-file-map
  "o"           '("embark-ace file"     . +vertico-embark-ace-find-file))

(+keybinds-global embark-buffer-map
  "o"           '("embark-ace buffer"   . +vertico-embark-ace-switch-to-buffer))

(+keybinds-global embark-bookmark-map
  "o"           '("embark-ace bookmark" . +vertico-embark-ace-bookmark-jump))

(+keybinds-global google-translate-minibuffer-keymap
  "<tab>"       '("next translation"    . google-translate-next-translation-direction)
  "<backtab>"   '("prev translation"    . google-translate-previous-translation-direction))

(provide 'keybinds-global)

;;; keybinds-global.el ends here
