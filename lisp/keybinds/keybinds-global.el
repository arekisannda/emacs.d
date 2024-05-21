;;; keybinds-global.el --- Emacs General Keybindings-*- lexical-binding: t; -*-
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
  "M-\\"        '("ace-window"          . ace-window)
  "M-0"         '("focus treemacs"      . treemacs)

  "C-/"         '("toggle project term" . multi-vterm-project)

  "<Copy>"      '("copy"                . kill-ring-save)
  "<Cut>"       '("cut"                 . kill-region)
  "<Paste>"     '("paste"               . yank)

  "M-?"         '("popper toggle"       . popper-toggle)
  "M-<"         '("popper prev"         . popper-cycle-backwards)
  "M->"         '("popper next"         . popper-cycle))

(+keybinds-global global
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
  "<escape>"    '("abort"               . abort-minibuffers)
  "C-v"         '("mark"                . set-mark-command))

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

(+keybinds-global tabulated-list-mode-map
  :states        '(normal insert visual emacs motion)
  "q"           '("kill window"         . quit-window))

(provide 'keybinds-global)

;;; keybinds-global.el ends here
