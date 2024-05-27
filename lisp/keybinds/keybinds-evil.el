;;; keybinds-evil.el --- Emacs Evil-mode Keybindings -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'general)
(require 'evil)
(require 'disable-mouse)
(require 'keybinds-custom)

(mapc #'disable-mouse-in-keymap
      (list evil-motion-state-map
            evil-normal-state-map
            evil-visual-state-map
            evil-insert-state-map))

(general-create-definer +keybinds-evil)

(general-create-definer +keybinds-evil-all-states
  :keymaps '(evil-normal-state-map
             evil-emacs-state-map
             evil-motion-state-map
             evil-visual-state-map))

(+keybinds-evil evil-normal-state-map
  "L"        '("next arg"        . evil-forward-arg)
  "H"        '("prev arg"        . evil-backward-arg)

  "K"        '("jump out arg"    . evil-jump-out-args)

  "u"        '("undo"            . undo-fu-only-undo)
  "U"        '("redo"            . undo-fu-only-redo)

  "[ g"      '("prev hunk"       . +hydra-git-motion/diff-hl-previous-hunk)
  "] g"      '("next hunk"       . +hydra-git-motion/diff-hl-next-hunk)

  "[ x"      '("prev xref"       . +hydra-xref-motion/xref-go-back)
  "] x"      '("prev xref"       . +hydra-xref-motion/xref-go-forward)

  "[ h"      '("prev help"       . +hydra-help-motion/help-go-back)
  "] h"      '("prev help"       . +hydra-help-motion/help-go-forward))

(+keybinds-evil evil-emacs-state-map
  "<escape>" (general-predicate-dispatch #'keyboard-quit
               (derived-mode-p 'vterm-mode) #'vterm--self-insert)
  "\\"       (general-predicate-dispatch #'evil-execute-in-normal-state
               (derived-mode-p 'vterm-mode) #'vterm--self-insert))

(+keybinds-evil evil-motion-state-map
  "<escape>" '("quit"            . keyboard-quit)

  "L"        '("next arg"        . evil-forward-arg)
  "H"        '("prev arg"        . evil-backward-arg)

  "[ x"      '("prev xref"       . +hydra-xref-motion/xref-go-back)
  "] x"      '("prev xref"       . +hydra-xref-motion/xref-go-forward)

  "[ h"      '("prev help"       . +hydra-help-motion/help-go-back)
  "] h"      '("prev help"       . +hydra-help-motion/help-go-forward))

(+keybinds-evil evil-inner-text-objects-map
  "a"        '("inner arg"       . evil-inner-arg))

(+keybinds-evil evil-outer-text-objects-map
  "a"        '("outer arg"       . evil-outer-arg))

(+keybinds-evil evil-window-map
  "="        '("balance windows" . balance-windows)
  "O"        '("clear windows"   . +keybinds--clear-windows)

  "u"        '("undo window"     . winner-undo)
  "U"        '("redo window"     . winner-redo)

  "q"        `("kill window"     . ,(+keybinds--one-window-tab-bar-close-tab delete-window))
  "Q"        `("kill bwindow "   . ,(+keybinds--one-window-tab-bar-close-tab kill-buffer-and-window))

  "V"        '("vsplit"          . split-window-horizontally)
  "v"        `("vsplit focus"    . ,(+keybinds--split-focus-other-window split-window-horizontally))
  "S"        '("ssplit"          . split-window-vertically)
  "s"        `("ssplit focus"    . ,(+keybinds--split-focus-other-window split-window-vertically))

  "d"        '("toggle buffer"   . +toggle-dedicated-window-buffer)
  "m"        '("window purpose"  . +window-set-purpose))

(+keybinds-evil-all-states
  "C-w"      '("evil-window"     . evil-window-map))

(provide 'keybinds-evil)

;;; keybinds-evil.el ends here
