;;; keybinds-evil.el --- Emacs Evil-mode Keybindings -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'general)
(require 'keybinds-custom)

(general-create-definer keybinds/evil)

(general-create-definer keybinds/evil-all-states
  :keymaps '(evil-normal-state-map
             evil-emacs-state-map
             evil-motion-state-map
             evil-visual-state-map))

(keybinds/evil evil-normal-state-map
  "L"        '("next arg"        . evil-forward-arg)
  "H"        '("prev arg"        . evil-backward-arg)

  "K"        '("jump out arg"    . evil-jump-out-args)

  "u"        '("undo"            . undo-fu-only-undo)
  "U"        '("redo"            . undo-fu-only-redo)

  "[ g"      '("prev hunk"       . hydra-git-motion/diff-hl-previous-hunk)
  "] g"      '("next hunk"       . hydra-git-motion/diff-hl-next-hunk))

(keybinds/evil evil-emacs-state-map
  "<escape>" '("quit"            . keyboard-quit))

(keybinds/evil evil-motion-state-map
  "<escape>" '("quit"            . keyboard-quit)

  "L"        '("next arg"        . evil-forward-arg)
  "H"        '("prev arg"        . evil-backward-arg))

(keybinds/evil evil-inner-text-objects-map
  "a"        '("inner arg"       . evil-inner-arg))

(keybinds/evil evil-outer-text-objects-map
  "a"        '("outer arg"       . evil-outer-arg))

(keybinds/evil evil-window-map
  "="        '("balance windows" . balance-windows)
  "O"        '("clear windows"   . keybinds/custom--clear-windows)

  "u"        '("undo window"     . winner-undo)
  "U"        '("redo window"     . winner-redo)

  "q"        `("kill window"     . ,(keybinds/custom--one-window-tab-bar-close-tab delete-window))
  "Q"        `("kill bwindow "   . ,(keybinds/custom--one-window-tab-bar-close-tab kill-buffer-and-window))

  "V"        '("vsplit"          . split-window-horizontally)
  "v"        `("vsplit focus"    . ,(keybinds/custom--split-focus-other-window split-window-horizontally))
  "S"        '("ssplit"          . split-window-vertically)
  "s"        `("ssplit focus"    . ,(keybinds/custom--split-focus-other-window split-window-vertically)))

(keybinds/evil-all-states
  "C-w"      '("evil-window"     . evil-window-map))

(provide 'keybinds-evil)

;;; keybinds-evil.el ends here
