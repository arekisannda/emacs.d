;;; keybinds-code.el --- Emacs Code Package Keybinds -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'general)

(general-create-definer +keybinds-code-exercism
  :prefix-command '+keybinds-code--exercism-command)

(general-create-definer +keybinds-code-leetcode
  :prefix-command '+keybinds-code--leetcode-command)

(+keybinds-code-leetcode
  "o" '("open"   . leetcode)
  "q" '("quit"   . leetcode-quit)
  "s" '("submit" . leetcode-submit)
  "t" '("try"    . leetcode-try))

(+keybinds-code-exercism
  "O" '("set track"      . exercism-set-track)
  "o" '("open exercise"  . exercism-open-exercise)
  "t" '("test"           . exercism-run-tests)
  "s" '("submit"         . exercism-submit)
  "P" '("publish"        . exercism-publish)
  "c" '("mark completed" . exercism-complete))

(provide 'keybinds-code)

;;; keybinds-code.el ends here
