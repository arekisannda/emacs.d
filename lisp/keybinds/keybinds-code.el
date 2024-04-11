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
  "o" '("set exercise"   . exercism-set-exercise)
  "t" '("test"           . exercism-run-tests)
  "s" '("submit"         . exercism-submit-solution)
  "p" '("publish"        . exercism-publish-solution)
  "u" '("unpublish"      . exercism-unpublish-solution)
  "c" '("mark completed" . exercism-mark-completed))

(provide 'keybinds-code)

;;; keybinds-code.el ends here
