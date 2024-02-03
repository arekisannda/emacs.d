;;; keybinds-completion.el --- Emacs Completion Keybindings -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'packages-init)

(general-define-key
 :states        '(insert)
 :prefix-map    'keybinds/completion--map
 :prefix        "C-."
 :global-prefix "C-.")

(general-create-definer keybinds/completion
  :keymaps 'keybinds/completion--map)

(general-create-definer keybinds/completion-active
  :keymaps 'company-active-map)

(keybinds/completion-active
 "<escape>" '("abort"              . company-abort))

(keybinds/completion
  "C-f"     '("suggest spelling"   . ispell-word)
  "C-s"     '("suggest word"       . company-ispell)
  "C-,"     '("suggest snippet"    . company-yasnippet)
  "C-."     '("suggest completion" . company-complete)
  "C->"     '("completion doc"     . company-show-doc-buffer))

(provide 'keybinds-completion)

;;; keybinds-completion.el ends here
