;;; keybinds-session.el --- Emacs Session Keybindings -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'general)
(require 'keybinds-custom)
(require 'keybinds-code)

(general-define-key
 :states         '(normal visual emacs motion)
 :prefix-map     '+keybinds-session--map
 :prefix         "C-\\"
 :global-prefix  "C-\\")

(general-create-definer +keybinds-session
  :keymaps '+keybinds-session--map)

(general-create-definer +keybinds-session-term
  :prefix-command '+keybinds-session--term-command)

(general-create-definer +keybinds-session-tools
  :prefix-command '+keybinds-session--tools-command)

(general-create-definer +keybinds-session-workspace
  :prefix-command '+keybinds-session--workspace-command)

(general-create-definer +keybinds-session-buffer
  :prefix-command '+keybinds-session--buffer-command)

(general-create-definer +keybinds-session-debugger
  :prefix-command '+keybinds-session--debugger-command)

(general-create-definer +keybinds-session-tabs
  :prefix-command '+keybinds-session--tabs-command)

(general-create-definer +keybinds-session-mode
  :prefix-command '+keybinds-session--mode-command)

(+keybinds-session
  "C-\\" '("tools"               . +keybinds-session--tools-command)
  "b"    '("buffers"             . +keybinds-session--buffer-command)
  "d"    '("debugger"            . +keybinds-session--debugger-command)
  "m"    '("modes"               . +keybinds-session--mode-command)
  "w"    '("workspace"           . +keybinds-session--workspace-command)
  "t"    '("tabs"                . +keybinds-session--tabs-command)
  "v"    '("terminal"            . +keybinds-session--term-command)

  "C-q"  (general-predicate-dispatch nil
           (equal (+keybinds--current-workspace) "leetcode") '("leetcode" . +keybinds-code--leetcode-command)
           (equal (+keybinds--current-workspace) "exercism") '("exercism" . +keybinds-code--exercism-command)))

(+keybinds-session-term
  "v"    '("toggle project term" . multi-vterm-project)
  "V"    '("create term"         . +vterm-custom-multi-vterm)
  "r"    '("rename term"         . multi-vterm-rename-buffer))

(+keybinds-session-tools
  "p"    '("package try"         . elpaca-try)
  "P"    '("package manager"     . elpaca-manager)
  "f"    '("ranger buffer"       . ranger)
  "s"    '("scratch buffer"      . scratch-buffer)

  "m"    '("magit"               . magit)

  "d"    '("eval defun"          . eval-defun)
  "D"    '("edebug defun"        . edebug-defun)
  "e"    '("eval region"         . eval-region)
  "E"    '("eval buffer"         . eval-buffer))

(+keybinds-session-workspace
  "p"    '("switch project"      . project-switch-project)
  "d"    '("forget project"      . project-forget-project)
  "D"    '("forget project in"   . project-forget-projects-under)
  "K"    '("clear project"       . +keybinds--clear-project)
  "w"    '("switch perspective"  . persp-switch)
  "q"    '("kill perspective"    . persp-kill)
  "s"    '("save perspective"    . +perspective-save)
  "o"    '("opn perspective"     . persp-state-load)
  "r"    '("rename perspective"  . persp-rename)
  "b"    '("remove buffer"       . persp-remove-buffer)
  "l"    '("apply layout"        . consult-layouts))

(+keybinds-session-buffer
  "l"    '("list buffers"        . ibuffer)
  "k"    '("kill buffer"         . kill-this-buffer)
  "K"    '("kill buffer by name" . kill-buffer)
  "r"    '("revert buffer"       . revert-buffer))

(+keybinds-session-tabs
  "t"    '("create new tab"      . tab-bar-new-tab)
  "T"    '("create fresh tab"    . +keybinds--create-fresh-tab)
  "k"    '("close tab"           . tab-bar-close-tab)
  "o"    '("close other tab"     . tab-bar-close-other-tabs)
  "r"    '("rename tab"          . tab-bar-rename-tab)
  "u"    '("undo close tab"      . tab-bar-undo-close-tab))

(+keybinds-session-mode
  "!"    '("normal-mode"         . normal-mode)
  "@"    '("fundamental-mode"    . fundamental-mode)
  "I"    '("image-mode"          . image-mode)
  "r"    '("rainbow-mode"        . rainbow-mode))

(+keybinds-session-debugger
  "D"    '("start"               . dap-debug)
  "R"    '("restart"             . dap-debug-restart)
  "K"    '("clear breakpoints"   . dap-breakpoint-delete-all)
  "l"    '("list breakpoints"    . dap-ui-breakpoints-list)
  "d"    '("toggle breakpoint"   . dap-breakpoint-toggle)
  "m"    '("add message"         . dap-breakpoint-log-message)
  "c"    '("add condition"       . dap-breakpoint-condition)
  "h"    '("add hit condiition"  . dap-breakpoint-hit-condition)
  "i"    '("step-in"             . +hydra-dap-motion/dap-step-in)
  "o"    '("step-out"            . +hydra-dap-motion/dap-step-out)
  "n"    '("next"                . +hydra-dap-motion/dap-next)
  "C"    '("continue"            . +hydra-dap-motion/dap-continue)
  "S"    '("list sessions"       . +hydra-dap-session/dap-ui-sessions))

(provide 'keybinds-session)

;;; keybinds-session.el ends here
