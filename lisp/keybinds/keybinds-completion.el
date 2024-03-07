;;; keybinds-completion.el --- Emacs Completion Keybindings -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'general)
(require 'keybinds-global)

(general-create-definer +keybinds-completion)

(general-create-definer +keybinds-completion-active
  :keymaps 'corfu-map)

(general-create-definer +keybinds-completion-info
  :prefix-command '+keybinds-completion--info-command)

(general-create-definer +keybinds-completion-extra
  :prefix-command '+keybinds-completion--extra-command)

(+keybinds-completion-extra
  "t"       '("capf snippet"      . yasnippet-capf)
  "w"       '("capf word"         . cape-dict)
  "y"       `("capf math"         . cape-math-symbols-unicode))

(+keybinds-global :states 'insert
  "C-SPC"   '("completion"        . completion-at-point)
  "C-S-SPC" '("complete overlay"  . corfu-candidate-overlay-complete-at-point)
  "M-SPC"   '("completion extra"  . +keybinds-completion--extra-command))

(+keybinds-global minibuffer-local-map
  "C-SPC"   '("completion"        . completion-at-point)
  "C-S-SPC" '("complete overlay"  . corfu-candidate-overlay-complete-at-point)
  "M-SPC"   '("completion extra"  . +keybinds-completion--extra-command))

(+keybinds-completion-active
  [remap next-line]                    #'corfu-next
  [remap previous-line]                #'corfu-previous
  [remap +keybinds--scroll-other-up]   #'corfu-popupinfo-scroll-down
  [remap +keybinds--scroll-other-down] #'corfu-popupinfo-scroll-up
  [remap +keybinds--scroll-up]         #'corfu-scroll-down
  [remap +keybinds--scroll-down]       #'corfu-scroll-up
  [remap beginning-of-buffer]          #'corfu-first
  [remap end-of-buffer]                #'corfu-last
  [remap move-beginning-of-line]       #'corfu-prompt-beginning
  [remap move-end-of-line]             #'corfu-prompt-end
  [remap evil-force-normal-state]      #'corfu-quit

  "<escape>" '("abort"            . corfu-quit)
  "C-g"      '("abort"            . corfu-quit)
  "SPC"      '("insert seperator" . corfu-insert-separator)
  "TAB"      '("complete"         . corfu-complete)
  "RET"      '("insert"           . corfu-insert)
  "<up>"     '("previous"         . corfu-previous)
  "<down>"   '("next"             . corfu-next)
  "M-?"      '("toggle info"      . corfu-popupinfo-toggle)
  "C-<tab>"  '("info command"     . +keybinds-completion--info-command))

(+keybinds-completion-info
  "d" '("popup info doc"          . corfu-popupinfo-documentation)
  "D" '("info doc"                . corfu-info-documentation)
  "l" '("popup info loc"          . corfu-popupinfo-location)
  "L" '("info loc"                . corfu-info-location))

(provide 'keybinds-completion)

;;; keybinds-completion.el ends here
