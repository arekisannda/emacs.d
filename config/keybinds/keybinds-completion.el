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
  :keymaps 'corfu-map)

(general-create-definer keybinds/completion-info
  :prefix-command 'keybinds/completion--info-command)

(keybinds/completion-active
  [remap next-line]                          #'corfu-next
  [remap previous-line]                      #'corfu-previous
  [remap keybinds/custom--scroll-other-down] #'corfu-popupinfo-scroll-down
  [remap keybinds/custom--scroll-other-up]   #'corfu-popupinfo-scroll-up
  [remap scroll-down-command]                #'corfu-scroll-down
  [remap scroll-up-command]                  #'corfu-scroll-up
  [remap beginning-of-buffer]                #'corfu-first
  [remap end-of-buffer]                      #'corfu-last
  [remap move-beginning-of-line]             #'corfu-prompt-beginning
  [remap move-end-of-line]                   #'corfu-prompt-end
  [remap evil-force-normal-state]            #'corfu-quit

  "<escape>" '("abort"            . corfu-quit)
  "C-g"      '("abort"            . corfu-quit)
  "SPC"      '("insert seperator" . corfu-insert-separator)
  "TAB"      '("complete"         . corfu-complete)
  "RET"      '("insert"           . corfu-insert)
  ">"        '("info"             . keybinds/completion--info-command)
  "<up>"     '("previous"         . corfu-previous)
  "<down>"   '("next"             . corfu-next))

(keybinds/completion-info
  ">" '("toggle info"             . corfu-popupinfo-toggle)
  "d" '("popup info doc"          . corfu-popupinfo-documentation)
  "D" '("info doc"                . corfu-info-documentation)
  "l" '("popup info loc"          . corfu-popupinfo-location)
  "L" '("info loc"                . corfu-info-location))

(keybinds/completion
  "C-w"      '("capf word"        . cape-dict)
  "C-y"      `("capf math"        . cape-math-symbols-unicode)
  "C-t"      '("capf snippet"     . yasnippet-capf)

  "C-,"      '("complete overlay" . corfu-candidate-overlay-complete-at-point)
  "C-."      '("capf"             . completion-at-point))

(keybinds/completion :major-modes 'TeX-mode
  "C-y"      `("capf math"        . cape-math-symbols-latex))

(provide 'keybinds-completion)

;;; keybinds-completion.el ends here
