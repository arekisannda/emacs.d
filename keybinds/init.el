;;; init.el --- Emacs keybinding configurations -*- lexical-binding: t -*-
;;; Commentary:

;;; Emacs keybinds configurations using general.el and hydra.el

;;; Code:

(require 'packages-lang)

(general-define-key
 (kbd "<escape>")  #'keyboard-escape-quit
 (kbd "C-q")       #'keyboard-quit

 (kbd "C-<left>")  #'evil-window-left
 (kbd "C-<right>") #'evil-window-right
 (kbd "C-<up>")    #'evil-window-up
 (kbd "C-<down>")  #'evil-window-down

 (kbd "S-<left>") (lambda () (interactive)(evil-scroll-column-left 5))
 (kbd "S-<right>") (lambda () (interactive)(evil-scroll-column-right 5))
 (kbd "S-<up>") (lambda () (interactive)(evil-scroll-line-up 10))
 (kbd "S-<down>") (lambda () (interactive)(evil-scroll-line-down 10)))

;;;;
;; general keybinds
;;;;
(general-define-key
 "M-\\" #'popper-toggle
 "M-|"  #'popper-toggle-type

 "M-["  #'tab-bar-switch-to-prev-tab
 "M-]"  #'tab-bar-switch-to-next-tab)

;; vterm keybinds
(general-define-key
 :keymaps '(vterm-mode-map)
 "M-\\" #'popper-toggle
 "M-]"  #'tab-bar-switch-to-next-tab

 (kbd "C-<left>")  #'evil-window-left
 (kbd "C-<right>") #'evil-window-right
 (kbd "C-<up>")    #'evil-window-up
 (kbd "C-<down>")  #'evil-window-down

 (kbd "S-<left>") (lambda () (interactive)(evil-scroll-column-left 5))
 (kbd "S-<right>") (lambda () (interactive)(evil-scroll-column-right 5))
 (kbd "S-<up>") (lambda () (interactive)(evil-scroll-line-up 10))
 (kbd "S-<down>") (lambda () (interactive)(evil-scroll-line-down 10)))

(general-define-key
 :keymaps '(minibuffer-local-map)
 "M-A" #'marginalia-cycle
 "M-s" #'consult-history
 "M-r" #'consult-history)

(general-define-key
 :keymaps '(popper-mode-map)
 (kbd "M-<prior>")  #'popper-cycle-backwards
 (kbd "M-<next>")   #'popper-cycle
 (kbd "M-<delete>") #'kill-current-buffer)

;;;;
;; vim-mode keybinds
;;;;
;; global level
(general-create-definer kb/global-leader-key
  :states        '(normal insert visual emacs motion)
  :keymaps       'override
  :prefix        "\\"
  :global-prefix "C-\\")

;; local/buffer level
(general-create-definer kb/local-leader-key
  :states        '(normal visual emacs motion)
  :keymaps       'override
  :prefix        ","
  :global-prefix "C-,")

;; search level
(general-create-definer kb/search-leader-key
  :states        '(normal insert visual emacs motion)
  :keymaps       'override
  :prefix        ";"
  :global-prefix "C-;")

;; auto completion
(general-create-definer kb/completion-leader-key
  :states        '(insert)
  :keymaps       'override
  :prefix        "C-,"
  :global-prefix "C-,")

;; input method
(general-create-definer kb/im-leader-key
  :states        '(normal insert visual emacs motion)
  :keymaps       'override
  :prefix        "C-i"
  :global-prefix "C-i")

;; bind evil-args text objects
(general-define-key
 :keymaps '(evil-inner-text-objects-map)
 "a" #'evil-inner-arg)

(general-define-key
 :keymaps '(evil-outer-text-objects-map)
 "a" #'evil-outer-arg)

(general-define-key
 :keymaps '(evil-window-map)
 "q" (lambda () (interactive)(if (one-window-p)(tab-bar-close-tab)(delete-window)))
 "V" #'split-window-horizontally
 "v" (lambda () (interactive)(split-window-horizontally)(other-window 1))
 "S" #'split-window-vertically
 "s" (lambda () (interactive)(split-window-vertically)(other-window 1)))

(general-define-key
 :keymaps '(evil-normal-state-map)
 ;; bind evil-forward/backward-args
 "L"  #'evil-forward-arg
 "H"  #'evil-backward-arg
 "K"  #'evil-jump-out-args
 "u"  #'undo-fu-only-undo
 "U"  #'undo-fu-only-redo

 "]g" #'diff-hl-next-hunk
 "[g" #'diff-hl-previous-hunk)

(general-define-key
 :keymaps '(company-active-map)
 (kbd "<escape>")  #'company-abort)

(general-define-key
 :keymaps '(evil-motion-state-map)
 ;; bind evil-forward/backward-args
 "L" #'evil-forward-arg
 "H" #'evil-backwards-arg)

(defhydra hydra-dap-motion (:foreign-key exit :exit nil :timeout nil)
  "dap motion"
  ("i" #'dap-step-in "step-in")
  ("o" #'dap-step-out "step-out")
  ("n" #'dap-next "next")
  ("C" #'dap-continue "continue"))

(defhydra hydra-dap-session (:foreign-key exit :exit nil :timeout nil)
  "dap session"
  ("S" #'dap-ui-sessions "sessions")
  ("d" #'dap-ui-delete-session "delete session"))

(kb/global-leader-key
  "C-\\" #'multi-vterm-project
  "C-|"  #'multi-vterm

  ;; terminal keybinds
  "vr" '(multi-vterm-rename-buffer :wk "rename terminal")

  ;; utility/tool keybinds
  "p"  '(:ignore t :wk "tools")
  "pe" '(eval-region :wk "eval-region")
  "pE" '(eval-buffer :wk "eval-buffer")
  "pp" '(elpaca-try :wk "elpaca-try")
  "pP" '(elpaca-manager :wk "elpaca-manager")
  "pf" '(project-switch-project :wk "projects")
  "pF" '(ranger :wk "ranger")
  "ps" '(scratch-buffer :wk "scratch buffer")
  "pk" '((lambda () (interactive)(project-kill-buffers)(delete-other-windows)(dashboard-open)) :wk "kill project buffers")
  "pK" '((lambda () (interactive)(project-kill-buffers)(persp-kill)) :wk "kill project frame")

  ;; workspaces/perspective
  "w"  '(:ignore t :wk "workspace")
  "ww" '(persp-switch :wk "switch perspective")
  "wk" '(persp-kill :wk "kill perspective")
  "wr" '(persp-rename :wk "rename perspective")
  "wb" '(persp-remove-buffer :wk "remove buffer from perspective")

  ;; buffer management keybinds
  "b"  '(:ignore t :wk "buffer")
  "bl" '(list-buffers :wk "list buffers")
  "bb" '(consult-project-buffer :wk "switch buffer")
  "bB" '(consult-buffer :wk "switch buffer (all)")
  "bX" '(kill-buffer :wk "kill buffer")
  "bk" '(kill-this-buffer :wk "kill current buffer")
  "bK" '(project-kill-buffers :wk "kill current buffer")
  "bQ" '(kill-buffer-and-window :wk "kill buffer and window")
  "bn" '(next-buffer :wk "next buffer")
  "bp" '(previous-buffer :wk "previous buffer")
  "br" '(revert-buffer :wk "reload buffer")

  ;; dap debugger
  "d"  '(:ignore t :wk "debug")
  "dD" '(dap-debug :wk "start debug")
  "dR" '(dap-debug-restart :wk "restart debug")
  "dK" '(dap-breakpoint-delete-all :wk "delete breakpoints")
  "dl" '(dap-ui-breakpoints-list :wk "list breakpoint")
  "dd" '(dap-breakpoint-toggle :wk "toggle breakpoint")
  "dm" '(dap-breakpoint-log-message :wk "breakpoint message")
  "dc" '(dap-breakpoint-condition :wk "breakpoint condition")
  "dh" '(dap-breakpoint-hit-condition :wk "breakpoint hit condition")
  "di" '(hydra-dap-motion/dap-step-in :wk "step-in")
  "do" '(hydra-dap-motion/dap-step-out :wk "step-out")
  "dn" '(hydra-dap-motion/dap-next :wk "next")
  "dC" '(hydra-dap-motion/dap-continue :wk "continue")
  "dS" '(hydra-dap-session/dap-ui-sessions :wk "session")

  ;; quick window switcher keybinds
  "s"  '(:ignore t :wk "switcher")
  "ss" '(ace-window :wk "select")
  "sS" '(ace-swap-window :wk "swap")
  "sd" '(ace-delete-window :wk "delete")

  ;; tab management keybinds
  "t"  '(:ignore t :wk "tabs")
  "tt" '(tab-bar-new-tab :wk "new tab")
  "tT" '((lambda () (interactive)(tab-bar-new-tab)(dashboard-open)) :wk "new fresh tab")
  "tk" '(tab-bar-close-tab :wk "kill tab")
  "to" '(tab-bar-close-other-tabs :wk "kill other tabs")
  "tr" '(tab-bar-rename-tab :wk "rename tab")

  ;; mode management keybinds
  "m"  '(:ignore t :wk "modes")
  "mI" '(image-mode :wk "image mode")
  "mf" '(fundamental-mode :wk "fundamental mode")
  "mr" '(rainbow-mode :wk "rainbow mode"))

(setq diff-hl-show-hunk--current-footer
      "(q)Quit  (j)Next  (k)Previous  (s)Stage  (d)Revert  (c)Copy original")
(defhydra hydra-git-hunk (:foreign-key exit :exit nil :timeout nil :hint nil)
  "git"
  ("i" #'diff-hl-show-hunk :hint nil)
  ("j" #'diff-hl-show-hunk-next :hint nil)
  ("k" #'diff-hl-show-hunk-previous :hint nil)
  ("s" #'diff-hl-show-hunk-stage-hunk :hint nil)
  ("d" #'diff-hl-show-hunk-revert-hunk :hint nil)
  ("c" #'diff-hl-show-hunk-copy-original-text :hint nil))
(hydra-set-property 'hydra-git-hunk :verbosity 0)

(kb/local-leader-key
  ;; editor keybinds
  ","  '(:ignore t :wk "editor")
  ",=" '(sort-lines :wk "sort lines")
  ",k" '(align :wk "basic align")
  ",l" '(evil-lion-left :wk "align left")
  ",L" '(evil-lion-right :wk "align right")
  ",c" '(evilnc-comment-or-uncomment-lines :wk "comment-line")
  ",C" '(evilnc-comment-or-uncomment-paragraphs :wk "comment-paragraph")
  ",s" '(embrace-add :wk "surround add")
  ",S" '(embrace-change :wk "surround change")
  ",d" '(embrace-delete :wk "surround delete")
  ",r" '(lsp-rename :wk "rename symbol")
  ",t" '(ispell-word :wk "fix spelling typo")

  ;; lsp
  "d"  '(:ignore t :wk "lsp")
  "di" '(lsp-describe-thing-at-point :wk "find definition")
  "dI" '(lsp-find-implementation :wk "find implementation")
  "dt" '(lsp-find-type-definition :wk "find type definition")
  "dd" '(lsp-find-definition :wk "find definition")
  "dr" '(lsp-find-references :wk "find reference")

  ;; editor motion keybinds
  "m"  '(:ignore t :wk "motion")
  "mj" '(evilem-motion-find-char :wk "fowards motion")
  "mk" '(evilem-motion-find-char-backward :wk "backwards motion")
  "mJ" '(evilem-motion-find-char-to :wk "forwards motion to")
  "mK" '(evilem-motion-find-char-to-backward :wk "backwards motion to")
  "ms" '(evil-snipe-s :wk "forwards inclusive snipe")
  "mS" '(evil-snipe-S :wk "backwards inclusive snipe")
  "mx" '(evil-snipe-x :wk "forwards exclusive snipe")
  "mX" '(evil-snipe-X :wk "backwards exclusive snipe")

  ;; git keybinds
  "g"  '(:ignore t :wk "git")
  "gi" '(hydra-git-hunk/diff-hl-show-hunk :wk "inspect")
  "gj" '(diff-hl-next-hunk :wk "next hunk")
  "gk" '(diff-hl-previous-hunk :wk "prev hunk")
  "gs" '(diff-hl-stage-current-hunk :wk "stage")
  "gd" '(diff-hl-revert-hunk :wk "revert"))

(kb/search-leader-key
  ;; consult keybinds
  "g" '(consult-ripgrep :wk "find text in project")
  "r" '(consult-recent-file :wk "find recent file")
  "f" '(project-find-file :wk "find project file")
  "F" '(find-file :wk "find file")
  "c" '(consult-mode-command :wk "find recent commands")
  "h" '(consult-history :wk "find history")
  "k" '(consult-kmacro :wk "find keyboard macro")
  "m" '(consult-man :wk "find man page")
  "i" '(consult-info :wk "find emacs info"))

(kb/completion-leader-key
  "s" '(company-ispell :wk "suggest word")
  "." '(company-yasnippet :wk "suggest snippet")
  "," '(company-complete :wk "completion")
  "<" '(company-show-doc-buffer :wk "show code completion doc"))

(kb/im-leader-key
  ;; input methods
  "i"  '(:ignore t :wk "input method")
  "ii" '(configs--set-default-input-method :wk "English input ")
  "ij" '(configs--set-japanese-input-method :wk "Japanese input ")

  ;; translate
  "t"  '(:ignore t :wk "translate")
  "tt" '(google-translate-at-point :wk "translate source -> target")
  "tT" '(google-translate-at-point-reverse :wk "translate target -> source"))

(provide 'keybind-init)

;;; init.el ends here
