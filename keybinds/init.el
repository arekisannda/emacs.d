;;; init.el --- Emacs keybinding configurations -*- lexical-binding: t; origami-fold-style: triple-braces; -*-
;;; Commentary:
;;; Emacs keybinds configurations using general.el and hydra.el

;;; Code:
(require 'configs-languages)
(require 'configs-terminal)

(defvar configs--vim-states '(normal insert visual emacs motion))

;; general definers
(general-create-definer kb/global-leader-key
  :states        configs--vim-states
  :prefix        "C-\\"
  :global-prefix "C-\\")

(general-create-definer kb/local-leader-key
  :states        configs--vim-states
  :prefix        ","
  :global-prefix "C-,")

(general-create-definer kb/search-leader-key
  :states        configs--vim-states
  :prefix        ";"
  :global-prefix "C-;")

(general-create-definer kb/completion-leader-key
  :states        'insert
  :prefix        "C-."
  :global-prefix "C-.")

(general-define-key ;; global {{{
 (kbd "<escape>") #'keyboard-escape-quit
 (kbd "C-q")      #'keyboard-quit

 "M-|"  #'popper-toggle-type

 "M-["  #'tab-bar-switch-to-prev-tab
 "M-]"  #'tab-bar-switch-to-next-tab

 (kbd "C-<left>")  #'evil-window-left
 (kbd "C-<right>") #'evil-window-right
 (kbd "C-<up>")    #'evil-window-up
 (kbd "C-<down>")  #'evil-window-down

 (kbd "S-<left>")  #'(lambda () (interactive)(evil-scroll-column-left 5))
 (kbd "S-<right>") #'(lambda () (interactive)(evil-scroll-column-right 5))
 (kbd "S-<up>")    #'(lambda () (interactive)(evil-scroll-line-up 10))
 (kbd "S-<down>")  #'(lambda () (interactive)(evil-scroll-line-down 10))

 :keymaps '(override))
;; }}}

(general-define-key ;; company-mode {{{
 (kbd "<escape>") '(company-abort)

 :keymaps '(company-active-map))
;; }}}

(general-define-key ;; minibuffer {{{
 ;; marginalia {{{
 "M-A" #'marginalia-cycle
 ;; }}}

 ;; consult {{{
 "M-s" #'consult-history
 "M-r" #'consult-history
 ;; }}}

 "M-," #'embark-act

 :keymaps '(minibuffer-local-map))
;; }}}

(general-define-key ;; popper-mode {{{
 (kbd "M-<prior>")  #'popper-cycle-backwards
 (kbd "M-<next>")   #'popper-cycle
 (kbd "M-<delete>") #'popper-kill-latest-popup

 :keymaps '(popper-mode-map))
;; }}}

(general-define-key ;; evil inner object {{{
 "a" #'evil-inner-arg

 :keymaps '(evil-inner-text-objects-map))
;; }}}

(general-define-key ;; evil outer object {{{
 "a" #'evil-outer-arg

 :keymaps '(evil-outer-text-objects-map))
;; }}}

(general-define-key ;; evil window {{{
 "O" #'(lambda () (interactive)(configs--layout-base)(dashboard-open))
 "q" #'(lambda () (interactive)(if (one-window-p)(tab-bar-close-tab)(delete-window)))
 "Q" #'(lambda () (interactive)(if (one-window-p)(tab-bar-close-tab)(kill-buffer-and-window)))
 "V" #'split-window-horizontally
 "v" #'(lambda () (interactive)(split-window-horizontally)(other-window 1))
 "S" #'split-window-vertically
 "s" #'(lambda () (interactive)(split-window-vertically)(other-window 1))
 "u" #'winner-undo
 "U" #'winner-redo

 :keymaps '(evil-window-map))
;; }}}

(general-define-key ;; evil normal {{{
 "L"  #'evil-forward-arg
 "H"  #'evil-backward-arg
 "K"  #'evil-jump-out-args
 "u"  #'undo-fu-only-undo
 "U"  #'undo-fu-only-redo

 "]g" #'diff-hl-next-hunk
 "[g" #'diff-hl-previous-hunk

 "[o" #'origami-previous-fold
 "]o" #'origami-next-fold

 :keymaps '(evil-normal-state-map))
;; }}}

(general-define-key ;; evil motion {{{
 "L" #'evil-forward-arg
 "H" #'evil-backwards-arg

 :keymaps '(evil-motion-state-map))
;; }}}

;; global leader
(defhydra hydra-dap-motion ;; {{{
  (:foreign-key exit :exit nil :timeout nil)
  "dap motion"
  ("i" #'dap-step-in "step-in")
  ("o" #'dap-step-out "step-out")
  ("n" #'dap-next "next")
  ("C" #'dap-continue "continue"))
;; }}}

(defhydra hydra-dap-session ;; {{{
  (:foreign-key exit :exit nil :timeout nil)
  "dap session"
  ("S" #'dap-ui-sessions "sessions")
  ("d" #'dap-ui-delete-session "delete session"))
;; }}}

(kb/global-leader-key ;; {{{
  ;; popup window {{{
  "C-\\"   '(popper-toggle :wk "toggle popup window")
  "C-M-\\" '(multi-vterm-project :wk "toggle project shell window")
  "C-|"    '(configs--multi-vterm :wk "create shell window")
  ;; }}}

  ;; terminal {{{
  "vr" '(multi-vterm-rename-buffer :wk "rename terminal")
  ;; }}}

  ;; tools {{{
  "p"  '(:ignore t :wk "tools")
  "pe" '(eval-region :wk "eval-region")
  "pE" '(eval-buffer :wk "eval-buffer")
  "pp" '(elpaca-try :wk "elpaca-try")
  "pP" '(elpaca-manager :wk "elpaca-manager")
  "pf" '(project-switch-project :wk "projects")
  "pF" '(ranger :wk "ranger")
  "ps" '(scratch-buffer :wk "scratch buffer")
  "pk" '((lambda () (interactive)
           (project-kill-buffers t)
           (delete-other-windows)
           (dashboard-open))
         :wk "kill project buffers")
  "pK" '((lambda () (interactive)
           (persp-kill (safe-persp-name (get-current-persp))))
         :wk "kill project frame")
  ;; }}}

  ;; workspaces/perspective management {{{
  "w"  '(:ignore t :wk "workspace")
  "ww" '(persp-switch :wk "switch perspective")
  "wk" '(persp-kill :wk "kill perspective")
  "wr" '(persp-rename :wk "rename perspective")
  "wb" '(persp-remove-buffer :wk "remove buffer from perspective")
  "wl" '(consult-layouts :wk "switch layouts")
  ;; }}}

  ;; buffer management {{{
  "b"  '(:ignore t :wk "buffer")
  "bl" '(project-list-buffers :wk "list buffers")
  "bL" '(list-buffers :wk "list buffers (all)")
  "bb" '(consult-project-buffer :wk "switch buffer")
  "bB" '(consult-buffer :wk "switch buffer (all)")
  "bX" '(kill-buffer :wk "kill buffer")
  "bk" '(kill-this-buffer :wk "kill current buffer")
  "bK" '(project-kill-buffers :wk "kill current buffer")
  "bQ" '(kill-buffer-and-window :wk "kill buffer and window")
  "bn" '(next-buffer :wk "next buffer")
  "bp" '(previous-buffer :wk "previous buffer")
  "br" '(revert-buffer :wk "reload buffer")
  ;; }}}

  ;; dap debugger {{{
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
  ;; }}}

  ;; window management {{{
  "s"  '(:ignore t :wk "switcher")
  "ss" '(ace-window :wk "select")
  "sS" '(ace-swap-window :wk "swap")
  "sd" '(ace-delete-window :wk "delete")
  ;; }}}

  ;; tab management {{{
  "t"  '(:ignore t :wk "tabs")
  "tt" '(tab-bar-new-tab :wk "new tab")
  "tT" '((lambda () (interactive)
           (tab-bar-new-tab)
           (dashboard-open))
         :wk "new fresh tab")
  "tk" '(tab-bar-close-tab :wk "kill tab")
  "tK" '(tab-bar-close-other-tabs :wk "kill other tabs")
  "tr" '(tab-bar-rename-tab :wk "rename tab")
  "tu" '(tab-bar-undo-close-tab :wk "undo tab close")
  ;; }}}

  ;; mode management {{{
  "m"  '(:ignore t :wk "modes")
  "mI" '(image-mode :wk "image mode")
  "mf" '(fundamental-mode :wk "fundamental mode")
  "mr" '(rainbow-mode :wk "rainbow mode")
  ;; }}}

  :keymaps '(override))
;; }}}

;; local leader
(defhydra hydra-git-hunk ;; {{{
  (:foreign-key exit :exit nil :timeout nil :hint nil)
  "git"
  ("i" #'diff-hl-show-hunk :hint nil)
  ("j" #'diff-hl-show-hunk-next :hint nil)
  ("k" #'diff-hl-show-hunk-previous :hint nil)
  ("s" #'diff-hl-show-hunk-stage-hunk :hint nil)
  ("d" #'diff-hl-show-hunk-revert-hunk :hint nil)
  ("c" #'diff-hl-show-hunk-copy-original-text :hint nil))
(hydra-set-property 'hydra-git-hunk :verbosity 0)
;; }}}

(kb/local-leader-key ;; {{{
  ;; general editor {{{
  ","  '(:ignore t :wk "editor")
  ",=" '(configs-indent-buffer :wk "indent buffer")
  ",+" '(sort-lines :wk "sort lines")
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
  ",!" '(flycheck-display-error-at-point :wk "show error at point")
  ;; }}}

  ;; lsp {{{
  "d"  '(:ignore t :wk "lsp")
  "di" '(lsp-describe-thing-at-point :wk "find definition")
  "dI" '(lsp-find-implementation :wk "find implementation")
  "dt" '(lsp-find-type-definition :wk "find type definition")
  "dd" '(lsp-find-definition :wk "find definition")
  "dr" '(lsp-find-references :wk "find reference")
  ;; }}}

  ;; code folding {{{
  "o" '(:ignore t :wk "code fold")
  "oa" '(configs-add-fold-inline :wk "add inline fold braces")
  "oA" '(configs-add-fold-surround :wk "add surround fold braces")
  "ox" '(origami-open-node-recursively :wk "open")
  "oX" '(origami-open-all-nodes :wk "open all")
  "oc" '(origami-close-node-recursively :wk "close")
  "oC" '(origami-close-all-nodes :wk "close all")
  "oo" '(origami-recursively-toggle-node :wk "toggle")
  "oO" '(origami-toggle-all-nodes :wk "toggle all")
  "o!" '(origami-show-only-node :wk "open only")
  ;; }}}

  ;; editor motion {{{
  "m"  '(:ignore t :wk "motion")
  "mj" '(evilem-motion-find-char :wk "fowards motion")
  "mk" '(evilem-motion-find-char-backward :wk "backwards motion")
  "mJ" '(evilem-motion-find-char-to :wk "forwards motion to")
  "mK" '(evilem-motion-find-char-to-backward :wk "backwards motion to")
  "ms" '(evil-snipe-s :wk "forwards inclusive snipe")
  "mS" '(evil-snipe-S :wk "backwards inclusive snipe")
  "mx" '(evil-snipe-x :wk "forwards exclusive snipe")
  "mX" '(evil-snipe-X :wk "backwards exclusive snipe")
  ;; }}}

  ;; git {{{
  "g"  '(:ignore t :wk "git")
  "gi" '(hydra-git-hunk/diff-hl-show-hunk :wk "inspect")
  "gj" '(diff-hl-next-hunk :wk "next hunk")
  "gk" '(diff-hl-previous-hunk :wk "prev hunk")
  "gs" '(diff-hl-stage-current-hunk :wk "stage")
  "gd" '(diff-hl-revert-hunk :wk "revert")
  ;; }}}

  ;; languages/translations {{{
  "i"  '(:ignore t :wk "input method")
  "ii" '(configs--set-default-input-method :wk "English input ")
  "ij" '(configs--set-japanese-input-method :wk "Japanese input ")

  "t"  '(:ignore t :.k "translate")
  "tt" '(google-translate-at-point :wk "translate source -> target")
  "tT" '(google-translate-at-point-reverse :wk "translate target -> source")
  ;; }}}

  :keymaps '(override))
;; }}}

(kb/search-leader-key ;; global {{{
  "g" '(consult-ripgrep :wk "find text in project")
  "r" '(consult-recent-file :wk "find recent file")
  "f" '(project-find-file :wk "find project file")
  "F" '(find-file :wk "find file")
  "M" '(consult-man :wk "find man page")
  "K" '(consult-kmacro :wk "find keyboard macro")
  "C" '(consult-mode-command :wk "find recent commands")
  "H" '(consult-history :wk "find history")
  "I" '(consult-info :wk "find emacs info")
  :keymaps '(override))
;; }}}

(kb/search-leader-key ;; org-mode {{{
  "h" '(consult-org-heading :wk "find org heading")
  :keymaps '(org-mode-map))
;; }}}

(kb/completion-leader-key ;; {{{
  "s"   '(company-ispell :wk "suggest word")
  "C-," '(company-yasnippet :wk "suggest snippet")
  "C-." '(company-complete :wk "suggest completion")
  ">"   '(company-show-doc-buffer :wk "show code completion doc")
  :keymaps '(override))
;; }}}

(provide 'keybind-init)
;;; init.el ends here
