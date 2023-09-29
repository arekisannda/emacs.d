;;; keybind/init --- summary:
;;; Emacs keybind configurations
;;; commentary:


;;; code:

(general-define-key
 (kbd "<escape>") #'keyboard-escape-quit
 (kbd "C-q")      #'keyboard-quit

 (kbd "C-<left>")  #'evil-window-left
 (kbd "C-<right>") #'evil-window-right
 (kbd "C-<up>")    #'evil-window-up
 (kbd "C-<down>")  #'evil-window-down)

;;;;
;; general keybinds
;;;;
(general-define-key
 "C-\\" #'vterm
 "C-|"  #'multi-vterm
 "M-["  #'tab-bar-switch-to-prev-tab
 "M-]"  #'tab-bar-switch-to-next-tab
 "C-,"  #'company-complete
 "C-."  #'company-show-doc-buffer)

;; vterm keybinds
(general-def vterm-mode-map
  "C-\\" #'switch-to-prev-buffer
  "M-]"  #'tab-bar-switch-to-next-tab

  (kbd "C-<left>")  #'evil-window-left
  (kbd "C-<right>") #'evil-window-right
  (kbd "C-<up>")    #'evil-window-up
  (kbd "C-<down>")  #'evil-window-down)


(general-def minibuffer-local-map
  "M-A" #'marginalia-cycle)

;;;;
;; vim-mode keybinds
;;;;
(general-create-definer kb/leader-key
  :states        '(normal insert visual emacs motion)
  :keymaps       'override
  :prefix        "\\"
  :global-prefix "M-\\")

(general-create-definer kb/local-leader-key
  :states        '(normal insert visual emacs motion)
  :keymaps       'override
  :prefix        ","
  :global-prefix "M-,")

(general-create-definer kb/search-leader-key
  :states        '(normal insert visual emacs motion)
  :keymaps       'override
  :prefix        ";"
  :global-prefix "M-;")

;; bind evil-args text objects
(general-def evil-inner-text-objects-map
  "a" #'evil-inner-arg)

(general-def evil-outer-text-objects-map
  "a" #'evil-outer-arg)

(general-def evil-window-map
  "V" #'split-window-horizontally
  "v" (lambda () (interactive)(split-window-horizontally)(other-window 1))
  "S" #'split-window-vertically
  "s" (lambda () (interactive)(split-window-vertically)(other-window 1)))

(general-def evil-normal-state-map
  ;; bind evil-forward/backward-args
  "L"  #'evil-forward-arg
  "H"  #'evil-backwards-arg
  "K"  #'evil-jump-out-args
  "u"  #'undo-fu-only-undo
  "U"  #'undo-fu-only-redo

  "]g" #'diff-hl-next-hunk
  "[g" #'diff-hl-previous-hunk)

(general-def evil-motion-state-map
  ;; bind evil-forward/backward-args
  "L" #'evil-forward-arg
  "H" #'evil-backwards-arg)

;; utility/tool keybinds
(kb/leader-key
  "p"  '(:ignore t :wk "tools")
  "pE" '(eval-buffer :wk "eval-buffer")
  "pp" '(elpaca-try :wk "elpaca-try")
  "pP" '(elpaca-manager :wk "elpaca-manager")
  "pf" '(project-switch-project :wk "projects")
  "pF" '(ranger :wk "ranger")
  "ps" '(scratch-buffer :wk "scratch buffer")
  "pt" '(popper-toggle-latest :wk "toggle window")
  "pT" '(popper-toggle-type :wk "toggle window type"))

;; quick window switcher keybinds
(kb/leader-key
  "s"  '(:ignore t :wk "switcher")
  "ss" '(ace-window :wk "select")
  "sS" '(ace-swap-window :wk "swap")
  "sd" '(ace-delete-window :wk "delete"))

;; tab management keybinds
(kb/leader-key
  "t"  '(:ignore t :wk "tabs")
  "tt" '((lambda () (interactive)(tab-bar-new-tab)(dashboard-open)) :wk "new tab")
  "tk" '(tab-bar-close-tab :wk "kill tab")
  "tK" '(tab-bar-close-other-tabs :wk "kill other tabs")
  "tr" '(tab-bar-rename-tab :wk "rename tab"))

;; buffer management keybinds
(kb/leader-key
  "b"  '(:ignore t :wk "buffer")
  "bb" '(consult-project-buffer :wk "switch buffer")
  "bB" '(consult-buffer :wk "switch buffer (all)")
  "bX" '(kill-buffer :wk "kill buffer")
  "bk" '(kill-this-buffer :wk "kill current buffer")
  "bK" '(project-kill-buffers :wk "kill project buffers")
  "bn" '(next-buffer :wk "next buffer")
  "bp" '(previous-buffer :wk "previous buffer")
  "br" '(revert-buffer :wk "reload buffer"))

;; mode management keybinds
(kb/leader-key
  "m"  '(:ignore t :wk "modes")
  "mr" '(rainbow-mode :wk "rainbow mode"))

;; editor keybinds
(kb/local-leader-key
  ","  '(:ignore t :wk "editor")
  ",=" '(sort-lines :wk "sort lines")
  ",k" '(align :wk "basic align")
  ",l" '(evil-lion-left :wk "align left")
  ",L" '(evil-lion-right :wk "align righ")
  ",c" '(evilnc-comment-or-uncomment-lines :wk "comment-line")
  ",C" '(evilnc-comment-or-uncomment-paragraphs :wk "comment-paragraph")
  ",s" '(embrace-add :wk "surround add")
  ",S" '(embrace-change :wk "surround change")
  ",d" '(embrace-delete :wk "surround delete"))

;; lsp
(kb/local-leader-key
  "d" '(:ignore t :wk "lsp")
  "dr" '(lsp-rename :wk "rename"))

;; editor motion keybinds
(kb/local-leader-key
  "m"  '(:ignore t :wk "motion")
  "mj" '(evilem-motion-find-char :wk "fowards motion")
  "mk" '(evilem-motion-find-char-backward :wk "backwards motion")
  "mJ" '(evilem-motion-find-char-to :wk "forwards motion to")
  "mK" '(evilem-motion-find-char-to-backward :wk "backwards motion to")
  "ms" '(evil-snipe-s :wk "forwards inclusive snipe")
  "mS" '(evil-snipe-S :wk "backwards inclusive snipe")
  "mx" '(evil-snipe-x :wk "forwards exclusive snipe")
  "mX" '(evil-snipe-X :wk "backwards exclusive snipe"))

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

;; git keybinds
(kb/local-leader-key
  "g"  '(:ignore t :wk "git")
  "gi" '(hydra-git-hunk/diff-hl-show-hunk :wk "inspect")
  "gj" '(diff-hl-next-hunk :wk "next hunk")
  "gk" '(diff-hl-previous-hunk :wk "prev hunk")
  "gs" '(diff-hl-stage-current-hunk :wk "stage")
  "gd" '(diff-hl-revert-hunk :wk "revert"))

;;;;
;; consult keybinds
;;;;
(kb/search-leader-key
  "g" #'consult-ripgrep
  "r" #'consult-recent-file
  "f" #'project-find-file
  "F" #'find-file
  "c" #'consult-mode-command
  "h" #'consult-history
  "k" #'consult-kmacro
  "m" #'consult-man
  "i" #'consult-info)

(general-def minibuffer-local-map
  "M-s" #'consult-history
  "M-r" #'consult-history)

(provide 'keybinds-init)
;;; init.el ends here
