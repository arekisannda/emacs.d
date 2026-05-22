;;; editor/evil.el -*- lexical-binding: t; -*-

(use-package undo-fu)

(use-package smartparens
  :custom
  (sp-autoinsert-pair nil)
  (sp-autoskip-closing-pair t)
  (sp-autoskip-opening-pair t)
  :hook
  (prog-mode . smartparens-global-mode))

(defmacro evil-move-or-goto-line-around (dir)
  "Wrapper for evil move in DIR."
  `(lambda (fn &optional arg)
     (let ((inhibit-message t))
       (if arg
           (goto-line (,(if dir '+ '-) (line-number-at-pos) arg))
         (apply fn arg)))))

(use-package evil :after undo-fu
  :custom
  (evil-want-integration t)
  (evil-default-state 'normal)
  (evil-undo-system 'undo-fu)
  (evil-split-window-below nil)
  (evil-vsplit-window-right nil)
  (evil-goto-definition-functions
   '(evil-goto-definition-semantic
     evil-goto-definition-xref
     evil-goto-definition-search
     ))
  :init

  (setq-default evil-respect-visual-line-mode t
                evil-want-keybinding nil
                evil-want-minibuffer nil)
  :config
  (setq evil-insert-state-modes
        '( erc-mode geiser-repl-mode gud-mode
           internal-ange-ftp-mode haskell-interactive-mode reb-mode
           slime-repl-mode  utop-mode wdired-mode))

  (setq evil-emacs-state-modes
        (delete-dups
         (append '(agent-shell-mode
                   calc-mode
                   calculator-mode
                   calendar-mode
                   comint-mode
                   dap-ui-breakpoints-ui-list-mode
                   dape-info-parent-mode
                   dape-repl-mode
                   dashboard-mode
                   eglot-list-connections-mode
                   eshell-mode
                   inferior-apl-mode
                   inferior-caml-mode
                   inferior-emacs-lisp-mode
                   inferior-j-mode
                   inferior-python-mode
                   inferior-python-mode
                   inferior-scheme-mode
                   inferior-sml-mode
                   prolog-inferior-mode
                   racket-repl-mode
                   shell-mode
                   special-mode
                   term-mode
                   vterm-mode)
                 evil-emacs-state-modes)))

  (setq evil-normal-state-modes
        (delete-dups
         (append '(code-review-mode
                   pr-review-mode
                   Custom-mode)
                 evil-normal-state-modes)))

  (setq evil-motion-state-modes
        '(Info-mode
          Man-mode
          apropos-mode
          backtrace-mode
          color-theme-mode
          command-history-mode
          compilation-mode
          detached-log-mode
          devdocs-mode
          dictionary-mode
          embark-collect-mode
          ert-results-mode
          eww-mode
          help-mode
          helpful-mode
          messages-buffer-mode
          rfc-mode
          shell-command-mode
          speedbar-mode
          tabulated-list
          undo-tree-visualizer-mode
          woman-mode))

  (advice-add #'evil-next-line :around (evil-move-or-goto-line-around t))
  (advice-add #'evil-previous-line :around (evil-move-or-goto-line-around nil))
  (advice-add #'evil-next-visual-line :around (evil-move-or-goto-line-around t))
  (advice-add #'evil-previous-visual-line :around (evil-move-or-goto-line-around nil))

  (advice-add #'evil-next-buffer :override (apply-partially #'tab-line-switch-to-next-tab nil))
  (advice-add #'evil-prev-buffer :override (apply-partially #'tab-line-switch-to-prev-tab nil))
  :hook
  (after-init . evil-mode))

(use-package evil-collection :after evil
  :custom
  (evil-collection-mode-list
   '((custom cus-edit)
     calc
     dired
     edebug
     ediff
     forge
     ibuffer
     info
     magit
     org
     org-roam
     w3m
     ))
  :diminish evil-collection-unimpaired-mode
  :init
  (setq forge-add-default-bindings nil)
  :hook
  (evil-mode . evil-collection-init))

(use-package evil-nerd-commenter :after evil)

(use-package evil-args :after evil)

(use-package evil-matchit :after evil
  :hook
  (evil-mode . global-evil-matchit-mode))

(use-package evil-mc :after evil
  :init
  (setq evil-mc-cursors-map (make-sparse-keymap)
        evil-mc-key-map (make-sparse-keymap))
  :hook
  (evil-mode . global-evil-mc-mode))

(use-package evil-surround
  :custom
  (evil-surround-pairs-alist
   '((?\( . ("(" . ")"))
     (?\[ . ("[" . "]"))
     (?\{ . ("{" . "}"))

     (?\) . ("( " . " )"))
     (?\] . ("[ " . " ]"))
     (?\} . ("{ " . " }"))

     (?# . ("#{" . "}"))
     (?> . ("<" . ">"))
     (?t . evil-surround-read-tag)
     (?< . evil-surround-read-tag)
     (?\C-f . evil-surround-prefix-function)
     (?f . evil-surround-function)))
  :hook
  (evil-mode . global-evil-surround-mode))
