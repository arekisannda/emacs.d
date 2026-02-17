;;; editor/evil.el -*- lexical-binding: t; -*-

(use-package undo-fu)

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
  :init

  (setq-default evil-respect-visual-line-mode t
                evil-want-keybinding nil
                evil-want-minibuffer nil)
  :config
  (setq evil-insert-state-modes
        '( comint-mode erc-mode geiser-repl-mode gud-mode inferior-apl-mode inferior-caml-mode
           inferior-emacs-lisp-mode inferior-j-mode inferior-python-mode inferior-scheme-mode inferior-sml-mode
           internal-ange-ftp-mode haskell-interactive-mode prolog-inferior-mode racket-repl-mode reb-mode shell-mode
           slime-repl-mode term-mode utop-mode wdired-mode))

  (setq evil-emacs-state-modes
        (delete-dups
         (append '(vterm-mode
                   dape-info-parent-mode
                   special-mode
                   eshell-mode
                   dashboard-mode
                   dap-ui-breakpoints-ui-list-mode
                   dape-repl-mode
                   calc-mode
                   comint-mode
                   calculator-mode
                   calendar-mode
                   eglot-list-connections-mode
                   inferior-python-mode)
                 evil-emacs-state-modes)))

  (setq evil-normal-state-modes
        (delete-dups
         (append '(code-review-mode
                   pr-review-mode
                   Custom-mode)
                 evil-normal-state-modes)))

  (setq evil-motion-state-modes
        '(apropos-mode
          eww-mode
          detached-log-mode
          color-theme-mode
          tabulated-list
          command-history-mode
          messages-buffer-mode
          backtrace-mode
          compilation-mode
          dictionary-mode
          ert-results-mode
          help-mode
          helpful-mode
          Info-mode
          devdocs-mode
          Man-mode
          speedbar-mode
          embark-collect-mode
          undo-tree-visualizer-mode
          rfc-mode
          woman-mode))

  (advice-add #'evil-next-line :around (evil-move-or-goto-line-around t))
  (advice-add #'evil-previous-line :around (evil-move-or-goto-line-around nil))
  (advice-add #'evil-next-visual-line :around (evil-move-or-goto-line-around t))
  (advice-add #'evil-previous-visual-line :around (evil-move-or-goto-line-around nil))
  :hook
  (after-init . evil-mode))

(use-package evil-collection :after evil
  :custom
  (evil-collection-mode-list
   '(info
     w3m
     dired
     ibuffer
     (custom cus-edit)
     magit
     forge
     edebug
     org
     org-roam
     ediff))
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

(use-package embrace
  :custom-face
  (embrace-help-separator-face
   ((nil :inherit font-lock-comment-face)))
  (embrace-help-key-face
   ((nil :inherit font-lock-function-name-face)))
  (embrace-help-mark-func-face
   ((nil :inherit font-lock-constant-face)))
  (embrace-help-pair-face
   ((nil :inherit nil
         :foreground  ,(doom-color 'blue)
         :inverse-video nil)))
  :init
  (setq embrace-help-separator " : ")
  (setq embrace--help-add-column-width 2)
  (setq embrace-show-help-p t)
  :config
  (setq embrace--help-buffer-name " *embrace-help*")
  (defun +embrace--show-help-buffer (help-string)
    (let ((alist '((window-width  . #'util/windows-popup-fit-window-to-buffer)
                   (window-height . #'util/windows-popup-fit-window-to-buffer)
                   (window-popup  . bottom)
                   (dedicated . t))))
      (embrace--setup-help-buffer)
      (with-current-buffer embrace--help-buffer
        (face-remap-add-relative 'default `(nil :background ,(doom-color 'bg-alt)))
        (erase-buffer)
        (insert help-string)
        (goto-char (point-min)))
      (if (get-buffer-window embrace--help-buffer)
          (display-buffer-reuse-window embrace--help-buffer alist)
        (let ((w (split-window (frame-root-window nil) nil nil)))
          (window--display-buffer embrace--help-buffer w 'window alist)
          (fit-window-to-buffer w))
        )))

  (advice-add #'embrace--show-help-buffer :override #'+embrace--show-help-buffer))
