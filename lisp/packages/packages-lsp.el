;;; packages-lsp.el --- LSP/Language Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-lang)

(use-package flymake
  :custom
  (flymake-start-on-flymake-mode t)
  (flymake-show-diagnostics-at-end-of-line nil)
  (flymake-indicator-type nil)
  (flymake-fringe-indicator-position nil)
  :custom-face
  (flymake-warning
   ((nil :underline (:style wave :color ,(doom-color 'orange))))))

(defvar-local +eglot-local-contact nil
  "Buffer-local `eglot' server configuration for project.")

(defun +eglot-with-local (alternative)
  "Generate server-choosing function with `+eglot-local-contact'.
If it is not set, use ALTERNATIVE instead."
  (lambda (&optional interactive _project)
    (if (and +eglot-local-contact (listp +eglot-local-contact))
        +eglot-local-contact
      alternative)))

(use-package eglot
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-documentation-strategy #'eldoc-documentation-compose)
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-sync-connect nil)
  (eglot-connect-timeout nil)
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 3)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider
                                       :inlayHintProvider
                                       :signatureHelpProvider))
  ;; (eglot-extend-to-xref nil)
  ;; (eglot-highlight-symbol-face ((t (:inherit (lazy-highlight)))))
  :config
  (let ((nixd         '("nixd" :name "nixd"))
        (tinymist     '("tinymist" :name "tinymist"))
        (texlab       '("texlab" :name "texlab"))
        (ltex-ls-plus '("ltex-ls-plus" :name "ltex-ls-plus"))
        (ty           '("ty" "server" :name "ty"))
        (pyrefly      '("pyrefly" "lsp" :name "pyrefly"))
        (biome        '("biome" "lsp-proxy" :name "biome"))
        (html         (+eglot-with-local '("vscode-html-language-server" "--stdio" :name "vscode-html")))
        (scad         '("openscad-lsp" "--stdio"))
        )

    (dolist (conf `(((scad-mode                  :language-id "scad")             . ,scad)
                    ((nix-ts-mode                :language-id "nix")              . ,nixd)
                    ((typst-ts-mode              :language-id "typst")            . ,tinymist)
                    ((yaml-ts-mode               :language-id "yaml")             . ,ltex-ls-plus)
                    ((org-mode                   :language-id "org")              . ,ltex-ls-plus)
                    ((git-commit-elisp-text-mode :language-id "gitcommit")        . ,ltex-ls-plus)
                    ((bibtex-mode                :language-id "bibtex")           . ,ltex-ls-plus)
                    ((context-mode               :language-id "context")          . ,ltex-ls-plus)
                    (((latex-mode                :language-id "latex")
                      (LaTeX-mode                :language-id "latex"))           . ,texlab)
                    ((markdown-mode              :language-id "markdown")         . ,ltex-ls-plus)
                    ((rst-mode                   :language-id "restructuredtext") . ,ltex-ls-plus)
                    (((python-ts-mode            :language-id "python")
                      (python-mode               :language-id "python"))          . ,pyrefly)
                    ((html-mode                  :language-id "html")             . ,html))
                  )
      (setf (alist-get (car conf) eglot-server-programs nil nil #'equal)
            (cdr conf))))

  (fset #'jsonrpc--log-event #'ignore)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  (defun +eglot--shutdown-project (name)
    (let* ((project (project-current nil name))
           (servers (gethash project eglot--servers-by-project)))
      (dolist (server servers)
        (eglot-shutdown server))))

  (setq eglot-stay-out-of nil)
  :autoload eglot-managed-p
  :hook
  (prog-mode . util/lsp-ensure-modes)
  (text-mode . util/lsp-ensure-modes)
  (eglot-managed-mode . eldoc-mode))

(use-package consult-eglot :after eglot)

(use-package eglot-booster :after eglot
  :custom
  (eglot-booster-io-only t)
  :hook
  (after-init . eglot-booster-mode))

(use-package eldoc :after (evil flymake)
  :init
  (setq-default eldoc-display-functions '(eldoc-display-in-buffer))
  (defvar-local eldoc--old-display-functions nil)

  (defun eldoc--disable ()
    (setq-local eldoc--old-display-functions eldoc-display-functions
                eldoc-display-functions nil))

  (defun eldoc--enable ()
    (setq-local eldoc-display-functions eldoc--old-display-functions
                eldoc--old-display-functions nil))
  :hook
  (evil-insert-state-entry . eldoc--disable)
  (evil-insert-state-exit  . eldoc--enable))

(use-package eldoc-box :after eldoc
  :preface
  (defun +eldoc-box-max-width ()
    (let ((max-width 800)
          (set-width (ceiling (* (frame-pixel-width) 0.3))))
      (if (> set-width max-width) max-width set-width)))

  (defun +eldoc-box-max-height ()
    (let ((max-height 600)
          (set-height (ceiling (* (frame-pixel-height) 0.3))))
      (if (> set-height max-height) max-height set-height)))
  :custom-face
  (eldoc-box-body
   ((nil :inherit default
         :background ,(doom-color 'bg-alt))))
  (eldoc-box-border
   ((nil :inherit popup-border
         :background unspecified
         :foreground unspecified)))
  (eldoc-box-markdown-separator
   ((nil :foreground "#323232")))
  :custom
  (eldoc-box-max-pixel-width #'+eldoc-box-max-width)
  (eldoc-box-max-pixel-height #'+eldoc-box-max-height)
  :config
  (add-to-list 'eldoc-box-self-insert-command-list 'eldoc-box-scroll-up)
  (add-to-list 'eldoc-box-self-insert-command-list 'eldoc-box-scroll-down)

  (defun eldoc-box-scroll-up (arg)
    "Scroll up ARG lines in the childframe."
    (interactive "p")
    (when eldoc-box--frame
      (with-selected-frame eldoc-box--frame
        (+buffer-scroll-up))))

  (defun eldoc-box-scroll-down (arg)
    "Scroll down ARG lines in the childframe."
    (interactive "p")
    (when eldoc-box--frame
      (with-selected-frame eldoc-box--frame
        (+buffer-scroll-down))))

  (defun eldoc-box--enable ()
    "Enable eldoc-box hover.
Intended for internal use."
    (if (not (boundp 'eldoc-display-functions))
        (add-function :before-while (local 'eldoc-message-function)
                      #'eldoc-box--eldoc-message-function)

      (setq-local eldoc-box--old-eldoc-functions
                  eldoc-display-functions)
      (remove-hook 'eldoc-display-functions #'eldoc-display-in-echo-area t)
      (add-hook 'eldoc-display-functions #'eldoc-box--eldoc-display-function -90 t))

    (when eldoc-box-clear-with-C-g
      (advice-add #'keyboard-quit :before #'eldoc-box-quit-frame)))

  (defun +eldoc-doc-buffer (&optional interactive)
    "Get or display ElDoc documentation buffer.

The buffer holds the results of the last documentation request.
If INTERACTIVE, display it.  Else, return said buffer."
    (interactive (list t))
    (unless (buffer-live-p eldoc--doc-buffer)
      (user-error (format
                   "ElDoc buffer doesn't exist, maybe `%s' to produce one."
                   (substitute-command-keys "\\[eldoc]"))))
    (let ((buf-name "*eldoc info*")
          (buf))
      (if (setq buf (get-buffer buf-name))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (replace-buffer-contents  eldoc--doc-buffer)))
        (with-current-buffer eldoc--doc-buffer
          (setq buf (clone-buffer buf-name t))))
      (with-current-buffer buf
        (rename-buffer buf-name)
        (display-buffer (current-buffer)))))

  (defun eldoc-display-in-buffer (docs interactive)
    "Display DOCS in a dedicated buffer.
If INTERACTIVE is t, also display the buffer."

    (eldoc--format-doc-buffer docs)
    (when interactive (+eldoc-doc-buffer nil)))

  (defun +eldoc-close-buffer ()
    "Helper function to kill Eldoc doc buffer."
    (interactive)
    (let (window)
      (when (and (buffer-live-p eldoc--doc-buffer)
                 (setq window (get-buffer-window eldoc--doc-buffer)))
        (quit-window t window))))
  :hook
  (eldoc-mode . eldoc-box-hover-at-point-mode))


(provide 'packages-lsp)

;;; packages-lsp.el ends here
