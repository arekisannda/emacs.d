;; init.el --- Emacs packages init -*- lexical-binding: t; origami-fold-style: triple-braces; -*-
;;; Commentary:

;;; Code:

(defvar configs--directory (expand-file-name "configs" configs--user-emacs-directory))
(load-file (expand-file-name "utils.el"       configs--directory))
(load-file (expand-file-name "terminal.el"    configs--directory))
(load-file (expand-file-name "programming.el" configs--directory))
(load-file (expand-file-name "languages.el"   configs--directory))
(load-file (expand-file-name "org.el"         configs--directory))
(load-file (expand-file-name "projects.el"    configs--directory))
(load-file (expand-file-name "minibuffer.el"  configs--directory))
(load-file (expand-file-name "editor.el"      configs--directory))
(load-file (expand-file-name "layouts.el"     configs--directory))

(elpaca-wait)

;;; emacs built-in configurations {{{
(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default ring-bell-function #'ignore)
(setq-default display-line-numbers-type 'relative)
(setq-default find-file-visit-truename t)
(setq-default inhibit-startup-screen t)
(setq-default confirm-nonexistent-file-or-buffer nil)
(setq-default confirm-kill-processes nil)
(setq-default auto-save-default nil)
(setq-default make-backup-files nil)
(setq-default create-lockfiles nil)
(setq-default indent-tabs-mode nil)
(setq-default line-spacing 0)
(setq-default truncate-lines t)

;; feature configurations
(global-eldoc-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(visual-line-mode -1)
(electric-pair-mode -1)
(winner-mode 1)

;; fringe
(setq-default fringe-styles 'default)
(setq-default fringe-indicator-alist nil)
(fringe-mode 5)

;; tab-bar
(tab-bar-mode 1)
(setq-default tab-bar-close-button-show nil)
(setq-default tab-bar-new-button-show nil)
(setq-default tab-bar-auto-width-max '(150 20))
(setq-default tab-bar-auto-width-min '(20 2))

;; log level settings
(setq-default message-log-max 2000)
(setq-default warning-minimum-level :emergency)
;; (kill-buffer "*Messages*")

;;; }}}

;;; deferred popup configurations {{{
(defun configs--enable-posframe ()
  "Use posframe."
  (add-hook 'flycheck-mode-hook 'flycheck-posframe-mode)
  (ace-window-posframe-mode 1)
  (company-posframe-mode 1)

  (require 'mozc-cand-posframe)
  (setq mozc-candidate-style 'posframe))

(defun configs--enable-popup ()
  "Use popup."
  (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))

(defun configs--get-popup-type ()
  "Retrieve popup dependency package."
  (if (display-graphic-p) 'posframe 'popup))

(defun configs--load-deferred-configurations ()
  "Load deferred package configurations."
  (let ((popup-type (configs--get-popup-type)))
    (cl-case popup-type
      (posframe (configs--enable-posframe))
      (popup (configs--enable-popup))
      (otherwise (message "Unable to configure popup")))))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame) (with-selected-frame frame (configs--load-deferred-configurations))))
  (configs--load-deferred-configurations))
;;; }}}

;;; tree-sitter {{{
(setq-default treesit-language-source-alist
              '((bash . ("https://github.com/tree-sitter/tree-sitter-bash.git"))
                (c . ("https://github.com/tree-sitter/tree-sitter-c.git"))
                (cmake . ("https://github.com/uyha/tree-sitter-cmake.git"))
                (c-sharp . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
                (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp.git"))
                (css . ("https://github.com/tree-sitter/tree-sitter-css.git"))
                (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
                (go . ("https://github.com/tree-sitter/tree-sitter-go.git"))
                (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
                (html . ("https://github.com/tree-sitter/tree-sitter-html.git"))
                (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript.git"))
                (json . ("https://github.com/tree-sitter/tree-sitter-json.git"))
                (latex . ("https://github.com/latex-lsp/tree-sitter-latex.git"))
                (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
                (python . ("https://github.com/tree-sitter/tree-sitter-python.git"))
                (rust . ("https://github.com/tree-sitter/tree-sitter-rust.git"))
                (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "tsx/src"))
                (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "typescript/src"))
                (yaml . ("https://github.com/ikatyang/tree-sitter-yaml.git"))))

(dolist (lang (mapcar #'car treesit-language-source-alist))
  (unless (treesit-language-available-p lang)
    (treesit-install-language-grammar lang)))
;;; }}}

;;; additional hook/mode configurations {{{
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'i3wm-config-mode #'display-line-numbers-mode)

(dolist (modes '(("\\.ya?ml\\'" . yaml-ts-mode)
                 ("\\.rs\\'" . rust-ts-mode)
                 ("\\.go\\'" . go-ts-mode)
                 ("\\.tsx\\'" . tsx-ts-mode)
                 ("\\.ts\\'" . typescript-ts-mode)
                 ("Dockerfile\\'" . dockerfile-ts-mode)
                 ("\\.dockerfile\\'" . dockerfile-ts-mode)
                 ("\\.jsonc\\'" . json-ts-mode)
                 ("\\.sway\\'" . i3wm-config-mode)))
  (configs--dedup-add-to-list 'auto-mode-alist modes))

(setq-default major-mode-remap-alist
              '((c++-mode . c++-ts-mode)
                (c-mode . c-ts-mode)
                (c-or-c++-mode . c-or-c++-ts-mode)
                (csharp-mode . csharp-ts-mode)
                (css-mode . css-ts-mode)
                (javascript-mode . js-ts-mode)
                (python-mode . python-ts-mode)
                (sh-mode . bash-ts-mode)
                (js-json-mode . json-ts-mode)))

;;; use emacs-mode for non prog-modes
(dolist (mode '(vterm-mode
                ranger-mode
                elpaca-ui-mode
                message-mode
                special-mode
                dap-ui-breakpoints-ui-list-mode
                eglot-list-connections-mode))
  (add-to-list 'evil-emacs-state-modes mode))


;;; set read-only files
(defvar configs--read-only-prefixes-list (list (expand-file-name elpaca-directory)
                                               (expand-file-name package-user-dir))
  "List of read-only file prefixes.")

(defun configs--set-read-only-mode ()
  "Enable read-only mode for files."
  (when (and buffer-file-name
             (or (cl-loop for prefix in configs--read-only-prefixes-list
                          thereis (string-prefix-p prefix buffer-file-name))))
    (read-only-mode 1)))

(add-hook 'find-file-hook #'configs--set-read-only-mode)
;;; }}}

;; font configurations {{{
(configs--dedup-add-to-list
 'default-frame-alist
 `(font . ,(concat configs--fixed-pitch-font-face "-9")))

(defun configs--set-custom-faces ()
  "Config method to set faces on frame create."
  (set-face-attribute 'default nil
                      :font configs--fixed-pitch-font-face
                      :height configs--fixed-pitch-font-size
                      :weight 'normal)
  (set-face-attribute 'fixed-pitch nil
                      :font configs--fixed-pitch-font-face
                      :height configs--fixed-pitch-font-size
                      :weight 'normal)
  (set-face-attribute 'variable-pitch nil
                      :font configs--variable-pitch-font-face
                      :height configs--variable-pitch-font-size
                      :weight 'normal)
  (set-face-attribute 'italic nil
                      :slant 'italic
                      :underline nil)
  (set-face-attribute 'font-lock-comment-face nil
                      :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil
                      :slant 'italic)

  (set-face-attribute 'tab-bar nil
                      :font configs--fixed-pitch-font-face
		              :height configs--tab-font-size
		              :weight 'bold)
  (set-face-attribute 'tab-bar-tab nil
                      :font configs--fixed-pitch-font-face
                      :height configs--tab-font-size
                      :weight 'bold
                      :box '(:line-width (5 . 5) :style flat-button)
                      :underline `(:inherit tab-bar-tab :style line :position 0))
  (set-face-attribute 'tab-bar-tab-inactive nil
                      :font configs--fixed-pitch-font-face
		              :height configs--tab-font-size
		              :weight 'light
                      :box '(:line-width (5 . 5) :style flat-button)
                      :underline `(:inherit tab-bar-tab :style line :position 0)))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame) (with-selected-frame frame (configs--set-custom-faces))))
  (configs--set-custom-faces))
;;; }}}

(provide 'configs-init)
;;; init.el ends here
