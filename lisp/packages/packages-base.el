;;; packages-base.el --- Base Packages Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-helpers)

(use-package aio :demand t)

(use-package shut-up :demand t)

(use-package no-littering :demand t)

(use-package diminish :demand t)

(use-package undo-fu :after diminish)

(use-package llama
  :ensure (:tag "v0.3.1"))

(use-package epkg :after llama)

(use-package general)

(use-package hydra
  :init
  (setq hydra-key-doc-function nil))

(defun +elpaca-unload-seq (e)
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))

(defun +elpaca-seq-build-steps ()
  (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                       elpaca--pre-built-steps elpaca-build-steps))
          (list '+elpaca-unload-seq 'elpaca--activate-package)))

(use-package seq :ensure `(seq :build ,(+elpaca-seq-build-steps)))

(use-package compat :ensure t)

(use-package persist :ensure t)

(use-package ov :ensure t)

(use-package jsonrpc
  :ensure t
  :config
  (fset #'jsonrpc--log-event #'ignore))

(use-package ext-tab-bar
  :ensure (:host github :repo "arekisannda/ext-tab-bar")
  :preface
  (defun +ext-tab-bar-name-format (tab i)
    (let ((current-p (eq (car tab) 'current-tab)))
      (propertize
       (concat (if tab-bar-tab-hints (format " %d " i) " ")
               (truncate-string-to-width
                (alist-get 'name tab)
                tab-bar-tab-name-truncated-max nil nil
                tab-bar-tab-name-ellipsis)
               (or (and tab-bar-close-button-show
                        (not (eq tab-bar-close-button-show
                                 (if current-p 'non-selected 'selected)))
                        tab-bar-close-button)
                   ""))
       'face (funcall tab-bar-tab-face-function tab))))
  :custom
  (tab-bar-tab-name-format-function #'+ext-tab-bar-name-format)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-tab-name-truncated-max 60)
  (tab-bar-auto-width t)
  (tab-bar-auto-width-max '(400 60))
  (tab-bar-auto-width-min '(100 15))
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (ext-tab-bar-project-disable-paths (list (expand-file-name elpaca-directory)
                                           (expand-file-name package-user-dir)))
  :config
  (util/if-daemon-run-after-make-frame-else-add-hook
   (ext-tab-bar-mode)
   'window-setup-hook))

(use-package indent-bars :disabled
  :elpaca (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module")))

(use-package emacs
  :ensure nil
  :custom
  (auth-source-pass-filename "~/.password-store/auth")
  :config
  (auth-source-pass-enable))

(provide 'packages-base)

;;; packages-base.el ends here
