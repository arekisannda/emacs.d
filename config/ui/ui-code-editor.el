;;; ui-code-editor.el --- Emacs Code Editor Configurations  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'hideshow)

(defvar ui/code-editor--fold-overlay-string
  (concat " "
          (propertize "!" 'face `(nil :inherit success
                                      :weight bold
                                      :box '(:style flat)))
          (propertize "!" 'face `(nil :inherit warning
                                      :weight bold
                                      :box '(:style flat)))
          (propertize "!" 'face `(nil :inherit error
                                      :weight bold
                                      :box '(:style flat)))
          " "))

(defun ui/code-editor--fold-overlay (ov)
  "Format fold overlay OV."
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'display ui/code-editor--fold-overlay-string)))

(defvar ui/code-editor--overlay-fold-function #'ui/code-editor--fold-overlay)
(defun ui/code-editor--fold-setup ()
  "Set up Code folding."
  (setq hs-set-up-overlay ui/code-editor--overlay-fold-function))

(defun ui/code-editor-setup ()
  "Set up code configurations."
  (ui/code-editor--fold-setup)

  (require 'lang-generic)
  (require 'lang-elisp)
  (require 'lang-org)
  (require 'lang-clang)
  (require 'lang-csharp)
  (require 'lang-go)
  (require 'lang-lua)
  (require 'lang-python))

(provide 'ui-code-editor)

;;; ui-code-editor.el ends here
