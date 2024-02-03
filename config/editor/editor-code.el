;;; editor-code.el --- Emacs Code Editor Configurations  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'hideshow)

(defun editor/code--overlay-boxed-fold (ov)
  "Format OV overlay with ... for hidden block."
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'display
		 (concat " "
			 (propertize
			  "!"
			  'face `(nil :inherit success
				      :weight bold
				      :box '(:style flat)))
			 (propertize
			  "!"
			  'face `(nil :inherit warning
				      :weight bold
				      :box '(:style flat)))
			 (propertize
			  "!"
			  'face `(nil :inherit error
				      :weight bold
				      :box '(:style flat)))
			 " "))))

(defvar editor/code--overlay-fold-function #'editor/code--overlay-boxed-fold)
(defun editor/code--fold-setup ()
  "Set up Code folding."
  (setq hs-set-up-overlay editor/code--overlay-fold-function))

(defun editor/code-setup ()
  "Set up code configurations."
  (editor/code--fold-setup)

  (require 'lang-generic)
  (require 'lang-clang)
  (require 'lang-csharp)
  (require 'lang-go)
  (require 'lang-lua)
  (require 'lang-python))

(editor/code-setup)

(provide 'editor-code)

;;; editor-code.el ends here
