;;; lang/lua.el -*- lexical-binding: t; -*-

(require 'util-lang)

(defun +lang-lua-mode-setup ()
  "Lua-mode setup."
  (setq-local lua-indent-level 2)
  (setq-local lua-indent-nested-block-content-align nil)
  (setq-local lua-indent-close-paren-align nil)

  (advice-add
   #'lua-calculate-indentation-block-modifier
   :around #'(lambda (old-function &rest arguments)
               (let ((old-res (apply old-function arguments)))
                 (if (> old-res lua-indent-level) lua-indent-level old-res))))
  )

(use-package lua-mode
  :config
  (util/update-alist
   'major-mode-remap-alist
   '((lua-mode . lua-ts-mode)))
  :mode
  ("\\.lua\\'" . lua-ts-mode)
  :hook
  (lua-ts-mode . +lang-lua-mode-setup))
