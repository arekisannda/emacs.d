;;; lang-lua.el --- Lua Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'packages-init)
(require 'lang-utils)

(defun lang/lua--setup ()
  "Setup to run for lua major modes."
  (setq lua-indent-level 2)
  (setq lua-indent-nested-block-content-align nil)
  (setq lua-indent-close-paren-align nil)

  (advice-add
   #'lua-calculate-indentation-block-modifier
   :around #'(lambda (old-funcion &rest arguments)
               (let ((old-res (apply old-function arguments)))
                 (if (> old-res lua-indent-level) lua-indent-level old-res))))
  )

(defun lang/lua-setup ()
  "Configurations for lua."
  (add-hook 'lua-mode-hook #'lang/lua--setup))

(lang/lua-setup)

(provide 'lang-lua)

;;; lang-lua.el ends here
