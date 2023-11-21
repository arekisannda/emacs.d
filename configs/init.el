;;; init.el --- Emacs base configurations -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(setq config-directory (expand-file-name "configs" user-emacs-directory))

(load-file (expand-file-name "gui.el" config-directory))
(load-file (expand-file-name "ispell.el" config-directory))

(provide 'packages-init)

;;; init.el ends here
