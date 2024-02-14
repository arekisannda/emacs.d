;;; packages-themes.el --- Emacs Theme Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(elpaca `(sonokai-theme
          :ensure t
          :host ,(unless init-file-debug 'github)
          :repo ,(if init-file-debug
                     "~/Code/sonokai-emacs"
                   "arekisannda/sonokai-emacs")))

(provide 'packages-themes)

;;; packages-themes.el ends here
