;;; lang-elisp.el --- Emacs Lisp Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package emacs
  :ensure nil
  :preface
  (defun +lang-elisp-setup ()
    "Setup to run for `emacs-lisp-mode` major-modes."
    (shut-up
      (corfu-candidate-overlay-mode 1))

    (hs-minor-mode 1)
    (add-hook 'before-save-hook #'util/indent-buffer nil 'local)
    (util/lang--add-to-capf-list (list #'cape-dabbrev
                                       #'cape-file
                                       #'cape-elisp-symbol
                                       #'cape-keyword)))

  :hook
  (emacs-lisp-mode . +lang-elisp-setup))

(provide 'lang-elisp)

;;; lang-elisp.el ends here
