;;; packages-magit.el --- Emacs Magit/vC Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-helpers)

;; Unload seq before continuing the elpaca build,
;; then continue to build the recipe E.
(elpaca `(seq :build ,(append (butlast (if (file-exists-p
                                            (expand-file-name "seq" elpaca-builds-directory))
                                           elpaca--pre-built-steps
                                         elpaca-build-steps))
                              (list #'(lambda (e)
                                        (util/unload-feature 'seq)
                                        (elpaca--continue-build e))
                                    #'elpaca--activate-package))))

(use-package magit :ensure t)

(use-package forge :ensure t :after magit)

(provide 'packages-magit)

;;; packages-magit.el ends here
