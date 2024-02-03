;;; packages-natural-languages.el --- Natural Language Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'elpaca)

(use-package google-translate)

(use-package mozc :ensure t)

(use-package mozc-cand-posframe :ensure t :after mozc :defer t)

(use-package migemo :ensure t :disabled)

(provide 'packages-natural-languages)

;;; packages-natural-languages.el ends here
