;;; packages-japanese.el --- Natural Language Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package google-translate :ensure t)

(use-package mozc :ensure t)

(use-package mozc-cand-posframe :ensure t :after mozc :defer t
  :elpaca (:type git :host github :repo "arekisannda/mozc-posframe" :branch "master" :source nil))

(use-package migemo :ensure t :disabled)

(provide 'packages-japanese)

;;; packages-japanese.el ends here
