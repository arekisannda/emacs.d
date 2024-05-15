;;; packages-japanese.el --- Japanese-lang Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-helpers)

;; (use-package mozc
;;   :custom
;;   (default-input-method 'japanese-mozc))

;; (use-package mozc-cand-posframe :after mozc
;;   :ensure (mozc-cand-posframe :type git
;;                               :host github
;;                               :repo "arekisannda/mozc-posframe")
;;   :custom
;;   (mozc-candidate-style 'posframe)
;;   :hook
;;   (mozc-mode . mozc-cand-posframe-init))

(use-package google-translate
  :demand t
  :custom
  (google-translate-default-source-language "en")
  :config
  (require 'google-translate-smooth-ui)
  (google-translate--setup-minibuffer-keymap)
  (setq google-translate-translation-directions-alist
        '(("ja" . "en") ("en" . "ja") )))

(use-package migemo :disabled)

(provide 'packages-japanese)

;;; packages-japanese.el ends here
