;;; packages-japanese.el --- Natural Language Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-helpers)

(use-package mozc :ensure t)

(use-package google-translate :ensure t :after mozc
  :config
  (setq google-translate-default-source-language "en")
  (setq google-translate-translation-directions-alist
        '(("ja" . "en") ("en" . "ja") ))
  (setq-default default-input-method 'japanese-mozc))

(use-package mozc-cand-posframe :ensure t :after mozc
  :elpaca (mozc-cand-posframe :type git
                              :host github
                              :repo "arekisannda/mozc-posframe")
  :config
  (setq mozc-candidate-style 'posframe)
  (util/if-daemon-run-after-make-frame (mozc-cand-posframe-init)))

(use-package migemo :ensure t :disabled)

(provide 'packages-japanese)

;;; packages-japanese.el ends here