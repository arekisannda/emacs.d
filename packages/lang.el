;;; lang.el --- Emacs language/translation configurations -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package mozc
  :ensure t)

(use-package mozc-cand-posframe
  :after mozc
  :defer t
  :ensure t)

(use-package google-translate)

(elpaca-wait)

;; set input methods
(defun configs--set-japanese-input-method ()
  "Load japanese-mozc input method"
  (interactive)
  (setq google-translate-default-source-language "ja")
  (setq google-translate-default-target-language "en")
  (set-input-method 'japanese-mozc))

(defun configs--set-default-input-method ()
  "Load default (English) input method"
  (interactive)
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language nil)
  (set-input-method nil))

(provide 'packages-lang)
;;; lang.el ends here
