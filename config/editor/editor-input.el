;;; editor-input.el --- Emacs Input Method Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'packages-init)

(defun editor/input--set-japanese-input-method ()
  "Load japanese-mozc input method."
  (interactive)
  (setq google-translate-default-source-language "ja")
  (setq google-translate-default-target-language "en")
  (set-input-method 'japanese-mozc))

(defun editor/input--set-english-input-method ()
  "Load default (English) input method."
  (interactive)
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language nil)
  (deactivate-input-method)
  (evil-deactivate-input-method))

(provide 'editor-input)

;;; editor-input.el ends here
