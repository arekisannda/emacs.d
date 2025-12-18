;;; tools/writing.el -*- lexical-binding: t; -*-

(use-package dictionary
  :defer t
  :custom
  (dictionary-use-single-buffer t)
  (dictionary-server "dict.org")
  (dictionary-display-definition-function #'dictionary-display-definition-in-help-buffer)
  :preface
  (defun +dictionary-word-etymology (&optional word)
    "Search WORD etymology."
    (interactive
     (list (read-string "Word: " (current-word))))
    (browse-url (format "https://etymonline.com/word/%s" word)))
  )

(use-package google-translate
  :defer t
  :custom
  (google-translate-default-source-language "en")
  :config
  (require 'google-translate-smooth-ui)
  (google-translate--setup-minibuffer-keymap)
  (setq google-translate-translation-directions-alist
        '(("ja" . "en") ("en" . "ja") )))

(use-package writeroom-mode
  :custom
  (visual-fill-column-width nil)
  (writeroom-width nil)
  (writeroom-fullscreen-effect 'maximized)
  (writeroom-restore-window-config t)
  (writeroom-header-line nil)
  (writeroom-mode-line nil))
