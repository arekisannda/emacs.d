;;; tools/writing.el -*- lexical-binding: t; -*-

(use-package dictionary
  :custom
  (dictionary-use-single-buffer t)
  (dictionary-server "dict.org")
  (dictionary-display-definition-function #'dictionary-display-definition-in-help-buffer)
  :preface
  (defun +dictionary-word-etymology (&optional word)
    "Search WORD etymology."
    (interactive
     (list (read-string "Word: " (current-word))))
    (w3m-browse-url (format "https://etymonline.com/word/%s" word))))

(use-package google-translate
  :custom
  (google-translate-default-source-language "en")
  :config
  (require 'google-translate-smooth-ui)
  (google-translate--setup-minibuffer-keymap)
  (setq google-translate-translation-directions-alist
        '(("ja" . "en") ("en" . "ja") )))

(use-package writegood)
