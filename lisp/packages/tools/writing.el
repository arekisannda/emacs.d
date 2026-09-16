;;; tools/writing.el -*- lexical-binding: t; -*-

(use-package dictionary
  :custom
  (dictionary-use-single-buffer t)
  (dictionary-create-buttons nil)
  (dictionary-display-definition-function nil)
  (dictionary-server "dict.org")
  :preface
  (defun +dictionary-word-etymology (&optional word)
    "Search WORD etymology."
    (interactive
     (list (read-string "Word: " (current-word))))
    (w3m-browse-url (format "https://etymonline.com/word/%s" word)))
  :config
  (utils/custom-set-faces
   (dictionary-word-entry-face
    ((nil :inverse-video nil
          :foreground ,(doom-color 'grey)
          )))
   )
  :hook
  (dictionary-mode . emacs-set-alt-face))

(use-package google-translate
  :custom
  (google-translate-default-source-language "en")
  :config
  (require 'google-translate-smooth-ui)
  (google-translate--setup-minibuffer-keymap)
  (setq google-translate-translation-directions-alist
        '(("ja" . "en") ("en" . "ja") )))

(use-package writegood :defer t)
