;;; languages.el --- Emacs natural language configurations -*- lexical-binding: t; origami-fold-style: triple-braces; -*-
;;; Commentary:

;;; Code:
(require 'ispell)

(use-package google-translate)
(use-package mozc :ensure t)
(use-package mozc-cand-posframe
  :after mozc
  :defer t
  :ensure t)

(elpaca-wait)

;; translations {{{
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
  (deactivate-input-method)
  (evil-deactivate-input-method))
;;; }}}

;;; ispell configurations {{{
(setq ispell-complete-word-dict
      (cond ((file-readable-p "/usr/dict/web2") "/usr/dict/web2")
            ((file-readable-p "/usr/share/dict/web2") "/usr/share/dict/web2")
            ((file-readable-p "/usr/dict/words") "/usr/dict/words")
            ((file-readable-p "/usr/lib/dict/words") "/usr/lib/dict/words")
            ((file-readable-p "/usr/share/dict/words") "/usr/share/dict/words")
            ((file-readable-p "/usr/share/lib/dict/words")
             "/usr/share/lib/dict/words")
            ((file-readable-p "/sys/dict") "/sys/dict")))

;; find aspell and hunspell automatically
(cond
 ((executable-find "hunspell")
  (setq ispell-program-name "/usr/bin/hunspell")
  (setq ispell-dictionary "en_US")
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
        ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))

  ;; new variable `ispell-hunspell-dictionary-alist' is defined in Emacs
  ;; If it's nil, Emacs tries to automatically set up the dictionaries.
  (when (boundp 'ispell-hunspell-dictionary-alist)
    (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)))

 ((executable-find "aspell")
  (setq ispell-program-name "/usr/bin/aspell")
  ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
  (setq ispell-extra-args '("-a" "soundslike" "--sug-mode=ultra" "--lang=en_US"))))

(defun ispell-display-buffer (buffer)
  "Show BUFFER in new window above selected one.
Also position fit window to BUFFER and select it."
  (let* ((unsplittable
          (cdr (assq 'unsplittable (frame-parameters (selected-frame)))))
         (window
          (or (get-buffer-window buffer)
              (and unsplittable
                   ;; If frame is unsplittable, temporarily disable that...
                   (let ((frame (selected-frame)))
                     (modify-frame-parameters frame '((unsplittable . nil)))
                     (prog1
                         (condition-case nil
                             (split-window
                              ;; Chose the last of a window group, since
                              ;; otherwise, the lowering of another window's
                              ;; TL corner would cause the logical order of
                              ;; the windows to be changed.
                              (car (last (selected-window-group)))
                              (- ispell-choices-win-default-height) 'below)
                           (error nil))
                       (modify-frame-parameters frame '((unsplittable . t))))))
              (and (not unsplittable)
                   (condition-case nil
                       (split-window
                        ;; See comment above.
                        (car (last (selected-window-group)))
                        (- ispell-choices-win-default-height) 'below)
                     (error nil)))
              (display-buffer buffer))))
    (if (not window)
        (error "Couldn't make window for *Choices*")
      (select-window window)
      (set-window-buffer window buffer)
      (set-window-point window (point-min))
      (fit-window-to-buffer window nil nil nil nil t))))
;;; }}}

(provide 'configs-languages)
;;; languages.el ends here
