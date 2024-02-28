;;; packages-ispell.el --- Emacs Ispell Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package emacs
  :ensure nil
  :config
  (require 'ispell)

  (defvar packages/ispell--check-backend nil)

  (setq-default ispell-complete-word-dict
                (cond ((file-readable-p "/usr/dict/web2") "/usr/dict/web2")
                      ((file-readable-p "/usr/share/dict/web2") "/usr/share/dict/web2")
                      ((file-readable-p "/usr/dict/words") "/usr/dict/words")
                      ((file-readable-p "/usr/lib/dict/words") "/usr/lib/dict/words")
                      ((file-readable-p "/usr/share/dict/words") "/usr/share/dict/words")
                      ((file-readable-p "/usr/share/lib/dict/words")
                       "/usr/share/lib/dict/words")
                      ((file-readable-p "/sys/dict") "/sys/dict")))

  (defun packages/ispell--load-hunspell-configs ()
    "Load hunspell spelling backend."
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
      (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist))
    (setq packages/ispell--check-backend "hunspell"))

  (defun packages/ispell--load-aspell-configs ()
    "Load aspell spelling backend."
    (setq ispell-program-name "/usr/bin/aspell")
    ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
    (setq ispell-extra-args '("-a" "soundslike" "--sug-mode=ultra" "--lang=en_US"))
    (setq packages/ispell--check-backend "aspell"))

  (cond ((executable-find "hunspell") (packages/ispell--load-hunspell-configs))
        ((executable-find "aspell") (packages/ispell--load-aspell-configs)))

  (defun packages/ispell-display-buffer (buffer)
    "Show BUFFER in new window below selected one.
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

  (advice-add #'ispell-display-buffer :override #'packages/ispell-display-buffer))

(provide 'packages-ispell)

;;; packages-ispell.el ends here
