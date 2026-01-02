;;; emacs/flyspell.el -*- lexical-binding: t; -*-

(use-package ispell
  :defer t
  :custom
  (text-mode-ispell-word-completion nil)
  (ispell-local-dictionary "en_US")
  (ispell-dictionary "en_US")
  (ispell-local-dictionary "en_US")
  (ispell-local-dictionary-alist
   ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
   ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
   '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
  (ispell-program-name (executable-find "aspell"))
  (ispell-extra-args '("-a" "soundslike" "--sug-mode=ultra" "--lang=en_US"))
  (ispell-silently-savep t)
  (ispell-quietly t)
  :preface
  (defun +ispell-display-buffer-override (buffer)
    "Show BUFFER in new window below selected one.
Also position fit window to BUFFER and select it."
    (let* ((unsplittable
            (cdr (assq 'unsplittable (frame-parameters (selected-frame)))))
           (window
            (or (get-buffer-window buffer)
                (and unsplittable
                     ;; If frame is unsplittable, temporarily disable that
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
  :init
  (advice-add #'ispell-display-buffer :override #'+ispell-display-buffer-override))

(use-package flyspell
  :defer t
  :custom
  (flyspell-delay 0))
