;;; emacs/calc.el -*- lexical-binding: t; -*-

(use-package calc
  :defer t
  :init
  (setq calc-display-trail t
        calc-show-banner nil)
  :config
  (defun calc-trail-display-override (flag &optional no-refresh interactive)
    (interactive "P\ni\np")
    (let ((win (get-buffer-window (calc-trail-buffer))))
      (if (setq calc-display-trail
                (not (if flag (memq flag '(nil 0)) win)))
          (if (null win)
              (progn
                (if calc-trail-window-hook
                    (run-hooks 'calc-trail-window-hook)
                  (let ((w (display-buffer calc-trail-buffer)))
                    (and calc-make-windows-dedicated
                         (set-window-dedicated-p w t))))
                (calc-wrapper
                 (setq overlay-arrow-string calc-trail-overlay
                       overlay-arrow-position calc-trail-pointer)
                 (or no-refresh
                     (if interactive
                         (calc-do-refresh)
                       (calc-refresh))))))
        (if win
            (progn
              (delete-window win)
              (calc-wrapper
               (or no-refresh
                   (if interactive
                       (calc-do-refresh)
                     (calc-refresh))))))))
    calc-trail-buffer)

  (advice-add #'calc-trail-display :override #'calc-trail-display-override))
