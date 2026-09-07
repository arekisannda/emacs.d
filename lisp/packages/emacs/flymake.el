;;; emacs/flymake.el -*- lexical-binding: t; -*-

(use-package flymake
  :defer t
  :custom
  (flymake-start-on-flymake-mode t)
  (flymake-show-diagnostics-at-end-of-line nil)
  (flymake-fringe-indicator-position nil)
  (flymake-indicator-type nil)
  :config
  (utils/custom-set-faces
   (flymake-warning
    ((nil :underline (:style wave :color ,(doom-color 'orange)))))
   )

  (defun flymake-show-buffer-diagnostics-override (&optional diagnostic)
    "Show listing of Flymake diagnostics for current buffer.
With optional DIAGNOSTIC, find and highlight this diagnostic in the
listing.

Interactively, grab DIAGNOSTIC from context.  For mouse events in
margins and fringes, use the first diagnostic in the corresponding line,
else look in the click position.  For non-mouse events, look for
diagnostics at point.

This function doesn't move point"
    (interactive
     (if (mouse-event-p last-command-event)
         (with-selected-window (posn-window (event-end last-command-event))
           (with-current-buffer (window-buffer)
             (let* ((event-point (posn-point (event-end last-command-event)))
                    (diags
                     (or
                      (flymake-diagnostics event-point)
                      (let (event-lbp event-lep)
                        (save-excursion
                          (goto-char event-point)
                          (setq event-lbp (line-beginning-position)
                                event-lep (line-end-position)))
                        (flymake-diagnostics event-lbp event-lep))))
                    (diag (car diags)))
               (unless diag
                 (error "No diagnostics here"))
               (list diag))))
       (flymake-diagnostics (point))))
    (unless flymake-mode
      (user-error "Flymake mode is not enabled in the current buffer"))
    (let* ((name (flymake--diagnostics-buffer-name))
           (source (current-buffer))
           (target (or (get-buffer name)
                       (with-current-buffer (get-buffer-create name)
                         (flymake-diagnostics-buffer-mode)
                         (current-buffer))))
           window)
      (with-current-buffer target
        (setq flymake--diagnostics-buffer-source source)
        (setq next-error-last-buffer (current-buffer))
        (revert-buffer)
        (setq window (display-buffer (current-buffer)))
        (when (and window diagnostic)
          (with-selected-window window
            (cl-loop initially (goto-char (point-min))
                     until (eobp)
                     until (eq (plist-get (tabulated-list-get-id) :diagnostic)
                               diagnostic)
                     do (forward-line)
                     finally
                     (recenter)
                     (pulse-momentary-highlight-one-line
                      (point) 'highlight)))))))

  (advice-add #'flymake-show-buffer-diagnostics :override #'flymake-show-buffer-diagnostics-override)

  (defun flymake-show-project-diagnostics-override ()
    "Show a list of Flymake diagnostics for the current project."
    (interactive)
    (let* ((prj (project-current))
           (root (project-root prj))
           (buffer (flymake--project-diagnostics-buffer root)))
      (with-current-buffer buffer
        (flymake-project-diagnostics-mode)
        (setq-local flymake--project-diagnostic-list-project prj)
        (setq next-error-last-buffer (current-buffer))
        (revert-buffer)
        (display-buffer (current-buffer)))))

  (advice-add #'flymake-show-project-diagnostics :override #'flymake-show-project-diagnostics-override)

  (defun flymake--highlight-line-wrapper (fn &rest args)
    (let ((ov (apply fn args)))
      (unless flymake-indicator-type
        (overlay-put ov 'before-string nil))))

  (advice-add #'flymake--highlight-line :around #'flymake--highlight-line-wrapper))
