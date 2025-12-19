;;; lang/scad.el -*- lexical-binding: t; -*-

(use-package scad-mode
  :config
  (defun +scad-preview-start ()
    (interactive)
    (unless (derived-mode-p 'scad-mode)
      (user-error "scad-mode unsupported buffer"))
    (setq-local +scad-preview-layout-state (window-state-get (frame-root-window) t))
    (delete-other-windows)
    (split-window-horizontally)
    (scad-preview)

    (let (scad-source-buffer
          scad-preview-buffer)
      (with-current-buffer (current-buffer)
        (cond
         ((derived-mode-p 'scad-preview-mode) (setq scad-source-buffer scad--preview-buffer
                                                    scad-preview-buffer (current-buffer)))
         ((derived-mode-p 'scad-mode) (setq scad-preview-buffer scad--preview-buffer
                                            scad-source-buffer (current-buffer)))
         ))
      (select-window (get-buffer-window scad-source-buffer))
      ))

  (defun +scad-preview-quit ()
    (interactive)
    (unless (derived-mode-p '(scad-mode scad-preview-mode))
      (user-error "scad-mode unsupported buffer"))
    (let (scad-source-buffer
          scad-preview-buffer)
      (with-current-buffer (current-buffer)
        (cond
         ((derived-mode-p 'scad-preview-mode) (setq scad-source-buffer scad--preview-buffer
                                                    scad-preview-buffer (current-buffer)))
         ((derived-mode-p 'scad-mode) (setq scad-preview-buffer scad--preview-buffer
                                            scad-source-buffer (current-buffer)))
         ))
      (quit-window t (get-buffer-window (get-buffer scad-preview-buffer)))
      (with-current-buffer scad-source-buffer
        (unless (buffer-live-p scad--preview-buffer)
          (setq-local scad--preview-buffer nil))
        (window-state-put +scad-preview-layout-state (frame-root-window) 'safe)))
    )

  (defun +scad-preview-toggle (&optional arg)
    (interactive "p")
    (pcase arg
      (4 (+scad-preview-quit))
      (_ (if scad--preview-buffer
             (+scad-preview-quit)
           (+scad-preview-start)))
      ))
  )


(use-package scad-dbus :after scad-mode :defer t)
