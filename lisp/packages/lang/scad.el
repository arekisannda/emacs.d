;;; lang/scad.el -*- lexical-binding: t; -*-

(use-package scad-mode
  :custom
  (scad-preview-colorscheme '("Monokai Pro" . "Monokai Pro"))
  (scad-preview-camera '(0 0 0 45 0 45 400))
  (scad-extra-args '("--enable=manifold"))
  (scad-preview-view '("wireframe" "edges" "scales"))
  :config
  (defun +scad--preview-render (&optional _)
    "Render image from current buffer."
    (if (not (buffer-live-p scad--preview-buffer))
        (scad--preview-status "Dead")
      (scad--preview-kill)
      (scad--preview-status "Render")
      (let* ((infile (make-temp-file "scad-preview-" nil ".scad"))
             (basefile (file-name-sans-extension infile))
             (outfile (concat basefile ".tmp.png"))
             (buffer (current-buffer))
             (win (or (get-buffer-window buffer)
                      (display-buffer buffer nil))))
        (with-current-buffer scad--preview-buffer
          (save-restriction
            (widen)
            (write-region (point-min) (point-max) infile nil 'nomsg)))
        (with-environment-variables
            ;; Setting the OPENSCADPATH to the current directory allows openscad to pick
            ;; up other local files with `include <file.scad>'.
            (("OPENSCADPATH"
              (if-let* ((path (getenv "OPENSCADPATH")))
                  (concat default-directory path-separator path)
                default-directory)))
          (setq scad--preview-proc
                (make-process
                 :noquery t
                 :connection-type 'pipe
                 :name "scad-preview"
                 :buffer "*scad preview output*"
                 :sentinel
                 (lambda (proc _event)
                   (unwind-protect
                       (when (and (buffer-live-p buffer)
                                  (memq (process-status proc) '(exit signal)))
                         (with-current-buffer buffer
                           (setq scad--preview-proc nil)
                           (if (not (ignore-errors
                                      (and (file-exists-p outfile)
                                           (> (file-attribute-size (file-attributes outfile)) 0))))
                               (scad--preview-status "Error")
                             (with-silent-modifications
                               (scad--preview-delete)
                               (setq scad--preview-image (concat basefile ".png"))
                               (rename-file outfile scad--preview-image)
                               (erase-buffer)
                               (insert (propertize "#" 'display `(image :type png :file ,scad--preview-image))))
                             (scad--preview-status "Done"))))
                     (delete-file infile)
                     (delete-file outfile)))
                 :command
                 (append
                  (list scad-command
                        "-o" outfile
                        "--preview=throwntogether"
                        (format "--projection=%s" scad-preview-projection)
                        (format "--imgsize=%d,%d"
                                (window-pixel-width win)
                                (window-pixel-height win))
                        (format "--view=%s"
                                (mapconcat #'identity scad-preview-view ","))
                        (format "--camera=%s"
                                (mapconcat #'number-to-string scad-preview-camera ","))
                        (format "--colorscheme=%s" (scad--preview-colorscheme))
                        infile)
                  scad-extra-args)))))))

  (advice-add #'scad--preview-render :override #'+scad--preview-render)

  (defun +scad-preview-view-all ()
    (interactive nil scad-preview-mode)
    (setq-local scad-extra-args
                (if (member "--viewall" scad-extra-args)
                    (remove "--viewall" scad-extra-args)
                  (add-to-list 'scad-extra-args "--viewall")))
    (scad--preview-render))

  (defmacro +scad-preview-view-toggle (element)
    (let ((ele (or (and (symbolp element) (symbol-name element))
                   (and (stringp element) element))))
      `(progn
         (setq-local scad-preview-view
                     (if (member ,ele scad-preview-view)
                         (remove ,ele scad-preview-view)
                       (add-to-list 'scad-preview-view ,ele)))
         (scad--preview-render))))

  (defun +scad-preview-axes ()
    "Toggle axes."
    (interactive nil scad-preview-mode)
    (+scad-preview-view-toggle axes))

  (defun +scad-preview-edges ()
    "Toggle edges."
    (interactive nil scad-preview-mode)
    (+scad-preview-view-toggle edges))

  (defun +scad-preview-scales ()
    "Toggle edges."
    (interactive nil scad-preview-mode)
    (+scad-preview-view-toggle scales))

  (defun +scad-preview-start ()
    (interactive nil 'scad-mode)
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
    (interactive nil 'scad-mode 'scad-preview-mode)
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
    (interactive nil 'scad-mode 'scad-preview-mode)
    (pcase arg
      (4 (+scad-preview-quit))
      (_ (if (and scad--preview-buffer
                  (window-live-p (get-buffer-window scad--preview-buffer)))
             (+scad-preview-quit)
           (+scad-preview-start)))
      ))
  )

(use-package scad-dbus :after scad-mode :defer t)
