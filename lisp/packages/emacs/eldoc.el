;;; emacs/eldoc.el -*- lexical-binding: t; -*-

(use-package eldoc
  :defer t
  :init
  (setq-default eldoc-display-functions '(eldoc-display-in-buffer))
  (defvar-local +eldoc--old-display-functions nil)

  (defun +eldoc--disable ()
    (setq-local +eldoc--old-display-functions eldoc-display-functions
                eldoc-display-functions nil))

  (defun +eldoc--enable ()
    (setq-local eldoc-display-functions +eldoc--old-display-functions
                +eldoc--old-display-functions nil)))

(use-package eldoc-box :after (eldoc windex-scroll)
  :preface
  (defun +eldoc-box-max-width ()
    (let ((max-width 800)
          (set-width (ceiling (* (frame-pixel-width) 0.3))))
      (if (> set-width max-width) max-width set-width)))

  (defun +eldoc-box-max-height ()
    (let ((max-height 600)
          (set-height (ceiling (* (frame-pixel-height) 0.3))))
      (if (> set-height max-height) max-height set-height)))
  :custom-face
  (eldoc-box-body
   ((nil :inherit default
         :background ,(doom-color 'bg-alt))))
  (eldoc-box-border
   ((nil :inherit popup-border
         :background unspecified
         :foreground unspecified)))
  (eldoc-box-markdown-separator
   ((nil :foreground "#323232")))
  :custom
  (eldoc-box-max-pixel-width #'+eldoc-box-max-width)
  (eldoc-box-max-pixel-height #'+eldoc-box-max-height)
  :config
  (add-to-list 'eldoc-box-self-insert-command-list 'eldoc-box-scroll-up)
  (add-to-list 'eldoc-box-self-insert-command-list 'eldoc-box-scroll-down)
  (add-to-list 'eldoc-box-self-insert-command-list 'eldoc-box-scroll-left)
  (add-to-list 'eldoc-box-self-insert-command-list 'eldoc-box-scroll-right)

  (defmacro create-eldoc-scroll-defun (direction)
    (let ((dir (symbol-name direction)))
      `(defun ,(intern (format "eldoc-box-scroll-%s" dir)) ()
         ,(format "Scroll %s in `eldoc-box` frame." dir)
         (interactive)
         (if eldoc-box--frame
             (windex-with-selector eldoc-box--frame nil
               (,(intern (concat "windex-scroll-" dir)))
               )))))

  (create-eldoc-scroll-defun up)
  (create-eldoc-scroll-defun down)
  (create-eldoc-scroll-defun right)
  (create-eldoc-scroll-defun left)

  (defun eldoc-box--enable ()
    "Enable eldoc-box hover.
Intended for internal use."
    (if (not (boundp 'eldoc-display-functions))
        (add-function :before-while (local 'eldoc-message-function)
                      #'eldoc-box--eldoc-message-function)

      (setq-local eldoc-box--old-eldoc-functions
                  eldoc-display-functions)
      (remove-hook 'eldoc-display-functions #'eldoc-display-in-echo-area t)
      (add-hook 'eldoc-display-functions #'eldoc-box--eldoc-display-function -90 t))

    (when eldoc-box-clear-with-C-g
      (advice-add #'keyboard-quit :before #'eldoc-box-quit-frame)))

  (defun +eldoc-doc-buffer (&optional interactive)
    "Get or display ElDoc documentation buffer.

The buffer holds the results of the last documentation request.
If INTERACTIVE, display it.  Else, return said buffer."
    (interactive (list t))
    (unless (buffer-live-p eldoc--doc-buffer)
      (user-error (format
                   "ElDoc buffer doesn't exist, maybe `%s' to produce one."
                   (substitute-command-keys "\\[eldoc]"))))
    (let ((buf-name "*eldoc info*")
          (buf))
      (if (setq buf (get-buffer buf-name))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (replace-buffer-contents  eldoc--doc-buffer)))
        (with-current-buffer eldoc--doc-buffer
          (setq buf (clone-buffer buf-name t))))
      (with-current-buffer buf
        (setq-local truncate-lines t)
        (visual-fill-column-mode -1)
        (visual-line-mode -1)
        (rename-buffer buf-name)
        (display-buffer (current-buffer)))))

  (defun eldoc-display-in-buffer (docs interactive)
    "Display DOCS in a dedicated buffer.
If INTERACTIVE is t, also display the buffer."

    (eldoc--format-doc-buffer docs)
    (when interactive (+eldoc-doc-buffer nil)))

  (defun +eldoc-close-buffer ()
    "Helper function to kill Eldoc doc buffer."
    (interactive)
    (let (window)
      (when (and (buffer-live-p eldoc--doc-buffer)
                 (setq window (get-buffer-window eldoc--doc-buffer)))
        (quit-window t window))))
  :hook
  (eldoc-mode . eldoc-box-hover-at-point-mode))
