;;; editor/terminal.el -*- lexical-binding: t; -*-

(require 'util-helpers)

(use-package vterm
  :disabled t
  :custom
  (vterm-term-environment-variable "xterm-256color")
  :custom-face
  (term-color-black
   ((nil :inherit ansi-color-black :foreground unspecified :background unspecified)))
  (vterm-color-black
   ((nil :inherit term-color-black :foreground unspecified :background unspecified)))
  (term-color-blue
   ((nil :inherit ansi-color-blue :foreground unspecified :background unspecified)))
  (vterm-color-blue
   ((nil :inherit term-color-blue :foreground unspecified :background unspecified)))
  (term-color-cyan
   ((nil :inherit ansi-color-cyan :foreground unspecified :background unspecified)))
  (vterm-color-cyan
   ((nil :inherit term-color-cyan :foreground unspecified :background unspecified)))
  (term-color-green
   ((nil :inherit ansi-color-green :foreground unspecified :background unspecified)))
  (vterm-color-green
   ((nil :inherit term-color-green :foreground unspecified :background unspecified)))
  (term-color-magenta
   ((nil :inherit ansi-color-magenta :foreground unspecified :background unspecified)))
  (vterm-color-magenta
   ((nil :inherit term-color-magenta :foreground unspecified :background unspecified)))
  (term-color-red
   ((nil :inherit ansi-color-red :foreground unspecified :background unspecified)))
  (vterm-color-red
   ((nil :inherit term-color-red :foreground unspecified :background unspecified)))
  (term-color-white
   ((nil :inherit ansi-color-white :foreground unspecified :background unspecified)))
  (vterm-color-white
   ((nil :inherit term-color-white :foreground unspecified :background unspecified)))
  (term-color-yellow
   ((nil :inherit ansi-color-yellow :foreground unspecified :background unspecified)))
  (vterm-color-yellow
   ((nil :inherit term-color-yellow :foreground unspecified :background unspecified)))
  :config
  (defun +vterm--internal (pop-to-buf-fun &optional arg)
    (cl-assert vterm-buffer-name)
    (let ((buf (cond ((numberp arg)
                      (get-buffer-create (format "%s<%d>"
                                                 vterm-buffer-name
                                                 arg)))
                     ((stringp arg) (generate-new-buffer arg))
                     (arg (generate-new-buffer vterm-buffer-name))
                     (t
                      (get-buffer-create vterm-buffer-name)))))
      (cl-assert (and buf (buffer-live-p buf)))
      (with-current-buffer buf
        (unless (derived-mode-p 'vterm-mode)
          (vterm-mode)))
      (when (functionp pop-to-buf-fun)
        (funcall pop-to-buf-fun buf))
      buf))

  (advice-add #'vterm--internal :override #'+vterm--internal)

  (defun +vterm-run-command (command &rest args)
    "Open vterm and run COMMAND.
The optional ARGS are keyword arguments."
    (interactive "sEnter command: ")
    (let* ((buffer-name (or (plist-get args :title) "*vterm*"))
           (display-fn (or (plist-get args :display) #'switch-to-buffer))
           (buffer (vterm--internal display-fn buffer-name)))
      (vterm-send-string (format "exec %s" command))
      (vterm-send-return)
      buffer))

  (defun +vterm-project ()
    (interactive)
    (let* ((project (project-current))
           (project-dir (if project (project-root project) default-directory))
           (default-directory project-dir)
           (term-buffer (format "*vterm - %s*" project-dir))
           buffer)
      (if-let* ((buffer (get-buffer term-buffer)))
          (display-buffer buffer)
        (setq buffer (vterm term-buffer))
        (with-current-buffer buffer
          (add-hook 'vterm-exit-functions #'+vterm-close-window-on-exit nil t))
        )))

  (defun +vterm-close-window-on-exit (&optional buffer event)
    (when (and (buffer-live-p buffer)
               (= (length (tab-line-tabs-window-buffers)) 1))
      (if-let* ((window (get-buffer-window buffer)))
          (delete-window window)))))

(use-package ghostel
  :custom
  (ghostel-set-title-function nil))
