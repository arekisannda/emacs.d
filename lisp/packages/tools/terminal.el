;;; editor/terminal.el -*- lexical-binding: t; -*-

(use-package vterm
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
           (term-buffer (format "*vterm - %s*" project-dir)))
      (if-let ((buffer (get-buffer term-buffer)))
          (display-buffer buffer)
        (vterm term-buffer))
      ))
  )
