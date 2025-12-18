;;; completion/embark.el -*- lexical-binding: t; -*-

(use-package embark
  :custom
  (embark-indicators '(+vertico-embark-which-key-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  :custom-face
  (embark-selected
   ((nil :inherit unspecified
         :foreground ,(doom-color 'magenta))))
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  :config
  (defvar +vertico-embark-prompter-map (make-sparse-keymap)
    "Embark completion read prompter map.")

  (defmacro +vertico-make-embark-ace-action (fn)
    `(defun ,(intern (concat "+vertico-embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "%s"
         (require 'ace-window)
         (let ((aw-dispatch-always t))
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn))))))

  (defun +vertico-embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (defun +vertico-embark-act-with-completing-read (&optional args)
    "Display embark actions in the minibuffer.
Passes on ARGS to `embark-act`"
    (interactive "P")
    (let* ((embark-prompter 'embark-completing-read-prompter)
           (_act (propertize "Act" 'face 'highlight))
           (_embark-indicator (lambda (_keymap targets) nil)))
      (embark-act args)))

  (defun +vertico-with-minibuffer-keymap (keymap)
    "Create function with minibuffer KEYMAP."
    (lambda (fn &rest args)
      (minibuffer-with-setup-hook
          (lambda ()
            (use-local-map
             (make-composed-keymap keymap (current-local-map))))
        (apply fn args))))

  (advice-add 'embark-completing-read-prompter
              :around (+vertico-with-minibuffer-keymap
                       +vertico-embark-prompter-map))

  (+vertico-make-embark-ace-action find-file)
  (+vertico-make-embark-ace-action affe-find)
  (+vertico-make-embark-ace-action switch-to-buffer)
  (+vertico-make-embark-ace-action bookmark-jump)
  (defun +vertico-embark-hide-which-key-indicator (fn &rest args)
    "Hide the `which-key` indicator after using the embark prompter.
Executes FN with ARGS."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'+vertico-embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'+vertico-embark-hide-which-key-indicator))

(use-package embark-consult :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
