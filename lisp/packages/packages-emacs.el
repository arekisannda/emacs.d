;;; packages-emacs.el --- Emacs Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-helpers)

(use-package emacs
  :ensure nil
  ;;:diminish auto-revert-mode
  :preface
  (defun +emacs-tuning-configurations ()
    ;; performance tuning
    (defvar packages/emacs-gc-cons-threshold (* 1024 1024 100))
    (setq-default gc-cons-threshold packages/emacs-gc-cons-threshold)
    (setq-default read-process-output-max (* 1024 1024)))

  (defcustom +emacs-read-only-rules
    (list (format "^%s" (expand-file-name elpaca-directory))
          (format "^%s" (expand-file-name package-user-dir))
          "^/usr/share/emacs/")
    "List of read-only file prefixes."
    :group 'convenience
    :type '(list :element-type string))

  (defun +emacs-set-read-only ()
    (read-only-mode 1)
    (evil-motion-state t))

  (defun +emacs-set-read-only-by-rules ()
    "Enable `read-only-mode` if buffer matches one of `+emacs-read-only-rules`."
    (when (and buffer-file-name
               (cl-loop for rule in +emacs-read-only-rules
                        thereis (string-match-p rule buffer-file-name)))
      (+emacs-set-read-only)))

  (defun +emacs-minibuffer-setup ()
    (setq gc-cons-threshold most-positive-fixnum)
    (marginalia-mode +1))

  (defun +emacs-minibuffer-exit ()
    (marginalia-mode -1)
    (setq gc-cons-threshold packages/emacs-gc-cons-threshold))

  (defun +emacs-create-directory-on-save ()
    (when buffer-file-name
      (let ((dir (file-name-directory buffer-file-name)))
        (when (and (not (file-exists-p dir))
                   (y-or-n-p (format "Directory %s does not exist; Create it?" dir)))
          (make-directory dir t)))))

  (defun +emacs-message-buffer-setup ()
    (visual-line-mode t)
    (follow-mode t))

  (defun +emacs-open-docs ()
    "Goto https://emacsdocs.org."
    (interactive)
    (browse-url "https://emacsdocs.org"))

  (defun +emacs-ro-clone-indirect-buffer ()
    (interactive)
    (let ((clone (call-interactively #'clone-indirect-buffer)))
      (with-current-buffer clone
        (+emacs-set-read-only))))

  (defun +embark-clone-indirect-buffer (buffer)
    "Embark clone BUFFER."
    (interactive "sClone buffer: ")
    (with-demoted-errors "%s"
      (with-current-buffer (get-buffer buffer)
        (call-interactively #'+emacs-ro-clone-indirect-buffer))))

  (defun +emacs-dedicated-frame-exit-after (&rest r)
    (when (frame-parameter nil '+dedicated-frame) (delete-frame nil nil)))
  (advice-add #'kill-current-buffer  :after #'+emacs-dedicated-frame-exit-after)
  (advice-add #'org-capture-finalize :after #'+emacs-dedicated-frame-exit-after)
  (advice-add #'org-capture-kill     :after #'+emacs-dedicated-frame-exit-after)

  (defun +emacs-edit-keybinds ()
    (interactive)
    (find-file (expand-file-name "keybinds.org" user-emacs-directory)))

  (defun +emacs-load-keybinds ()
    (org-babel-load-file (expand-file-name "keybinds.org" user-emacs-directory)))

  (defun +emacs-refresh-messages-buffer ()
    (kill-buffer (messages-buffer)))

  :custom
  (read-quoted-char-radix 16)
  (display-line-numbers-type 'relative)
  (minibuffer-message-clear-timeout 0)
  (+emacs-read-only-rules
   (append +emacs-read-only-rules
           '("/node_modules/"
             "/vendor/")))
  :hook
  (find-file . +emacs-set-read-only-by-rules)
  (messages-buffer-mode . +emacs-message-buffer-setup)
  (minibuffer-setup . +emacs-minibuffer-setup)
  (minibuffer-exit . +emacs-minibuffer-exit)
  (emacs-startup . +emacs-load-keybinds)
  (emacs-startup . +emacs-refresh-messages-buffer)
  (elpaca-after-init . +emacs-tuning-configurations)
  (before-save . +emacs-create-directory-on-save))

(provide 'packages-emacs)

;;; packages-emacs.el ends here
