;;; ui-tab-bar.el --- Emacs Tab-Bar Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'tab-bar)
(require 'project)
(require 'packages-init)
(require 'ui-fonts)
(require 'util-strings)

(defvar ui/tab-bar--project-bar-location 'tab-bar-format-align-right)
(defvar ui/tab-bar--project-bar-temporary-bar nil)
(defvar ui/tab-bar--project-bar-previous-format nil)
(defvar ui/tab-bar--project-bar-default "--")
(defvar ui/tab-bar--project-bar-project nil)

(defun ui/tab-bar--active-frame-p ()
  "Return t if active frame."
  (cond ((boundp 'moody--active-window)
         (eq (window-frame) (window-frame moody--active-window)))
        ((boundp 'powerline-selected-window)
         (eq (window-frame) (window-frame powerline-selected-window)))
        (t t)))

(defun ui/tab-bar--project-bar-update (&optional project-name)
  "Update `ui/tab-bar--project-bar-project` value with PROJECT-NAME."
  (setq ui/tab-bar--project-bar-project ui/tab-bar--project-bar-default)
  (unless (util/strings-blank-or-nil-p project-name)
    (if (file-directory-p project-name)
        (setq ui/tab-bar--project-bar-project (expand-file-name project-name))))
  ui/tab-bar--project-bar-project)

(defun ui/tab-bar-project-bar-project ()
  "Get current tab-bar project name."
  (interactive)
  (if (util/strings-blank-or-nil-p ui/tab-bar--project-bar-project)
      ui/tab-bar--project-bar-default
    (util/strings-remove-suffix
     (abbreviate-file-name ui/tab-bar--project-bar-project)
     "/")))

(defun ui/tab-bar-project-string ()
  "Return tab bar project string."
  (concat
   (propertize (if init-file-debug "[DEBUG]" "")
               'face `(nil :inherit ui/colors-red-d
                           :box nil
                           :weight normal
                           :height ,ui/fonts-fixed-pitch-size))
   (propertize (format "[%s]" persp-last-persp-name)
               'face `(nil :inherit ui/colors-orange-d
                           :box nil
                           :weight normal
                           :height ,ui/fonts-fixed-pitch-size))
   (propertize (format "[%s] " (ui/tab-bar-project-bar-project))
               'face `(nil :inherit ui/colors-green-d
                           :box nil
                           :weight normal
                           :height ,ui/fonts-fixed-pitch-size))))

(defun ui/tab-bar-project-bar ()
  "Produce project information for the tab bar."
  (and ui/tab-bar-project-bar-mode
       (ui/tab-bar--active-frame-p)
       (ui/tab-bar-project-string)))

(define-minor-mode ui/tab-bar-project-bar-mode
  "Show current command and its key binding in the tab bar."
  :global t
  (when (< emacs-major-version 28)
    (user-error "`ui/tab-bar-project-bar-mode' requires Emacs 28.1"))
  (cond
   (ui/tab-bar-project-bar-mode
    (unless tab-bar-mode
      (setq ui/tab-bar--project-bar-temporary-bar t)
      (tab-bar-mode 1))
    (cl-case ui/tab-bar--project-bar-location
      (replace
       (setq ui/tab-bar--project-bar-previous-format tab-bar-format)
       (setq tab-bar-format (list 'ui/tab-bar-project-bar)))
      (beginning
       (setq tab-bar-format (cons 'ui/tab-bar-project-bar tab-bar-format)))
      (end
       (setq tab-bar-format (nconc tab-bar-format (list 'ui/tab-bar-project-bar))))
      (t
       (let ((mem (memq ui/tab-bar--project-bar-location tab-bar-format)))
         (if mem
             (setcdr mem (cl-pushnew 'ui/tab-bar-project-bar (cdr mem)))
           (setq tab-bar-format
                 (nconc tab-bar-format
                        (if (eq ui/tab-bar--project-bar-location
                                'tab-bar-format-align-right)
                            (list 'tab-bar-format-align-right
                                  'ui/tab-bar-project-bar)
                          (message "%s not found in %s; adding at end instead"
                                   ui/tab-bar--project-bar-location 'tab-bar-format)
                          (list 'ui/tab-bar-project-bar)))))))))
   (t
    (when ui/tab-bar--project-bar-temporary-bar
      (setq ui/tab-bar--temporary-tab-bar nil)
      (tab-bar-mode -1))
    (cond (ui/tab-bar--project-bar-previous-format
           (setq tab-bar-format ui/tab-bar--project-bar-previous-format)
           (setq ui/tab-bar--project-bar-previous-format nil))
          (t
           (setq tab-bar-format (delq 'ui/tab-bar-project-bar tab-bar-format)))))))

(defun ui/tab-bar--persp-mode-setup ()
  "Set up tab-bar `persp-mode` integration."
  (add-hook 'management/workspace-project-update-hook #'ui/tab-bar--project-bar-update)
  (add-hook 'management/workspace-project-clear-hook #'ui/tab-bar--project-bar-update))

(defun ui/tab-bar-setup ()
  "Set up tab-bar configurations."
  (ui/tab-bar--persp-mode-setup)
  (ui/tab-bar-project-bar-mode)

  (setq-default tab-bar-close-button-show nil)
  (setq-default tab-bar-new-button-show nil)
  (setq-default tab-bar-auto-width-max '(150 20))
  (setq-default tab-bar-auto-width-min '(20 2)))

(provide 'ui-tab-bar)

;;; ui-tab-bar.el ends here
