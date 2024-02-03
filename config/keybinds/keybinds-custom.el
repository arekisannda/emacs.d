;;; keybinds-custom.el --- Keybinding Functions -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'packages-init)

(defhydra hydra-dap-motion
  (:foreign-key exit :exit nil :timeout nil)
  "dap motion"
  ("i" #'dap-step-in "step-in")
  ("o" #'dap-step-out "step-out")
  ("n" #'dap-next "next")
  ("C" #'dap-continue "continue"))

(defhydra hydra-dap-session
  (:foreign-key exit :exit nil :timeout nil)
  "dap session"
  ("S" #'dap-ui-sessions "sessions")
  ("d" #'dap-ui-delete-session "delete session"))

(setq diff-hl-show-hunk--current-footer
      "(q)Quit  (j)Next  (k)Previous  (s)Stage  (d)Revert  (c)Copy original")

(defhydra hydra-git-motion
  (:foreign-key exit :exit nil :timeout nil :hint nil)
  ("[" #'diff-hl-previous-hunk "prev hunk")
  ("]" #'diff-hl-next-hunk "next hunk"))
(hydra-set-property #'hydra-git-motion :verbosity 0)

(defhydra hydra-git-hunk
  (:foreign-key exit :exit nil :timeout nil :hint nil)
  "git"
  ("i" #'diff-hl-show-hunk :hint nil)
  ("j" #'diff-hl-show-hunk-next :hint nil)
  ("k" #'diff-hl-show-hunk-previous :hint nil)
  ("s" #'diff-hl-show-hunk-stage-hunk :hint nil)
  ("d" #'diff-hl-show-hunk-revert-hunk :hint nil)
  ("c" #'diff-hl-show-hunk-copy-original-text :hint nil))
(hydra-set-property #'hydra-git-hunk :verbosity 0)

(defun keybinds/custom--emacs-doc ()
  (interactive)
  (browse-url "https://emacsdocs.org"))

(defun keybinds/custom--clear-project ()
  "Clear project and reset windows."
  (interactive)
  (project-kill-buffers t)
  (delete-other-windows)
  (dashboard-open))

(defun keybinds/custom--clear-windows ()
  "Reset windows."
  (interactive)
  (delete-other-windows)
  (dashboard-open))

(defun keybinds/custom--create-fresh-tab ()
  "Create fresh tab."
  (interactive)
  (tab-bar-new-tab)
  (dashboard-open))

(eval-when-compile
  (defmacro keybinds/custom--one-window-tab-bar-close-tab (fn)
    `(lambda ()
       (interactive)
       (if (one-window-p)
           (tab-bar-close-tab)
         (funcall #',fn))))

  (defmacro keybinds/custom--split-focus-other-window (splitfn)
    `(lambda ()
       (interactive)
       (funcall #',splitfn)
       (other-window 1)))

  (defmacro keybinds/custom--scroll-column (n)
    `(lambda ()
       (interactive)
       (evil-scroll-column-right ,n)))

  (defmacro keybinds/custom--scroll-line (n)
    `(lambda ()
       (interactive)
       (evil-scroll-line-down ,n)))

  (defmacro keybinds/custom--scroll-other-window (n)
    `(lambda ()
       (interactive)
       (if (minibuffer-window-active-p (selected-window))
	   (minibuffer-scroll-other-window ,n)
	 (scroll-other-window ,n))))
  )

(provide 'keybinds-custom)

;;; keybinds-custom.el ends here
