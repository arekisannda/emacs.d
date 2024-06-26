;;; keybinds-custom.el --- Keybinding Functions -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'general)
(require 'hydra)

(defhydra +hydra-text-scale
  (:foreign-key exit :exit nil :timeout nil)
  "text scale"
  ("+" #'text-scale-increase "increase")
  ("-" #'text-scale-decrease "decrease"))
(hydra-set-property #'+hydra-text-scale :verbosity 0)

(defhydra +hydra-dap-motion
  (:foreign-key exit :exit nil :timeout nil)
  "dap motion"
  ("i" #'dap-step-in "step-in")
  ("o" #'dap-step-out "step-out")
  ("n" #'dap-next "next")
  ("C" #'dap-continue "continue"))

(defhydra +hydra-dap-session
  (:foreign-key exit :exit nil :timeout nil)
  "dap session"
  ("S" #'dap-ui-sessions "sessions")
  ("d" #'dap-ui-delete-session "delete session"))

(setq diff-hl-show-hunk--current-footer
      "(q)Quit  (j)Next  (k)Previous  (s)Stage  (d)Revert  (c)Copy original")

(defhydra +hydra-git-motion
  (:foreign-key exit :exit nil :timeout nil :hint nil)
  ("[" #'diff-hl-previous-hunk "prev hunk")
  ("]" #'diff-hl-next-hunk "next hunk"))
(hydra-set-property #'+hydra-git-motion :verbosity 0)

(defhydra +hydra-git-hunk
  (:foreign-key exit :exit nil :timeout nil :hint nil)
  "git"
  ("i" #'diff-hl-show-hunk :hint nil)
  ("j" #'diff-hl-show-hunk-next :hint nil)
  ("k" #'diff-hl-show-hunk-previous :hint nil)
  ("s" #'diff-hl-show-hunk-stage-hunk :hint nil)
  ("d" #'diff-hl-show-hunk-revert-hunk :hint nil)
  ("c" #'diff-hl-show-hunk-copy-original-text :hint nil))
(hydra-set-property #'+hydra-git-hunk :verbosity 0)

(defhydra +hydra-xref-motion
  (:foreign-key exit :exit nil :timeout nil :hint nil)
  ("[" #'xref-go-back "prev xref")
  ("]" #'xref-go-forward "next xref"))
(hydra-set-property #'+hydra-xref-motion :verbosity 0)

(defhydra +hydra-help-motion
  (:foreign-key exit :exit nil :timeout nil :hint nil)
  ("[" #'help-go-back "prev help")
  ("]" #'help-go-forward "next help"))
(hydra-set-property #'+hydra-help-motion :verbosity 0)

(defhydra +hydra-org-remark-motion
  (:foreign-key exit :exit nil :timeout nil :hint nil)
  ("[" #'org-remark-prev "prev remark")
  ("]" #'org-remark-next "next remark")
  ("o" #'org-remark-view "view"))
(hydra-set-property #'+hydra-org-remark-motion :verbosity 0)

(defun +keybinds--current-workspace ()
  "Return name of current perspective."
  (cond ((featurep 'persp-mode) (safe-persp-name (get-current-persp)))
        ((featurep 'perspective) (persp-name (persp-curr)))))

(defun +keybinds--emacs-doc ()
  "Goto https://emacsdocs.org."
  (interactive)
  (browse-url "https://emacsdocs.org"))

(defun +keybinds--clear-project ()
  "Clear project and reset windows."
  (interactive)
  (project-kill-buffers t)
  (delete-other-windows)
  (dashboard-open))

(defun +keybinds--clear-windows ()
  "Reset windows."
  (interactive)
  (delete-other-windows)
  (dashboard-open))

(defun +keybinds--create-fresh-tab ()
  "Create fresh tab."
  (interactive)
  (tab-bar-new-tab)
  (dashboard-open))

(defvar +keybinds--scroll-lines 5)
(defvar +keybinds--hscroll-lines 5)

(defun +keybinds--scroll-left ()
  "Scroll window left."
  (interactive)
  (evil-scroll-column-left +keybinds--hscroll-lines))

(defun +keybinds--scroll-right ()
  "Scroll window right."
  (interactive)
  (evil-scroll-column-right +keybinds--hscroll-lines))

(defun +keybinds--scroll-down ()
  "Scroll window down."
  (interactive)
  (evil-scroll-line-down +keybinds--scroll-lines))

(defun +keybinds--scroll-up ()
  "Scroll window up."
  (interactive)
  (evil-scroll-line-up +keybinds--scroll-lines))

(defun +keybinds--scroll-other-down ()
  "Scroll other window down."
  (interactive)
  (scroll-other-window +keybinds--scroll-lines))

(defun +keybinds--scroll-other-up ()
  "Scroll other window up."
  (interactive)
  (scroll-other-window-down +keybinds--scroll-lines))

(defun +keybinds--minibuffer-scroll-other-down ()
  "Scroll other window down."
  (interactive)
  (minibuffer-scroll-other-window +keybinds--scroll-lines))

(defun +keybinds--minibuffer-scroll-other-up ()
  "Scroll other window up."
  (interactive)
  (minibuffer-scroll-other-window-down +keybinds--scroll-lines))

(eval-when-compile
  (defmacro +keybinds--one-window-tab-bar-close-tab (fn)
    `(lambda ()
       (interactive)
       (if (one-window-p)
           (tab-bar-close-tab)
         (funcall #',fn))))

  (defmacro +keybinds--split-focus-other-window (splitfn)
    `(lambda ()
       (interactive)
       (funcall #',splitfn)
       (other-window 1)))

  (defmacro +keybinds--org-agenda (key)
    `(defun ,(intern (concat "+org-agenda-list-key-" key)) ()
       (interactive)
       (org-agenda nil ,key)))

  (defmacro +keybinds--scroll-line-to (ppt)
    `(defun ,(intern (format "+keybinds--scroll-line-to-%d-ppt" ppt)) ()
       ,(format "Scroll current to %d%% of window" ppt)
       (interactive)
       (when-let* ((buffer-height (count-lines (point-min) (point-max)))
                   (_ (> buffer-height (window-height)))
                   (window-ppt (/ ,ppt 100.0))
                   (window-start-line (line-number-at-pos (window-start)))
                   (target-line (+ window-start-line (ceiling (* (window-height) window-ppt))))
                   (current-line (line-number-at-pos))
                   (scroll-count (- target-line current-line)))
         (scroll-down-line scroll-count))))
  )

(defun +keybinds--org-scratch-buffer ()
  "Open a new scratch buffer in Org mode."
  (interactive)
  (let ((buffer (get-buffer-create "*org-scratch*")))
    (with-current-buffer buffer
      (org-mode))
    (display-buffer buffer)))

(defun +keybinds--select-mru-main-window ()
  "Select most recently used main window."
  (interactive)
  (select-window (util/window-get-mru-in-main)))

(defun +embark-clone-indirect-buffer (buffer)
  "Embark clone BUFFER."
  (interactive "sClone buffer: ")
  (with-demoted-errors "%s"
    (with-current-buffer (get-buffer buffer)
      (call-interactively #'clone-indirect-buffer))))

(provide 'keybinds-custom)

;;; keybinds-custom.el ends here
