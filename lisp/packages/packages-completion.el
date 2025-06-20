;;; packages-completion.el --- Completion Tools Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'keymap)
(require 'cl-seq)
(require 'util-helpers)

(use-package corfu
  :init
  (setq corfu-map (make-sparse-keymap)
        corfu-popupinfo-map (make-sparse-keymap))
  :custom
  (completion-auto-help 'always)
  (corfu-preselect 'first)
  (corfu-preview-current nil)
  (corfu-on-exact-match nil)
  (corfu-sort-override-function
   (lambda (candidates)
     "Yasnippet candidates first"
     (sort candidates
           (lambda (x y)
             (and (< (length x) (length y) )
                  (get-text-property 0 'yas-annotation x))))
     candidates))
  (corfu-cycle nil)
  (corfu-auto t)
  (corfu-auto-delay 0.3)
  (corfu-popupinfo-delay (cons nil 0.5))
  (corfu-min-width 40)
  (corfu-max-width 100)
  (corfu-left-margin-width 1.0)
  (corfu-right-margin-width 1.0)
  (corfu-scroll-margin 2)
  (corfu-bar-width 0.5)
  (completion-cycle-threshold nil)
  (tab-always-indent nil)
  :config
  (defun +corfu-minibuffer-completion-setup ()
    "Setup to run for minibuffer mode."
    (shut-up
      (unless (or (bound-and-true-p mct--active)
                  (bound-and-true-p vertico--input)
                  (eq (current-local-map) read-passwd-map))
        (when (local-variable-p 'completion-at-point-functions)
          (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                      corfu-auto t
                      corfu-cycle nil
                      corfu-popupinfo-delay (cons nil 0.5)
                      corfu-left-margin-width 1.0
                      corfu-right-margin-width 1.0
                      corfu-bar-width 0.5
                      corfu-min-width 40))
        (setq-local completion-cycle-threshold nil)
        (setq-local tab-always-indent nil)
        (corfu-mode 1))))

  (defcustom +corfu-exact-match--auto-insert nil
    "Enable auto-insert of exact match."
    :type 'boolean)

  (defun +corfu--in-region-1 (beg end table &optional pred)
    "Complete in region, see `completion-in-region' for BEG, END, TABLE, PRED."
    (barf-if-buffer-read-only)
    ;; Restart the completion. This can happen for example if C-M-/
    ;; (`dabbrev-completion') is pressed while the Corfu popup is already open.
    (when completion-in-region-mode (corfu-quit))
    (let* ((pt (max 0 (- (point) beg)))
           (str (buffer-substring-no-properties beg end))
           (metadata (completion-metadata (substring str 0 pt) table pred))
           (threshold (completion--cycle-threshold metadata))
           (completion-in-region-mode-predicate
            (or completion-in-region-mode-predicate #'always)))
      (pcase (completion-try-completion str table pred pt metadata)
        ('nil (corfu--message "No match") nil)
        ('t (goto-char end)
            (corfu--message "Sole match")
            (if (eq corfu-on-exact-match 'show)
                (corfu--setup beg end table pred)
              (corfu--exit-function
               str 'finished
               (alist-get 'corfu--candidates (corfu--recompute str pt table pred))))
            t)
        (`(,newstr . ,newpt)
         (setq beg (if (markerp beg) beg (copy-marker beg))
               end (copy-marker end t))
         (when +corfu-exact-match--auto-insert
           (corfu--replace beg end newstr)
           (goto-char (+ beg newpt)))
         (let* ((state (corfu--recompute newstr newpt table pred))
                (base (alist-get 'corfu--base state))
                (total (alist-get 'corfu--total state))
                (cands (alist-get 'corfu--candidates state)))
           (cond
            ((<= total 1)
             ;; If completion is finished and cannot be extended further and
             ;; `corfu-on-exact-match' is not 'show, return 'finished.  Otherwise
             ;; setup the popup.
             (if (and (= total 1)
                      (or (eq corfu-on-exact-match 'show)
                          (consp (completion-try-completion
                                  newstr table pred newpt
                                  (completion-metadata newstr table pred)))))
                 (corfu--setup beg end table pred)
               (corfu--exit-function newstr 'finished cands)))
            ;; Too many candidates for cycling -> Setup popup.
            ((or (not threshold) (and (not (eq threshold t)) (< threshold total)))
             (corfu--setup beg end table pred))
            (t
             ;; Cycle through candidates.
             (corfu--cycle-candidates total cands (+ (length base) beg) end)
             ;; Do not show Corfu when completion is finished after the candidate.
             (unless (equal (completion-boundaries (car cands) table pred "") '(0 . 0))
               (corfu--setup beg end table pred)))))
         t))))

  (advice-add #'corfu--in-region-1 :override #'+corfu--in-region-1)

  (advice-add #'completion-preview-insert :after #'corfu-quit)
  :hook
  (window-setup . global-corfu-mode)
  (global-corfu-mode . corfu-popupinfo-mode)
  (corfu-mode . completion-preview-mode)
  (minibuffer-setup . +corfu-minibuffer-completion-setup))

(use-package emacs
  :ensure nil
  :custom-face
  (completion-preview
   ((nil :inherit nil
         :foreground ,(doom-darken (doom-color 'yellow) 0.2))))
  (completion-preview-exact
   ((nil :inherit completion-preview-common
         :underline (:color ,(doom-darken (doom-color 'yellow) 0.2)
                            :style line
                            :position nil)))))

(use-package nerd-icons-corfu :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package corfu-terminal :disabled)

(use-package cape
  :config
  (require 'cape-char)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (plist-put cape--tex-properties :exit-function nil)
  :hook
  (eglot-managed-mode
   . (lambda ()
       (setq-local completion-at-point-functions
                   (list
                    (cape-capf-super
                     #'eglot-completion-at-point
                     #'yasnippet-capf)))
       (add-to-list 'completion-at-point-functions #'cape-file))))

(use-package yasnippet-capf :after yasnippet
  :custom
  (yasnippet-capf-lookup-by 'key))

(provide 'packages-completion)

;;; packages-completion.el ends here
