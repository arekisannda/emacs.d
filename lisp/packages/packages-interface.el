;;; packages-interface.el --- Interface Packages Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-helpers)

;; posframe handlers
;; 1.  `posframe-poshandler-frame-center'
;; 2.  `posframe-poshandler-frame-top-center'
;; 3.  `posframe-poshandler-frame-top-left-corner'
;; 4.  `posframe-poshandler-frame-top-right-corner'
;; 5.  `posframe-poshandler-frame-top-left-or-right-other-corner'
;; 6.  `posframe-poshandler-frame-bottom-center'
;; 7.  `posframe-poshandler-frame-bottom-left-corner'
;; 8.  `posframe-poshandler-frame-bottom-right-corner'
;; 9.  `posframe-poshandler-window-center'
;; 10. `posframe-poshandler-window-top-center'
;; 11. `posframe-poshandler-window-top-left-corner'
;; 12. `posframe-poshandler-window-top-right-corner'
;; 13. `posframe-poshandler-window-bottom-center'
;; 14. `posframe-poshandler-window-bottom-left-corner'
;; 15. `posframe-poshandler-window-bottom-right-corner'
;; 16. `posframe-poshandler-point-top-left-corner'
;; 17. `posframe-poshandler-point-bottom-left-corner'
;; 18. `posframe-poshandler-point-bottom-left-corner-upward'
;; 19. `posframe-poshandler-point-window-center'
;; 20. `posframe-poshandler-point-frame-center'
(use-package posframe)

(use-package transient-posframe
  :custom
  (transient-posframe-poshandler 'posframe-poshandler-window-center)
  :config
  (transient-posframe-mode t))

(use-package which-key
  :custom
  (which-key-sort-order 'which-key-description-order)
  :hook
  (elpaca-after-init . which-key-mode))

(use-package which-key-posframe :after which-key
  :custom
  (which-key-posframe-poshandler #'posframe-poshandler-frame-bottom-left-corner)
  (which-key-posframe-parameters
   `((min-width . ,(frame-width))
     (left-fringe . 10)
     (right-fringe . 10)))
  :hook
  (which-key-mode . which-key-posframe-mode))

(use-package disable-mouse :demand t
  :diminish disable-mouse-mode
  :config
  (disable-mouse-global-mode))

(use-package editorconfig :demand t
  :config
  (setq-default editorconfig-lisp-use-default-indent t)
  (editorconfig-mode))

(use-package nerd-icons)

(use-package mixed-pitch :disabled)

(use-package ranger :after evil
  :hook
  (emacs-startup . ranger-override-dired-mode))

(use-package rainbow-mode
  :custom
  (rainbow-r-colors-alist '())
  (rainbow-html-colors-alist '()))

(use-package rainbow-delimiters)

(use-package telephone-line
  :config
  (telephone-line-mode))

(use-package ace-window :after posframe
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-dispatch-when-more-than 0)
  :preface
  (defun +ace-window-configure-fonts ()
    (set-face-attribute 'aw-leading-char-face nil
                        :foreground (face-foreground 'easy-color-faces-red)
                        :height 2.00
                        :weight 'bold))
  :init
  (util/if-daemon-run-after-make-frame-else-add-hook
   (ace-window-posframe-mode)
   'window-setup-hook)
  (util/if-daemon-run-after-make-frame-else-add-hook
   (+ace-window-configure-fonts)
   'window-setup-hook))

(use-package dashboard :after nerd-icons
  :preface
  (defun +dashboard-insert-project-shortmenu (&rest _)
    (let* ((fn #'project-switch-project)
           (fn-keymap (format "\\[%s]" fn))
           (icon-name (alist-get 'projects dashboard-heading-icons))
           (icon (nerd-icons-octicon icon-name :face 'dashboard-heading)))
      (insert (format "%s " icon))
      (widget-create 'item
                     :tag (format "%-30s" "Open project")
                     :action (lambda (&rest _) (call-interactively #'project-switch-project))
                     :mouse-face 'highlight
                     :button-face 'dashboard-heading
                     :button-prefix ""
                     :button-suffix ""
                     :format "%[%t%]")
      (insert (propertize (substitute-command-keys fn-keymap)
                          'face
                          'font-lock-constant-face))))

  (defun +dashboard-insert-org-agenda-shortmenu (&rest _)
    (let* ((fn #'org-agenda)
           (fn-keymap (format "\\[%s]" fn))
           (icon-name (alist-get 'agenda dashboard-heading-icons))
           (icon (nerd-icons-octicon icon-name :face 'dashboard-heading)))
      (insert (format "%s " icon))
      (widget-create 'item
                     :tag (format "%-30s" "Open org-agenda")
                     :action (lambda (&rest _) (call-interactively #'org-agenda))
                     :mouse-face 'highlight
                     :button-face 'dashboard-heading
                     :button-prefix ""
                     :button-suffix ""
                     :format "%[%t%]")
      (insert (propertize (substitute-command-keys fn-keymap)
                          'face
                          'font-lock-constant-face))))

  (defun +dashboard-insert-bookmark-shortmenu (&rest _)
    (let* ((fn #'consult-bookmark)
           (fn-keymap (format "\\[%s]" fn))
           (icon-name (alist-get 'bookmarks dashboard-heading-icons))
           (icon (nerd-icons-octicon icon-name :face 'dashboard-heading)))
      (insert (format "%s " icon))
      (widget-create 'item
                     :tag (format "%-30s" "Jump to bookmark")
                     :action (lambda (&rest _) (call-interactively #'consult-bookmark))
                     :mouse-face 'highlight
                     :button-face 'dashboard-heading
                     :button-prefix ""
                     :button-suffix ""
                     :format "%[%t%]")
      (insert (propertize (substitute-command-keys fn-keymap)
                          'face
                          'font-lock-constant-face))))

  (defun +dashboard-insert-recents-shortmenu (&rest _)
    (let* ((fn #'consult-recent-file)
           (fn-keymap (format "\\[%s]" fn))
           (icon-name (alist-get 'recents dashboard-heading-icons))
           (icon (nerd-icons-octicon icon-name :face 'dashboard-heading)))
      (insert (format "%s " icon))
      (widget-create 'item
                     :tag (format "%-30s" "Recently opened files")
                     :action (lambda (&rest _) (call-interactively #'consult-recent-file))
                     :mouse-face 'highlight
                     :button-face 'dashboard-heading
                     :button-prefix ""
                     :button-suffix ""
                     :format "%[%t%]")
      (insert (propertize (substitute-command-keys fn-keymap)
                          'face
                          'font-lock-constant-face))))
  :custom
  (dashboard-icon-type 'nerd-icons)
  (dashboard-startup-banner (expand-file-name "assets/logo.png" user-emacs-directory))
  (dashboard-banner-logo-title nil)
  (dashboard-projects-backend 'project-el)
  (dashboard-center-content t)
  (dashboard-show-shortcuts t)
  (dashboard-display-icons-p t)
  (dashboard-set-heading-icons t)
  (dashboard-set-footer nil)
  (dashboard-set-file-icons t)
  (dashboard-items '((agenda . 8)  (bookmarks . 8) (projects . 16) (recents . 32)))
  (initial-buffer-choice #'dashboard-open)
  (dashboard-startupify-list
   '(dashboard-insert-banner
     dashboard-insert-newline
     dashboard-insert-banner-title
     dashboard-insert-newline
     dashboard-insert-init-info
     dashboard-insert-items))

  ;; (dashboard-startupify-list
  ;;  '(dashboard-insert-banner
  ;;    dashboard-insert-newline
  ;;    dashboard-insert-banner-title
  ;;    dashboard-insert-newline
  ;;    dashboard-insert-init-info
  ;;    dashboard-insert-items))

  ;; (dashboard-item-generators
  ;;  '((recents . +dashboard-insert-recents-shortmenu)
  ;;    (bookmarks . +dashboard-insert-bookmark-shortmenu)
  ;;    (projects . +dashboard-insert-project-shortmenu)
  ;;    (agenda . +dashboard-insert-org-agenda-shortmenu)))

  ;; (dashboard-items
  ;;  '(agenda
  ;;    bookmarks
  ;;    projects
  ;;    recents))
  :hook
  (elpaca-after-init . dashboard-setup-startup-hook)
  (elpaca-after-init . dashboard-insert-startupify-lists))

(use-package treemacs
  :custom
  (treemacs-position 'left)
  (treemacs-width 35)
  (treemacs-display-in-side-window t)
  :config

  (defun +treemacs--popup-window ()
    "Pop up a side window and buffer for treemacs."
    (let ((buf (treemacs-get-local-buffer-create)))
      (display-buffer buf
                      `(,(if treemacs-display-in-side-window
                             'display-buffer-in-side-window
                           'display-buffer-in-direction)
                        . (;; for buffer in direction
                           (direction . ,treemacs-position)
                           (window . root)
                           ;; for side windows
                           (slot . 0)
                           (side . ,treemacs-position)
                           ;; general-purpose settings
                           (window-width . ,treemacs-width)
                           (dedicated . t))))
      (select-window (get-buffer-window buf))))

  (advice-add #'treemacs--popup-window :override #'+treemacs--popup-window))

(use-package treemacs-nerd-icons :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-evil :after treemacs)

(use-package treemacs-magit :after treemacs)

(provide 'packages-interface)

;;; packages-interface.el ends here
