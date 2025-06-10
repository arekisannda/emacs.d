;;; packages-org-mode.el --- Org-mode Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl)
(require 'util-strings)
(require 'util-lang)

(use-package ob-go)

(use-package ob-rust)

(use-package ob-typescript)

(use-package ob-kotlin)

(use-package ob-mermaid
  :custom
  (ob-mermaid-cli-path (executable-find "mmdc"))
  :config
  (setq org-babel-default-header-args:mermaid
        '((:results . "file")
          (:exports . "results")
          (:theme . "dark")
          (:background-color . "transparent"))))

(use-package gnuplot)

(use-package valign
  :custom
  (valign-fancy-bar t))

(defmacro +org-table-set-height (type)
  "Helper function to set table font height for TYPE."
  (progn
    (face-remap-set-base 'org-table
                         :inherit 'org-table
                         :height (plist-get +org-table-heights type))))

(defun +org-mode-setup ()
  "Setup to run for `org-mode` major modes."
  (visual-line-mode 1)
  (visual-fill-column-mode 1)
  (org-modern-mode 1)
  (util/lang--add-to-capf-list (list #'yasnippet-capf
                                     #'cape-dabbrev
                                     #'cape-file
                                     #'cape-tex
                                     #'cape-elisp-block
                                     #'cape-keyword
                                     ))
  ;; (display-line-numbers-mode 1)
  ;; (diff-hl-mode 1)
  (flyspell-mode)
  (valign-mode t)

  (let ((font-family (org-entry-get (point-min) "font-family" t))
        (font-height (org-entry-get (point-min) "font-height" t)))
    (when font-family
      (setq-local buffer-face-mode-face `(:family ,font-family)))
    (when font-height
      (setq-local buffer-face-mode-face `(:height ,(string-to-number font-height))))
    (buffer-face-mode)))

(defun +org-agenda-configure ()
  "Org-agenda configuration."
  (setq-local window-size-fixed 'width))

(defmacro +org-agenda-list-key (key)
  "Create Org-agenda shortcut functions for KEY."
  `(defun ,(intern (concat "+org-agenda-list-key-" key)) ()
     (interactive)
     (org-agenda nil ,key)))

(defcustom +org-auto-hide-block-languages '()
  "List of languages to auto hide."
  :type '(repeat string)
  :group 'org)

(defun +org-fold-auto-hide-block-languages ()
  "Fold blocks matching languages in `+org-auto-hide-block-languages'."
  (interactive)
  (org-block-map
   (lambda ()
     (let* ((element (org-element-at-point))
            (lang (org-element-property :language element)))
       (if (cl-some (lambda (l) (equal lang l)) +org-auto-hide-block-languages)
           (org-fold--hide-wrapper-toggle element 'block 'hide nil))))))

(use-package org
  :ensure `(org :repo "https://code.tecosaur.net/tec/org-mode.git/"
                :branch "dev")
  :custom
  (+org-auto-hide-block-languages '("mermaid"))

  (org-pretty-entities-include-sub-superscripts nil)
  (org-use-tag-inheritance nil)
  (org-link-descriptive t)
  (org-startup-with-inline-images t)
  (org-image-align 'center)
  (org-image-actual-width nil)
  (org-startup-indented t)
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-cycle-level-faces nil)
  (org-read-date-popup-calendar t)

  (org-src-preserve-indentation nil)
  (org-src-window-setup 'current-window)
  (org-edit-src-persistent-message nil)
  (org-edit-src-content-indentation 0)

  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis " ... ") ;; folding symbol
  (org-use-sub-superscripts '{})

  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)

  (org-todo-keywords
   '((sequence "TODO" "ONGOING" "|" "DONE" "CANCELLED" )))

  (org-todo-keyword-faces
   `(("TODO"      . (nil :inherit default
                         :weight bold
                         :foreground ,(doom-darken (doom-color 'red) 0.2)))
     ("ONGOING"   . (nil :inherit default
                         :weight bold
                         :foreground ,(doom-darken (doom-color 'orange) 0.2)))
     ("CANCELLED" . (nil :inherit default
                         :weight bold
                         :foreground ,(doom-darken (doom-color 'gray) 0.2)))
     ("DONE"      . (nil :inherit default
                         :weight bold
                         :foreground ,(doom-lighten (doom-color 'gray) 0.2)))))

  (org-agenda-window-setup 'current-window)
  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid '((daily today require-timed)
                          (800 1000 1200 1400 1600 1800 2000)
                          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  (org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc"
                                         "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf"
                                         "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf"
                                         "acn" "acr" "alg" "glg" "gls" "ist")))
  (org-latex-hyperref-template nil)
  (org-highlight-latex-and-related '(native script entities))
  (org-startup-with-latex-preview t)
  (org-latex-preview-live '(inline block edit-special))
  (org-latex-preview-process-default 'luadvisvgm)
  (org-latex-preview-appearance-options
   `( :foreground auto
      :background "Transparent"
      :scale 2.0
      :zoom ,(* (/ (face-attribute 'default :height) 100.0) 1.6)
      :page-width nil
      :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
  (org-latex-pdf-process
   '("latexmk -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"))

  (org-agenda-files '("~/Agenda/date"
                      "~/Agenda/project"
                      "~/Agenda/work"
                      "~/Agenda/general"))
  (org-src-block-faces
   `(("japanese" (:height 1.5 :foreground ,(doom-color 'violet)))))
  (org-confirm-babel-evaluate nil)
  (org-babel-default-header-args:go '((:wrap . "example")))
  (org-plantuml-exec-mode 'plantuml)
  :config
  (dolist (face `((org-level-1 . 1.175)
                  (org-level-2 . 1.150)
                  (org-level-3 . 1.125)
                  (org-level-4 . 1.100)
                  (org-level-5 . 1.075)
                  (org-level-6 . 1.050)
                  (org-level-7 . 1.025)
                  (org-level-8 . 1.00)))
    (set-face-attribute (car face) nil :weight 'regular :height (cdr face)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (awk . t)
     (calc . t)
     (ditaa . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (go . t)
     (js . t)
     (kotlin . t)
     (latex . t)
     (mermaid . t)
     (octave . t)
     (plantuml . t)
     (python . t)
     (rust . t)
     (R . t)
     (shell . t)
     (sql . t)
     (sqlite . t)
     (typescript . t)))

  (util/update-alist
   'org-src-lang-modes
   '(("C"          . c)
     ("C++"        . c++)
     ("bash"       . shell)
     ("cpp"        . c++)
     ("desktop"    . conf-desktop)
     ("dot"        . graphviz-dot)
     ("elisp"      . emacs-lisp)
     ("go"         . go)
     ("javascript" . javascript)
     ("kotlin"     . kotlin)
     ("mermaid"    . mermaid)
     ("ocaml"      . tuareg)
     ("python"     . python)
     ("rust"       . rust)
     ("screen"     . shell-script)
     ("shell"      . sh)
     ("sqlite"     . sql)
     ("toml"       . conf-toml)
     ("typescript" . typescript)))

  (add-to-list 'org-latex-preview-process-alist
               '(luadvisvgm :programs ("dvilualatex" "dvisvgm")
                            :description "dvi > svg"
                            :message "you need to install the programs: lualatex and dvisvgm."
                            :image-input-type "dvi"
                            :image-output-type "svg"
                            :image-size-adjust (1.7 . 1.5)
                            :latex-precompiler
                            ("dvilualatex --output-directory=/tmp --ini --jobname=%b \"&%L\" mylatexformat.ltx %f")
                            :latex-compiler
                            ("dvilualatex --output-format=dvi --shell-escape --interaction=nonstopmode --output-directory=/tmp %f")
                            :image-converter
                            ("dvisvgm --page=1- --clipjoin --relative --no-fonts -v3 --bbox=preview --output=%B-%%9p.svg %f")))

  :hook
  (org-babel-after-execute . org-redisplay-inline-images)
  (org-agenda-mode . +org-agenda-configure)
  (org-mode . +org-mode-setup)
  (org-mode . +org-fold-auto-hide-block-languages)
  (org-mode . embrace-org-mode-hook)
  (org-mode . org-cdlatex-mode))

(use-package org-crypt :after org
  :ensure nil
  :custom
  (org-tags-exclude-from-inheritance (quote ("crypt")))
  :config
  (org-crypt-use-before-save-magic))

(use-package org-modern :after org
  :custom
  (org-modern-table nil)
  (org-modern-timestamp nil)
  (org-modern-todo nil)
  (org-modern-todo-faces nil)
  (org-modern-star "○")
  (org-modern-replace-stars "○")
  (org-modern-internal-target '(" ↪ " t " "))
  (org-modern-radio-target '("  " t " "))
  (org-modern-progress '("󰝦" "󰪞" "󰪟" "󰪠" "󰪡" "󰪢" "󰪣" "󰪤" "󰪥"))
  (org-modern-checkbox '((?X . "󰄳") (?- . "󰝥") (?\s . "󰝦")))
  :custom-face
  (org-checkbox
   ((nil :box nil
         :height ,+fonts-fixed-pitch-size)))
  (org-modern-label
   ((nil :box (:line-width 5 :style flat-button)
         :height ,+fonts-fixed-pitch-size)))
  (org-block
   ((nil :background ,(doom-color 'bg)))))

(use-package org-contrib :after org
  :config
  (require 'ox-extra)
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

(use-package org-super-agenda
  :custom
  (org-agenda-custom-commands
   '(("n" "Next View"
      ((agenda "" ((org-agenda-span 'day)
                   (org-super-agenda-groups
                    '((:name "Today"
                             :time-grid t
                             :todo "TODAY"
                             :scheduled today
                             :order 0)
                      (:habit t)
                      (:name "Due Today"
                             :deadline today
                             :order 2)
                      (:name "Due Soon"
                             :deadline future
                             :order 8)
                      (:name "Overdue"
                             :deadline past
                             :order 7)
                      ))))
       (todo "" ((org-agenda-overriding-header "")
                 (org-super-agenda-groups
                  '((:name "Inbox"
                           :file-path "inbox"
                           :order 0
                           )
                    (:discard (:todo "TODO"))
                    (:auto-category t
                                    :order 9)
                    ))))))
     ("t" "Todo View"
      (
       (todo "" ((org-agenda-overriding-header "")
                 (org-super-agenda-groups
                  '((:name "Inbox"
                           :file-path "inbox"
                           :order 0
                           )
                    (:auto-category t
                                    :order 9)
                    ))))))
     ))

  :hook
  (elpaca-after-init . org-super-agenda-mode))

(use-package org-remark :after org
  :custom
  (org-remark-create-default-pen-set nil)
  (org-remark-notes-file-name ".remarks.org")
  (org-remark-notes-auto-delete :auto-delete)
  (org-remark-notes-buffer-name "*remark-notes*")
  (org-remark-notes-display-buffer-action '())
  :custom-face
  (org-remark-highlighter
   ((nil :inherit default
         :weight bold)))
  (org-remark-highlighter-warning
   ((nil :inherit default
         :weight bold
         :foreground ,(doom-darken (doom-color 'yellow) 0.2))))
  :config
  (org-remark-global-tracking-mode +1)
  (org-remark-line-mode +1)

  (defmacro +org-remark-height-face (height)
    `(list t :height ,height))

  (defmacro +org-remark-event-face (color height)
    `(list t :foreground ,color :height ,height))

  (defmacro +org-remark-highlight-face (color)
    `(list t :foreground ,color :inverse-video t))

  (defmacro +org-remark-color-face (color)
    `(list t :foreground ,color))

  (org-remark-create "size-025" (+org-remark-height-face 0.25))
  (org-remark-create "size-050" (+org-remark-height-face 0.50))
  (org-remark-create "size-075" (+org-remark-height-face 0.75))
  (org-remark-create "size-125" (+org-remark-height-face 1.25))
  (org-remark-create "size-150" (+org-remark-height-face 1.50))
  (org-remark-create "size-175" (+org-remark-height-face 1.75))
  (org-remark-create "size-200" (+org-remark-height-face 2.00))

  (org-remark-create "warn"
                     (+org-remark-event-face
                      (doom-darken (doom-color 'yellow) 0.1)
                      1.25))
  (org-remark-create "error"
                     (+org-remark-event-face
                      (doom-darken (doom-color 'red) 0.1)
                      1.25))

  (org-remark-create "hl-yellow"
                     (+org-remark-highlight-face
                      (doom-lighten (doom-color 'yellow) 0.1)))
  (org-remark-create "hl-orange"
                     (+org-remark-highlight-face
                      (doom-lighten (doom-color 'orange) 0.1)))
  (org-remark-create "hl-red"
                     (+org-remark-highlight-face
                      (doom-lighten (doom-color 'red) 0.1)))
  (org-remark-create "hl-magenta"
                     (+org-remark-highlight-face
                      (doom-lighten (doom-color 'magenta) 0.1)))
  (org-remark-create "hl-blue"
                     (+org-remark-highlight-face
                      (doom-lighten (doom-color 'dark-blue) 0.1)))
  (org-remark-create "hl-green"
                     (+org-remark-highlight-face
                      (doom-lighten (doom-color 'green) 0.1)))
  (org-remark-create "hl-cyan"
                     (+org-remark-highlight-face
                      (doom-lighten (doom-color 'blue) 0.1)))
  (org-remark-create "hl-violet"
                     (+org-remark-highlight-face
                      (doom-lighten (doom-color 'violet) 0.3)))
  (org-remark-create "hl-purple"
                     (+org-remark-highlight-face
                      (doom-lighten (doom-color 'violet) 0.1)))
  (org-remark-create "hl-gray"
                     (+org-remark-highlight-face
                      (doom-lighten (doom-color 'grey) 0.1)))

  (org-remark-create "yellow"
                     (+org-remark-color-face
                      (doom-color 'yellow)))
  (org-remark-create "orange"
                     (+org-remark-color-face
                      (doom-color 'orange)))
  (org-remark-create "red"
                     (+org-remark-color-face
                      (doom-color 'red)))
  (org-remark-create "magenta"
                     (+org-remark-color-face
                      (doom-color 'magenta)))
  (org-remark-create "blue"
                     (+org-remark-color-face
                      (doom-lighten (doom-color 'dark-blue) 0.1)))
  (org-remark-create "green"
                     (+org-remark-color-face
                      (doom-color 'green)))
  (org-remark-create "cyan"
                     (+org-remark-color-face
                      (doom-color 'cyan)))
  (org-remark-create "violet"
                     (+org-remark-color-face
                      (doom-lighten (doom-color 'violet) 0.3)))
  (org-remark-create "purple"
                     (+org-remark-color-face
                      (doom-color 'violet)))
  (org-remark-create "gray"
                     (+org-remark-color-face
                      (doom-color 'grey)))
  )

(defcustom +org-roam-default-profile "default"
  "Default Org-roam profile."
  :type 'string
  :group 'org-roam
  :group 'convenience)

(defvar +org-roam-current-profile "default")

(defcustom +org-roam-profiles `(("default"
                                 :description "Default Org-roam"
                                 :directory ,(expand-file-name "~/org-roam")
                                 :db-location ,(locate-user-emacs-file "org-roam.db")))
  "Profiles for switching between different note org-roam repositories."
  :type '(repeat
          (list :tag "Org-roam Profile"
                (string :tag "Name")
                (string :tag "Description")
                (string :tag "Directory")
                (string :tag "DB Path")))
  :group 'org-roam
  :group 'convenience)

(defun +org-roam--profile-candidate-entry (cand)
  "Create Org-roam profile CAND entry."
  (let* ((profile-name (car cand))
         (description (plist-get (cdr cand) :description)))
    (list (util/strings-pad-string profile-name 20) `(:description ,(format "%s" description)))))

(defun +org-roam--profile-annotations (cand)
  "Retrieve profile CAND description."
  (let* ((option (car (last (assoc cand minibuffer-completion-table))))
         (description (plist-get option :description)))
    (concat " " (util/strings-add-font-lock description 'font-lock-comment-face))))

(defun +org-roam-switch-profile (&optional profile-name)
  "Load Org-roam PROFILE-NAME."
  (interactive)

  (unless profile-name
    (setq completion-extra-properties '(:annotation-function +org-roam--profile-annotations))
    (setq profile-name (s-trim
                        (completing-read
                         "Org-roam profile: "
                         (mapcar #'+org-roam--profile-candidate-entry +org-roam-profiles)))))

  (let* ((profile (cdr (assoc profile-name +org-roam-profiles))))
    (unless profile (error "Invalid profile name"))
    (setq +org-roam-current-profile profile-name)
    (setq org-roam-directory (plist-get profile :directory))
    (setq org-roam-db-location (plist-get profile :db-location))
    (org-roam-db-sync)))

(defcustom +org-roam-node-types '("capture"
                                  "concept"
                                  "procedure"
                                  "reference"
                                  "index")
  "A list of node types."
  :type '(repeat string)
  :group 'org-roam
  :group 'convenience)

(use-package org-roam
  :custom
  (+org-roam-default-profile "notes")
  (org-roam-node-display-template (concat "${title:40} " (propertize "${tags:80}" 'face 'org-tag)))
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   `(("d" "default"
      plain "%?"
      :target (file+head
               "${id}.org"
               ,(util/read-file-to-string
                 (expand-file-name "templates/default.org" user-emacs-directory)))
      :jump-to-captured t
      :immediate-finish t
      :unnarrowed t)
     ("c" "code"
      plain "%?"
      :target (file+head
               "${id}.org"
               ,(util/read-file-to-string
                 (expand-file-name "templates/code.org" user-emacs-directory)))
      :jump-to-captured t
      :immediate-finish t
      :unnarrowed t)
     ("j" "japanese"
      plain "%?"
      :target (file+head
               "${id}.org"
               ,(util/read-file-to-string
                 (expand-file-name "templates/japanese.org" user-emacs-directory)))
      :jump-to-captured t
      :immediate-finish t
      :unnarrowed t)
     ("m" "mathematics"
      plain "%?"
      :target (file+head
               "${id}.org"
               ,(util/read-file-to-string
                 (expand-file-name "templates/mathematics.org" user-emacs-directory)))
      :jump-to-captured t
      :immediate-finish t
      :unnarrowed t)
     ("e" "encrypted"
      plain "%?"
      :target (file+head
               "${id}.org.gpg"
               ,(concat
                 "# -*- mode:org -*-\n"
                 (util/read-file-to-string
                  (expand-file-name "templates/default.org" user-emacs-directory))))
      :jump-to-captured t
      :immediate-finish t
      :unnarrowed t)))
  :init
  (+org-roam-switch-profile +org-roam-default-profile)
  (org-roam-db-autosync-enable)
  :config
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (let ((tags (org-roam-node-tags node)))
          (car (seq-some
                (lambda (tag) (member tag tags))
                +org-roam-node-types)))
      (error ""))))

(defun +org-roam-node-remove ()
  "Remove node."
  (interactive)
  (let* ((file (buffer-file-name (current-buffer)))
         (roam-p (org-roam-file-p file))
         (id (and roam-p
                  (car (car (org-roam-db-query [:select id :from nodes :where (= file $s1)] file)))))
         (node (org-roam-node-from-id id))
         (target (org-roam-node-read (and node (org-roam-node-title node))))
         (target-file (org-roam-node-file target)))
    (when (y-or-n-p (format "Delete node '%s'?" (org-roam-node-title target)))
      (delete-file target-file)
      (if-let* ((buffer (find-buffer-visiting target-file)))
          (kill-buffer-ask buffer)))
    (org-roam-db-sync)))

(defun +org-roam--list-tags ()
  "List Org-roam tags."
  (let ((sql "SELECT DISTINCT tags.tag FROM tags ORDER BY tags.tag COLLATE NOCASE ASC"))
    (mapcar #'car (org-roam-db-query sql))))

(defun +org-roam-node-find-by-filetag (&optional tag)
  "Find an Org-roam node by filetag TAG."
  (interactive
   (list (completing-read "Tag: " (+org-roam--list-tags) nil t)))
  (org-roam-node-find nil (format "#%s" tag)))

(defun +org-roam-node-find-uncategorized ()
  "Find Org-roam node non-categorized by PACER."
  (interactive)
  (org-roam-node-find nil nil
                      (lambda (node)
                        (seq-every-p
                         (lambda (tag) (not (member tag (org-roam-node-tags node))))
                         +org-roam-node-types))))

(defun +org-roam-node-find-captures ()
  "Find Org-roam node not processed by PACER."
  (interactive)
  (org-roam-node-find nil nil
                      (lambda (node)
                        (member "capture" (org-roam-node-tags node)))))

(use-package org-roam-ui :after org-roam
  :custom
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil)
  (org-roam-ui-sync-theme t))

(defcustom +org-roam-ui-viewer-function nil
  "Function to launch org-roam-ui."
  :type 'function
  :group 'org-roam-ui
  :group 'convenience)

(defun +org-roam-ui-launch-viewer ()
  "Launch custom org-roam-ui viewer."
  (interactive)
  (shut-up
    (funcall +org-roam-ui-viewer-function)))

(defun +org-scratch-buffer ()
  "Open a new scratch buffer in Org mode."
  (interactive)
  (let ((buffer (get-buffer-create "*org-scratch*")))
    (with-current-buffer buffer
      (org-mode))
    (display-buffer buffer)))

(use-package edraw :after (org ox)
  :ensure (:host github :repo "misohena/el-easydraw" :files (:defaults "*.el"))
  :custom
  (edraw-default-document-properties
   '((width . 600)
     (height . 400)
     (background . "#00000000")))
  (edraw-package-default-shape-properties
   `((rect
      (fill . ,(doom-color 'fg-alt))
      (stroke . ,(doom-color 'bg-alt))
      (stroke-width . 1))
     (ellipse
      (fill . ,(doom-color 'fg-alt))
      (stroke . ,(doom-color 'bg-alt))
      (stroke-width . 1))
     (path
      (fill . "none")
      (stroke . ,(doom-color 'bg-alt))
      (stroke-width . 1)
      ;; (marker-end . "arrow")
      )
     (text
      (fill . ,(doom-color 'bg-alt)) ;; Not edraw-package-default-fill
      (font-size . 16)
      (font-family . ,+fonts-variable-pitch-face)
      (text-anchor . "middle"))
     (image)))
  :config
  (require 'edraw-org)
  (edraw-org-setup-default))

(use-package org-download :after org
  :custom
  (org-download-image-dir "./images")
  (org-download-screenshot-method "grim -g \"$(slurp)\" %s"))

(use-package org-typst-preview
  :ensure (:type git :host github :repo "remimimimimi/org-typst-preview.el"))

(defun +org-typst-preview-render (&optional arg)
  "Render/clear `Typst` preview in buffer.

With prefix ARG \\[universal-argument], clear preview in buffer."
  (interactive "p")
  (pcase arg
    (4 (org-typst-preview-clear-buffer))
    (_ (org-typst-preview-render-buffer))))

(provide 'packages-org-mode)

;;; packages-org-mode.el ends here
