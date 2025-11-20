;;; packages-org-mode.el --- Org-mode Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'util-strings)
(require 'util-lang)
(require 'mule-util)

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

(use-package gnuplot-mode)

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
  (setq-local face-font-rescale-alist
              `(("-cdac$" . 1.3)
                (,(font-spec :family "Source Han Sans") . 1.3)))
  (let ((font-family (org-entry-get (point-min) "font-family" t))
        (font-height (org-entry-get (point-min) "font-height" t)))
    (when font-family
      (setq-local buffer-face-mode-face `(:family ,font-family)))
    (when font-height
      (setq-local buffer-face-mode-face `(:height ,(string-to-number font-height)))))

  (visual-line-mode 1)
  (visual-fill-column-mode 1)
  (org-modern-mode 1)
  (flyspell-mode)
  (buffer-face-mode 1)
  (completion-preview-mode 1)
  (yas-minor-mode 1)

  (util/add-capf-hooks
   #'yasnippet-capf
   #'cape-file
   #'cape-tex
   #'cape-elisp-block
   #'cape-keyword)

  (util/remove-capf-hooks
   #'pcomplete-completions-at-point
   t))

(defun +org-agenda-configure ()
  "Org-agenda configuration."
  (setq-local window-size-fixed 'width))

(defmacro +org-agenda-list-key (key)
  "Create Org-agenda shortcut functions for KEY."
  `(defun ,(intern (concat "+org-agenda-list-key-" key)) ()
     (interactive)
     (org-agenda nil ,key)))

(defmacro +org-capture-key (key)
  "Create Org-capture shortcut functions for KEY."
  `(defun ,(intern (concat "+org-capture-key-" key)) ()
     (interactive)
     (org-capture nil ,key)))

(defmacro +org-capture-open (description file)
  "Create Org-capture FILE shortcut functions for DESCRIPTION."
  `(defun ,(intern (concat "+org-capture-open-" description)) ()
     (interactive)
     (org-open-file (concat org-directory "/" ,file))))

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
            (lang (org-element-property :language element))
            (params (org-element-property :parameters element))
            (hidden (and params (string-match-p ":hidden t" params))))
       (if (or hidden (cl-some (lambda (l) (equal lang l)) +org-auto-hide-block-languages))
           (org-fold--hide-wrapper-toggle element 'block 'hide nil))))))

(defun +org-agenda-get-categories (file)
  "Return list of categories in org FILE."
  (with-current-buffer (org-capture-target-buffer file)
    (let (categories)
      (org-map-entries
       (lambda ()
         (add-to-list 'categories (org-get-category) t)))
      categories)))

(use-package org
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
  (org-read-date-popup-calendar nil)
  (org-read-date-display-live t)

  (org-src-preserve-indentation nil)
  (org-src-window-setup 'current-window)
  (org-edit-src-persistent-message nil)
  (org-edit-src-content-indentation 0)

  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis (concat " " (truncate-string-ellipsis) " ")) ;; folding symbol
  (org-use-sub-superscripts '{})

  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)

  (org-todo-keywords
   '((sequence "TODO" "ONGOING" "|" "DONE" "CANCELLED" )))

  (org-todo-keyword-faces
   `(("TODO"      . (
                     :inherit default
                     :weight bold
                     :foreground ,(doom-darken (doom-color 'red) 0.2)))
     ("ONGOING"   . (
                     :inherit default
                     :weight bold
                     :foreground ,(doom-darken (doom-color 'orange) 0.2)))
     ("CANCELLED" . (
                     :inherit default
                     :weight bold
                     :foreground ,(doom-darken (doom-color 'fg-alt) 0.3)))
     ("DONE"      . (
                     :inherit default
                     :weight bold
                     :foreground ,(doom-color 'fg-alt) 0.2))))

  (org-src-block-faces
   `(("japanese" (:height 1.5 :foreground ,(doom-color 'violet)))))
  (org-confirm-babel-evaluate nil)
  (org-plantuml-exec-mode 'plantuml)

  (org-default-notes-file "todo.org")
  (org-expiry-inactive-timestamps t)
  (org-capture-templates
   '(("t" "Todo"
      entry
      (file+headline
       "todo.org"
       (lambda ()
         (let* ((categories (+org-agenda-get-categories "todo.org"))
                (choice (completing-read "Category: " categories)))
           choice)))
      "* TODO %?\n  %i\n"
      :unnarrowed t)
     ("s" "Schedule"
      entry (file+olp+datetree "schedule.org")
      "%T %?\n"
      :time-prompt t
      :tree-type month
      :unnarrowed t)
     ("d" "Schedule Deadline"
      entry (file+olp+datetree "schedule.org")
      "%T %?\nDEADLINE: %^{DEADLINE}T\n"
      :time-prompt t
      :tree-type month
      :unnarrowed t)
     ("j" "Journal"
      entry (file+olp+datetree "journal.org")
      "%T %?\n %i\n"
      :unnarrowed t)))
  :hook
  (org-mode . +org-mode-setup)
  (org-mode . +org-fold-auto-hide-block-languages))

(use-package nil ;; org-agenda
  :custom
  (org-agenda-window-setup 'current-window)
  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid '((daily today require-timed)
                          (800 1000 1200 1400 1600 1800 2000)
                          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  :hook
  (org-agenda-mode . +org-agenda-configure))

(use-package nil ;; org-babel
  :custom
  (org-babel-default-header-args:go '((:wrap . "example")))
  :init
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
     ("cmake"      . cmake)
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
     ("gnuplot"    . gnuplot)
     ("typescript" . typescript)))

  :hook
  (org-babel-after-execute . org-redisplay-inline-images))

(use-package nil ;; org-embrace
  :after (org embrace)
  :init
  (defun +embrace-with-org-block ()
    (let ((block-type (completing-read
                       "Org block type: "
                       '(center comment example export justifyleft justifyright
                                quote src verse))))
      (cond ((string= block-type "src")
             (cons
              (concat (format "#+begin_src %s"
                              (completing-read "Language: "
                                               (embrace--get-org-src-block-modes)))
                      (let ((args (read-string "Arguments: ")))
                        (unless (string= args "")
                          (format " %s" args))))
              "#+end_src"))
            ((string= block-type "export")
             (cons (format "#+begin_export %s"
                           (completing-read "Format: "
                                            '(ascii beamer html latex texinfo)))
                   "#+end_export"))
            (t
             (setq block-type (downcase block-type))
             (cons (format "#+begin_%s" block-type)
                   (format "#+end_%s" block-type))))))

  (defun +embrace-org-mode-hook ()
    (dolist (lst '((?= "=" . "=")
                   (?~ "~" . "~")
                   (?/ "/" . "/")
                   (?* "*" . "*")
                   (?_ "_" . "_")
                   (?+ "+" . "+")
                   (?k "@@html:<kbd>@@" . "@@html:</kbd>@@")))
      (embrace-add-pair (car lst) (cadr lst) (cddr lst)))
    (embrace-add-pair-regexp ?l "#\\+begin_.*" "#\\+end_.*" 'embrace-with-org-block
                             (embrace-build-help "#+begin_*" "#+end") t))
  (advice-add #'embrace-with-org-block :override #'+embrace-with-org-block)
  (advice-add #'embrace-org-mode-hook :override #'+embrace-org-mode-hook)
  :hook
  (org-mode . embrace-org-mode-hook))

(use-package nil ;; org-latex
  :custom
  (org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc"
                                         "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf"
                                         "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf"
                                         "acn" "acr" "alg" "glg" "gls" "ist" "ltjruby")))
  (org-latex-hyperref-template nil)
  (org-highlight-latex-and-related '(native script entities))
  (org-startup-with-latex-preview t)
  (org-export-with-latex 'luadvisvgm)
  (org-html-with-latex 'luadvisvgm)
  (org-latex-preview-live '(inline block edit-special))
  (org-latex-preview-process-default 'luadvisvgm)
  (org-latex-preview-appearance-options
   `( :foreground auto
      :background auto
      :scale nil
      :zoom 1.3
      :page-width nil
      :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
  (org-latex-pdf-process
   '("lualatex -shell-escape -interaction nonstopmode %f"))
  :init
  (add-to-list
   'org-latex-preview-process-alist
   '(luadvisvgm :programs ("dvilualatex" "dvisvgm")
                :description "dvi > svg"
                :message "you need to install the programs: lualatex and dvisvgm."
                :image-size-adjust (1.7 . 1.5)
                :latex-precompiler
                ("dvilualatex --output-directory=/tmp --ini --jobname=%b \"&%L\" mylatexformat.ltx %f")
                :latex-compiler
                ("dvilualatex --output-directory=/tmp --output-format=dvi --shell-escape --interaction=nonstopmode %f")
                :image-input-type "dvi"
                :image-output-type "svg"
                :image-converter
                ("dvisvgm --page=1- --clipjoin --relative --no-fonts -v3 --bbox=preview --output=%B-%%9p.svg %f")))
  :hook
  (org-mode . org-cdlatex-mode))

(use-package org-crypt :after org
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
  (org-modern-star 'replace)
  (org-modern-replace-stars "○○○◦◦◦∙")
  (org-modern-internal-target '(" ↪ " t " "))
  (org-modern-radio-target '("  " t " "))
  (org-modern-progress nil)
  (org-modern-checkbox '((?X . "󰄳") (?- . "󰝥") (?\s . "󰝦")))
  :custom-face
  (org-level-1
   ((nil :weight regular
         :foreground ,(doom-color 'blue)
         :height 1.20)))
  (org-level-2
   ((nil :weight regular
         :foreground ,(doom-color 'dark-blue)
         :height 1.20)))
  (org-level-3
   ((nil :weight regular
         :foreground ,(doom-color 'violet)
         :height 1.20)))
  (org-level-4
   ((nil :weight regular
         :foreground ,(doom-color 'magenta)
         :height 1.10)))
  (org-level-5
   ((nil :weight regular
         :foreground ,(doom-color 'red)
         :height 1.10)))
  (org-level-6
   ((nil :weight regular
         :foreground ,(doom-color 'orange)
         :height 1.10)))
  (org-level-7
   ((nil :weight regular
         :foreground ,(doom-color 'yellow)
         :height 1.00)))
  (org-level-8
   ((nil :inherit default
         :foreground ,(doom-color 'fg)
         :height 1.00)))
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

(use-package org-super-agenda :after org
  :custom
  (org-agenda-custom-commands
   '(("n" "Today View"
      ((agenda "" ((org-agenda-span 'day)
                   (org-super-agenda-groups
                    '(( :name "Today"
                        :time-grid t
                        :scheduled today
                        :discard (:deadline t)
                        :order 0)
                      ( :habit t)
                      ( :name "Due Today"
                        :deadline today
                        :order 2)
                      ( :name "Due Soon"
                        :deadline future
                        :order 8)
                      ( :name "Overdue"
                        :deadline past
                        :order 7)
                      ))))
       ))
     ("t" "Todo View"
      ((todo "" ((org-agenda-overriding-header "Tasks")
                 (org-super-agenda-groups
                  '((:auto-category t)
                    (:discard (:todo "CANCELLED"))
                    ))))
       ))
     ))
  :hook
  (after-init . org-super-agenda-mode))

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
    `(list t :foreground unspecified :height ,height))

  (defmacro +org-remark-highlight-face (color)
    `(list t :foreground unspecified :inverse-video t))

  (defmacro +org-remark-color-face (color)
    `(list t :foreground unspecified))

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
                      (doom-lighten (doom-color 'fg-alt) 0.1)))

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
                      (doom-color 'fg-alt)))
  )

(defcustom +org-roam-profiles `(("default"
                                 :description "Default Org-roam"
                                 :directory ,(expand-file-name "~/org-roam")
                                 :db-location ,(locate-user-emacs-file "org-roam.db")))
  "Profiles for switching between different note org-roam repositories."
  :type '(alist :key-type (string :tag "Name")
                :value-type (plist :options
                                   (((const :tag "Description" :description) string)
                                    ((const :tag "Directory" :directory) string)
                                    ((const :tag "DB Path" :db-location) string))))
  :group 'org-roam
  :group 'convenience)

(defcustom +org-roam-node-types '("capture"
                                  "concept"
                                  "procedure"
                                  "reference"
                                  "index")
  "A list of node types."
  :type '(repeat string)
  :group 'org-roam
  :group 'convenience)

(use-package org-roam :after (org persist emacsql)
  :custom
  (org-roam-node-display-template (concat "${title:40} " (propertize "${tags:80}" 'face 'org-tag)))
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-completion-everywhere nil)
  (org-roam-capture-templates
   `(("d" "default"
      plain "%?"
      :target (file+head
               "${id}.org"
               ,(util/read-file-to-string
                 (expand-file-name "templates/default.org" user-emacs-directory)))
      :jump-to-captured t
      :immediate-finish t
      :unnarrowed t
      :empty-lines-before 1
      :prepend t)
     ("c" "code"
      plain "%?"
      :target (file+head
               "${id}.org"
               ,(util/read-file-to-string
                 (expand-file-name "templates/code.org" user-emacs-directory)))
      :jump-to-captured t
      :immediate-finish t
      :unnarrowed t
      :empty-lines-before 1
      :prepend t)
     ("j" "japanese"
      plain "%?"
      :target (file+head
               "${id}.org"
               ,(util/read-file-to-string
                 (expand-file-name "templates/japanese.org" user-emacs-directory)))
      :jump-to-captured t
      :immediate-finish t
      :unnarrowed t
      :empty-lines-before 1
      :prepend t)
     ("m" "mathematics"
      plain "%?"
      :target (file+head
               "${id}.org"
               ,(util/read-file-to-string
                 (expand-file-name "templates/mathematics.org" user-emacs-directory)))
      :jump-to-captured t
      :immediate-finish t
      :unnarrowed t
      :empty-lines-before 1
      :prepend t)
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
      :unnarrowed t
      :empty-lines-before 1
      :prepend t)))
  :init
  (defun +org-roam--profile-candidate-entry (cand)
    "Create Org-roam profile CAND entry."
    (let* ((profile-name (car cand))
           (description (plist-get (cdr cand) :description)))
      (list (util/strings-pad-string profile-name 20) `(:description ,(format "%s" description)))))

  (defun +org-roam--profile-annotations (cand)
    "Retrieve profile CAND description."
    (let* ((option (car (last (assoc cand minibuffer-completion-table))))
           (description (copy-sequence (plist-get option :description))))
      (concat " " (util/strings-add-font-lock description 'font-lock-comment-face))))

  (defun +org-roam--profile-prompter ()
    "`org-roam--profile' prompter function."
    (let* ((completion-extra-properties '(:annotation-function +org-roam--profile-annotations)))
      (s-trim
       (completing-read
        "Org-roam profile: "
        (mapcar #'+org-roam--profile-candidate-entry +org-roam-profiles)))))

  (defmacro +org-roam-with-profile (profile-name &rest body)
    "Run BODY with org-roam profile with PROFILE-NAME."
    (declare (indent 1))
    `(let* ((profile (cdr (assoc ,profile-name +org-roam-profiles))))
       (unless profile (error "Invalid profile name"))
       (let ((org-roam-directory (plist-get profile :directory))
             (org-roam-db-location (plist-get profile :db-location)))
         (org-roam-db-sync)
         ,@body)
       (org-roam-db-sync)))

  (defun +org-roam-switch-profile (&optional profile-name)
    "Load Org-roam PROFILE-NAME."
    (interactive)
    (let* ((profile-name (or profile-name (+org-roam--profile-prompter))))
      (let* ((profile (cdr (assoc profile-name +org-roam-profiles))))
        (unless profile (error "Invalid profile name"))
        (setq +org-roam-current-profile profile-name
              org-roam-directory (plist-get profile :directory)
              org-roam-db-location (plist-get profile :db-location))
        (org-roam-db-sync))))

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
    (let ((sql [:select :distinct [tags:tag]
                :from tags
                :order-by [[tags:tag] :asc]]))
      (mapcar #'car (org-roam-db-query sql))))

  (defun +org-roam--list-nodes-with-tags (tags)
    "List Org-roam files with TAGS."
    (mapcar #'cdr (org-roam-node-read--completions
                   (lambda (node)
                     (let ((ntags (org-roam-node-tags node)))
                       (cl-every (lambda (t) (member t ntags)) tags))))))

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

  (with-demoted-errors "Variable `+org-roam-default-profile' failed to load persisted data: %S"
    (persist-defvar +org-roam-current-profile "notes" "`org-roam' default profile."))

  (+org-roam-switch-profile +org-roam-current-profile)

  :config
  (defun +org-roam-node-find (&optional arg)
    "`org-roam-node-find' wrapper.

With prefix ARG \\[universal-argument], one-shot note selection for profile."
    (interactive "p")
    (pcase arg
      (4 (let ((profile-name (+org-roam--profile-prompter)))
           (+org-roam-with-profile profile-name (org-roam-node-find))))
      (_ (org-roam-node-find))))

  (org-roam-db-autosync-mode))

(use-package org-roam-ui :after org-roam
  :custom
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil)
  (org-roam-ui-sync-theme t)
  :hook
  (window-setup . org-roam-ui-mode))

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

(use-package org-typst-preview)

(defun +org-typst-preview-render (&optional arg)
  "Render/clear `Typst` preview in buffer.

With prefix ARG \\[universal-argument], clear preview in buffer."
  (interactive "p")
  (pcase arg
    (4 (org-typst-preview-clear-buffer))
    (_ (org-typst-preview-render-buffer))))

(provide 'packages-org-mode)

;;; packages-org-mode.el ends here
