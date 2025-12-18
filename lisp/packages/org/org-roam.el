;;; org/org-roam.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'util-strings)
(require 'util-lang)
(require 'util-helpers)
(require 'mule-util)

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
               ,(util/strings-from-file
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
               ,(util/strings-from-file
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
               ,(util/strings-from-file
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
               ,(util/strings-from-file
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
                 (util/strings-from-file
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
  (window-setup . (lambda () (unless init-file-debug (org-roam-ui-mode)))))

(defcustom +org-roam-ui-viewer-function nil
  "Function to launch org-roam-ui."
  :type 'function
  :group 'org-roam-ui
  :group 'convenience)

(defun +org-roam-ui-launch-viewer ()
  "Launch custom org-roam-ui viewer."
  (interactive)
  (util/quiet
    (funcall +org-roam-ui-viewer-function)))
