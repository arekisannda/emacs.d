;;; util-commands.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defcustom util/commands-command-list '()
  "List of preset commands."
  :local t
  :type '(alist :key-type
                (string :tag "Description")
                :value-type
                (plist :options
                       ((:doc (string :tag "Description"))
                        (:command (choice
                                   (string :tag "Shell Command")
                                   (function :tag "Elisp Function")))))))

(defcustom util/commands-string-command-function #'async-shell-command
  "Function to execute string commands."
  :type 'function)

(defun util/commands--quote-command (command)
  (let (str
        in-quote)
    (dolist (c (split-string command))
      (cond
       (in-quote
        (setq str (concat str (shell-quote-argument " ") c))
        (when (string-suffix-p "\"" c) (setq in-quote nil))
        )
       (t
        (setq str (concat str " " c))
        (when (string-prefix-p "\"" c) (setq in-quote t))
        )))
    (string-trim str)))

(defun util/commands-root-dir ()
  (or (locate-dominating-file default-directory ".dir-locals.el")
      (when-let* ((proj (project-current)))
        (project-root proj))
      default-directory))

(defcustom util/commands-use-base-commands t
  "Use base commands."
  :type 'boolean
  :local t)

(defcustom util/commands-run-command-max-depth 1
  "Run command recursion limit."
  :type 'integer)

(defcustom util/commands-add-base-commands-functions '()
  "Add base commands.
Function takes one argument for BUFFER and return list of command entries or nil."
  :type 'hook)

(defun util/commands--get-placeholder-value (placeholder)
  (when-let* ((ph-name (and (string-match "\\${{\\(.*?\\)}}" placeholder) (match-string 1 placeholder)))
              (ph-var (intern ph-name)))
    (unless (boundp ph-var) (error "Invalid placeholder"))
    (symbol-value ph-var)))

(defun util/commands-replace-placeholders (str)
  "Return a list of all ${{...}} matches in STR."
  (let ((pos 0)
        (matches '()))
    (while (string-match "\\${{\\(.*?\\)}}" str pos)
      (push (match-string 0 str) matches)  ; full match, e.g. "${{foo}}"
      (setq pos (match-end 0)))

    (dolist (ph matches)
      (setq str (string-replace ph (util/commands--get-placeholder-value ph) str)))
    str))

(defun util/commands--run-command-candidate (cand)
  (list (car cand) `(:info ,(plist-get (cdr cand) :doc))))

(defun util/commands--run-command-annotation (command &optional column-width)
  (let* ((params (car (last (assoc command minibuffer-completion-table))))
         (info (or (plist-get params :info) "")))
    (concat " "
            (propertize " " 'display `(space :align-to ,(or column-width 0)))
            (util/strings-add-font-lock (copy-sequence info) 'font-lock-comment-face))))

(defun util/commands--run-command (command &optional depth)
  (when (and (numberp depth) (< depth 0)) (error "Invalid call depth %d" depth))

  (when (and (numberp depth) (> depth util/commands-run-command-max-depth))
    (error "Reached maximum call depth %d %S" depth command))

  (when (stringp command)
    (setq command (util/commands-replace-placeholders command)))

  (cond
   ((functionp command)
    (when-let* ((res (funcall command))
                (command (and (plistp res)
                              (plist-get res :command))))
      (util/commands--run-command command (1+ (or depth 0)))))
   ((stringp command) (funcall util/commands-string-command-function command)))
  )

(defun util/commands-run-command (command &optional suppress-output)
  "Run preset command."
  (interactive
   (let ((default-directory default-directory)
         commands
         column-width)
     (unless buffer-file-name
       (hack-dir-local-variables-non-file-buffer))

     (setq commands
           (append commands
                   (buffer-local-value 'util/commands-command-list (current-buffer))
                   (apply #'append
                          (mapcar (lambda (fn) (funcall fn (current-buffer)))
                                  util/commands-add-base-commands-functions))))

     (setq column-width (+ 5 (cl-loop for (key . _) in commands
                                      maximize (string-width key) into max
                                      finally return (or max 0))))

     (let* ((prompt
             (format "Run command [%s]: "
                     (shrink-path-dirs
                      (or (car (dir-locals-find-file default-directory))
                          (when-let* ((proj (project-current)))
                            (project-root proj))
                          default-directory)
                      )))
            (completion-extra-properties `(:annotation-function
                                           ,(lambda (cand)
                                              (util/commands--run-command-annotation cand column-width))))
            (selected (string-trim
                       (completing-read
                        prompt
                        (mapcar #'util/commands--run-command-candidate commands))))
            (preset (plist-get (alist-get selected commands nil nil #'equal) :command)))
       (if (functionp preset)
           (list preset current-prefix-arg)
         (list (or preset selected) current-prefix-arg)
         ))
     ))

  (let ((default-directory (util/commands-root-dir))
        (display-buffer-alist display-buffer-alist))

    (when suppress-output
      (setq display-buffer-alist
            '(("\\*Async Shell Command\\*"    display-buffer-no-window)
              ("\\*Detached Shell Command\\*" display-buffer-no-window))
            ))

    (util/commands--run-command command)))

(provide 'util-commands)
