;;; util-commands.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defcustom util/commands-command-list '()
  "List of preset commands."
  :local t
  :type '(alist :key-type
                (string :tag "Description")
                :value-type
                (choice
                 (string :tag "Shell Command")
                 (sexp :tag "Elisp Function"))))

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
        ))
      )
    (s-trim str)))

(defun util/commands-root-dir ()
  (or (locate-dominating-file default-directory ".dir-locals.el")
      (project-root (project-current))
      default-directory))

(defcustom util/commands-run-list-additional-command-hook '()
  "Add additional commands.
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

(defun util/commands-run-command (command &optional suppress-output)
  "Run preset command."
  (interactive
   (let ((commands
          (apply #'append
                 (mapcar(lambda (fn) (funcall fn (current-buffer)))
                        util/commands-run-list-additional-command-hook))))
     (let ((default-directory default-directory))
       (unless buffer-file-name
         (hack-dir-local-variables-non-file-buffer))

       (setq commands
             (append commands
                     (buffer-local-value 'util/commands-command-list (current-buffer))))

       (let* ((prompt
               (format "Run command [%s]: "
                       (shrink-path-dirs
                        (or (car (dir-locals-find-file default-directory))
                            (when-let ((proj (project-current)))
                              (project-root proj))
                            default-directory)
                        )))
              (selected (completing-read prompt commands))
              (preset (alist-get selected commands nil nil #'string=)))
         (if (functionp preset)
             (list preset current-prefix-arg)
           (list (or preset selected) current-prefix-arg)
           ))
       )))

  (let ((default-directory (util/commands-root-dir))
        (display-buffer-alist display-buffer-alist))

    (when suppress-output
      (setq display-buffer-alist
            '(("\\*Async Shell Command\\*"    display-buffer-no-window)
              ("\\*Detached Shell Command\\*" display-buffer-no-window))
            ))

    (when (stringp command)
      (setq command (util/commands-replace-placeholders command)))

    (cond
     ((functionp command) (funcall command))
     ((stringp command) (funcall util/commands-string-command-function command))
     )))

(provide 'util-commands)
