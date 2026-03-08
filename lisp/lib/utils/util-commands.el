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

(defun util/commands-run-command (command &optional suppress-output)
  "Run preset command."
  (interactive
   (with-temp-buffer
     (let ((default-directory default-directory))
       (unless buffer-file-name
         (hack-dir-local-variables-non-file-buffer))
       (setq commands (buffer-local-value 'util/commands-command-list (current-buffer)))

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
         (list (util/commands--quote-command (or preset selected)) current-prefix-arg))
       )))

  (cond
   ((functionp command) (funcall command))
   ((stringp command) (funcall util/commands-string-command-function command suppress-output))
   ))

(provide 'util-commands)
