;;; justcmd.el --- Run justfile recipes  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Use `just' recipes in `util-commands-run-command'
;;
;;; Code:

(require 'cl-lib)
(require 'map)

(defgroup justcmd nil
  "Run just recipes."
  :group 'tools
  :prefix "justcmd-")

(defcustom justcmd-executable "just"
  "Name or path of the `just' executable."
  :type 'string
  :group 'justcmd
  :risky t)

(defcustom justcmd-default-shell-command #'async-shell-command
  "Default function to execute commands."
  :type 'function)

(defun justcmd--root ()
  "Directory of the nearest justfile, searching upward."
  (locate-dominating-file
   default-directory
   (lambda (d)
     (seq-some (lambda (f) (file-exists-p (expand-file-name f d)))
               '("justfile" ".justfile")))))

;;TODO: add cache garbage collection
(defvar justcmd--cache (make-hash-table :test #'equal)
  "Map justfile root to plist with (:sig SIG :dump DUMP [SLOT VALUE]...).
All values are shared: callers must not mutate them.")

(defun justcmd--project ()
  "Return the cache entry for the current justfile.
Runs `just --dump' every call; re-parses only when the output changed."
  (when-let* ((dir (justcmd--root)))
    (with-temp-buffer
      (let* ((default-directory dir)
             (status (process-file justcmd-executable nil (list t nil) nil
                                   "--dump" "--dump-format" "json")))
        (unless (eq status 0)
          (user-error "`just --dump' failed (exit %s)" status)))
      (let ((sig (buffer-hash))
            (hit (gethash dir justcmd--cache)))
        (if (and hit (equal (plist-get hit :sig) sig))
            hit
          (goto-char (point-min))
          (puthash dir
                   (list :sig sig
                         :dump (json-parse-buffer :object-type 'hash-table
                                                  :null-object nil
                                                  :false-object nil))
                   justcmd--cache))))))

(defun justcmd--project-slot (project slot compute)
  "Return SLOT of the current project cache, computing it once via COMPUTE.
COMPUTE is called with the parsed dump.  A nil result is cached too."
  (if (plist-member project slot)
      (plist-get project slot)
    (let ((val (funcall compute project)))
      (nconc project (list slot val))
      val)))

(defun justcmd--dump ()
  "Parsed `just --dump' for the current justfile."
  (plist-get (justcmd--project) :dump))

(defun justcmd-clear-cache ()
  "Empty the `just --dump' cache."
  (interactive)
  (clrhash justcmd--cache))

(defun justcmd--walk-recipes (tree pred prefix)
  "Unsorted alist of (\"mod::name\" . RECIPE) in TREE matching PRED."
  (nconc
   (when-let* ((recipes (gethash "recipes" tree)))
     (cl-loop for name being the hash-keys of recipes
              using (hash-values recipe)
              when (funcall pred recipe)
              collect (cons (concat prefix name) recipe)))
   (when-let* ((mods (gethash "modules" tree)))
     (cl-loop for mod being the hash-keys of mods
              using (hash-values sub)
              nconc (justcmd--walk-recipes sub pred (concat prefix mod "::"))))))

(defun justcmd--collect-recipes (tree pred &optional prefix)
  "Sorted alist of (\"mod::name\" . RECIPE) for recipes in TREE matching PRED."
  (sort (justcmd--walk-recipes tree pred prefix)
        (lambda (a b) (string< (car a) (car b)))))

(defun justcmd--build-recipes (project)
  (justcmd--collect-recipes (plist-get project :dump) (lambda (r) (not (gethash "private" r)))))

(defun justcmd--build-private-recipes (project)
  (justcmd--collect-recipes (plist-get project :dump) (lambda (r) (gethash "private" r))))

(defun justcmd--recipes (project)
  (justcmd--project-slot project :recipes #'justcmd--build-recipes))

(defun justcmd--private-recipes (project)
  (justcmd--project-slot project :private #'justcmd--build-private-recipes))

(defun justcmd--collect-recipe-attributes (recipe &optional attr-type pred)
  (cl-loop for attr-table across (gethash "attributes" recipe)
           for attr = (and (hash-table-p attr-table) (gethash attr-type attr-table))
           when (and attr
                     (or (not (functionp pred))
                         (funcall pred attr)
                         ))
           collect attr))

(defun justcmd--recipe-documentation (recipe)
  (if-let* ((doc (justcmd--collect-recipe-attributes recipe "doc")))
      (car doc)
    ""))

(defun justcmd--recipe-runner (recipe)
  (when-let* ((runner (justcmd--collect-recipe-attributes
                       recipe "metadata"
                       (lambda (attr)
                         (and (vectorp attr)
                              (equal (seq-take attr 2) ["emacs" "runner"]))
                         )))
              (fn (aref (car runner) 2))
              (sym (intern-soft fn)))
    (if (and sym (functionp sym))
        sym
      (error "Invalid runner function: %S" fn))))

(defun justcmd--recipe-parameters (recipe)
  (cl-loop for param across (gethash "parameters" recipe)
           collect (gethash "name" param)))

(defun justcmd--intern (param context fn)
  (let ((sym (intern-soft fn)))
    (if (and sym (functionp sym))
        sym
      (error "Invalid metadata: (emacs %S %S <%S> ...) is not a valid elisp function" param context fn))))

(defun justcmd--recipe-parameters-spec (recipe)
  (let ((params-map (make-hash-table :test #'equal)))
    (dolist (meta (justcmd--collect-recipe-attributes
                   recipe
                   "metadata"
                   (lambda (attr)
                     (and (vectorp attr)
                          (equal (seq-take attr 1) ["emacs"]))
                     )))
      (pcase-let ((`(,context . ,value)
                   (pcase (append meta nil)
                     (`(,_ ,param "completion" ,fn ,prompt)
                      `(completion
                        (function . ,(justcmd--intern param "completion" fn))
                        (prompt . ,prompt)))

                     (`(,_ ,param "completion-source" ,type ,cmd . ,args)
                      `(completion-source
                        (type . ,type)
                        (command . ,(pcase type
                                      ("shell" cmd)
                                      ("elisp" (justcmd--intern param "completion-source" cmd))
                                      (_ (error "Invalid completion-source type: %S" type))))
                        (args . ,args)))

                     (`(,_ ,_ "placeholder" ,text)
                      `(placeholder . ,text))
                     )))
        (if context
            (setf (alist-get context (gethash (aref meta 1) params-map)) value)
          (message "justcmd: ignoring metadata %S" meta))))
    params-map))

(defun justcmd--recipe-spec (project recipe)
  "Param map for RECIPE in ENTRY, computed on first use.
RECIPE must come from ENTRY's dump; lookup is by object (`eq')."
  (let ((attrmap (justcmd--project-slot project :recipe-specs
                                        (lambda (_project)
                                          (make-hash-table :test #'eq)))))
    (or (gethash recipe attrmap)
        (puthash recipe
                 (map-into
                  `(("doc" . ,(justcmd--recipe-documentation recipe))
                    ("runner" . ,(justcmd--recipe-runner recipe))
                    ("params-list" . ,(justcmd--recipe-parameters recipe))
                    ("params-spec" . ,(justcmd--recipe-parameters-spec recipe)))
                  '(hash-table :test equal))
                 attrmap)
        )))

(defun justcmd--params-completion (complete-fn param prompt collection)
  (let* ((prompt (or prompt (format "%s: " param))))
    (pcase complete-fn
      ('completing-read (list (completing-read prompt collection)))
      ('completing-read-multiple (delete-dups (completing-read-multiple prompt collection)))
      (_ (error "Unknown completing function: %S" complete-fn)))
    ))

(defun justcmd--shell-source-collector (command &optional args)
  (let* ((default-directory (justcmd--root)))
    (apply #'process-lines-handling-status command nil args)))

(defun justcmd--elisp-source-collector (fn &optional args)
  (let* ((default-directory (justcmd--root)))
    (apply fn args)))

(defconst justcmd--source-collectors
  '(("shell" . justcmd--shell-source-collector)
    ("elisp" . justcmd--elisp-source-collector)))

(defun justcmd--compute-param-value (param &optional spec)
  (if (null spec)
      "''"
    (let-alist spec
      (cond
       (.placeholder .placeholder)
       (.completion
        (let* ((collector (alist-get .completion-source.type justcmd--source-collectors nil nil #'equal)))
          (mapconcat
           #'shell-quote-argument
           (justcmd--params-completion
            .completion.function
            param
            .completion.prompt
            (funcall collector .completion-source.command .completion-source.args))
           " ")
          )))
      )))

(defun justcmd--build-commands (project)
  (let* ((recipes (justcmd--recipes project)))
    (cl-loop for command in (mapcar #'car recipes)
             collect
             (let* ((spec (justcmd--recipe-spec project (alist-get command recipes nil nil #'string=)))
                    (cmd command))
               (cons
                cmd
                `( :doc ,(gethash "doc" spec)
                   :command
                   ,(lambda ()
                      (let* ((params (gethash "params-list" spec))
                             (runner (or (gethash "runner" spec)
                                         justcmd-default-shell-command))
                             (args (mapconcat
                                    (lambda (p)
                                      (justcmd--compute-param-value
                                       p (map-nested-elt spec (list "params-spec" p))))
                                    params
                                    "")))
                        (funcall runner (format "%s %s %s" justcmd-executable cmd args))
                        ))
                   ))
               ))
    ))

(defun justcmd-commands-list ()
  (when-let* ((project (justcmd--project)))
    (justcmd--build-commands project)))

(defun justcmd--run-command-candidates (cand)
  (list (car cand) `(:info ,(plist-get (cdr cand) :doc))))

(defun justcmd--run-command-annotations (command &optional column-width)
  (let* ((params (car (last (assoc command minibuffer-completion-table))))
         (info (or (plist-get params :info) "")))
    (concat " "
            (propertize " " 'display `(space :align-to ,(or column-width 0)))
            (util/strings-add-font-lock (copy-sequence info) 'font-lock-comment-face))))

(defun justcmd-run (&optional dir)
  (interactive
   (list (when current-prefix-arg
           (read-directory-name "Run in directory: " nil nil t))))
  (let* ((default-directory (or dir default-directory))
         (commands (justcmd-commands-list))
         (prompt (format "Run command [%s]: " (justcmd--root)))
         (column-width (+ 5 (cl-loop for (key . _) in commands
                                     maximize (string-width key) into max
                                     finally return (or max 0))))
         (selected
          (let ((completion-extra-properties
                 `(:annotation-function
                   ,(lambda (cand)
                      (justcmd--run-command-annotations cand column-width)))))
            (string-trim
             (completing-read
              prompt
              (mapcar #'justcmd--run-command-candidates commands)
              nil t)))))
    (thread-first (alist-get selected commands nil nil #'equal)
                  (plist-get :command)
                  funcall)
    ))

(provide 'justcmd)
;;; justcmd.el ends here
