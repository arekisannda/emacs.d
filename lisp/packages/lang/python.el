;;; lang/python.el -*- lexical-binding: t; -*-

(require 'util-lang)
(require 'util-helpers)

(defvar +lang-python-format-command "ruff"
  "Python formatting command.")

(defun +lang-python-flymake-setup ()
  "Setup to run for `flymake-ruff`."
  (when-let ((exec (executable-find "ruff")))
    (setq-local flymake-ruff-program exec)
    (flymake-ruff-load)))

(defun +lang-python-mode-setup ()
  (remove-hook 'flymake-diagnostic-functions #'python-flymake t)
  (when (+envrc-root)
    (add-hook '+envrc-update-hook #'+lang-python-flymake-setup nil t))
  (+lang-python-flymake-setup))

(defun +lang-python-format-buffer ()
  "Format the current Python buffer using `ruff` before saving."
  (interactive)
  (unless (derived-mode-p 'python-mode 'python-base-mode)
    (user-error "Only python buffers can be linted with ruff"))
  (python-sort-imports)
  (let* ((tmpfile (make-temp-file "python-tmp" nil ".py"))
         (patchbuf (get-buffer-create " *python format patch*"))
         (errbuf (get-buffer-create " *python format error*"))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (ruff-format-diff-args (list "format" "--diff" "-s" tmpfile))
         (ruff-format-args (list "format" "-s" tmpfile)))

    (unwind-protect
        (save-restriction
          (widen)

          (let ((inhibit-message t))
            (write-region nil nil tmpfile))

          (with-current-buffer errbuf
            (setq buffer-read-only nil)
            (erase-buffer))

          (with-current-buffer patchbuf
            (erase-buffer))

          (if (zerop (apply #'process-file +lang-python-format-command nil nil nil ruff-format-args))
              (progn
                (if (zerop (util/call-diff "diff" tmpfile patchbuf (point-min) (point-max)))
                    (message "Buffer is already formatted")
                  (util/apply-rcs-patch-to-buffer patchbuf)))
            (message "%s" (with-current-buffer errbuf (buffer-string)))))

      (kill-buffer patchbuf)
      (delete-file tmpfile))))

(use-package python
  :custom
  (python-indent-offset 4)
  (python-indent-guess-indent-offset-verbose nil)
  :config
  (util/update-alist
   'major-mode-remap-alist
   '((python-mode . python-ts-mode)))
  :hook
  (python-ts-mode . +lang-python-mode-setup))

(use-package flymake-ruff)
