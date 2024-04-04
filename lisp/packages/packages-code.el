;;; packages-code.el --- Coding Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package leetcode
  :custom
  (leetcode-prefer-language "golang")
  (leetcode-prefer-sql "mysql")
  (leetcode-save-solutions t)
  (leetcode-directory (expand-file-name "~/Code/leetcode"))
  :hook
  (leetcode-solution-mode . (lambda () (eglot--managed-mode -1)))
  :config
  (defun +leetcode--solving-window-layout-override ()
    (delete-other-windows)
    (setq leetcode--description-window (selected-window))
    (setq leetcode--testcase-window (split-window-below))
    (other-window 1)
    (setq leetcode--result-window (split-window-below))
    (setq leetcode--code-window (split-root-window-right))
    (other-window 1)
    (other-window 1))

  (defun +leetcode--display-result-override (buffer &optional alist)
    (set-window-buffer leetcode--result-window buffer)
    leetcode--result-window)

  (defun +leetcode--display-testcase-override (buffer &optional alist)
    (set-window-buffer leetcode--testcase-window buffer)
    leetcode--testcase-window)

  (defun +leetcode--display-detail-override (buffer &optional alist)
    (set-window-buffer leetcode--description-window buffer)
    leetcode--description-window)

  (defun +leetcode--display-code-override (buffer &optional alist)
    (set-window-buffer leetcode--code-window buffer)
    leetcode--code-window)

  (advice-add #'leetcode--solving-window-layout :override #'+leetcode--solving-window-layout-override)
  (advice-add #'leetcode--display-result :override #'+leetcode--display-result-override)
  (advice-add #'leetcode--display-testcase :override #'+leetcode--display-testcase-override)
  (advice-add #'leetcode--display-detail :override #'+leetcode--display-detail-override)
  (advice-add #'leetcode--display-code :override #'+leetcode--display-code-override))

(use-package exercism
  :custom
  (exercism-display-tests-after-run t)
  (exercism-directory (expand-file-name "~/Code/exercism"))
  :config
  (defun exercism-setup (&optional config-file-path)
    (interactive)
    (let* ((config-file-path (or config-file-path "~/.config/exercism/user.json")))
      (unless (file-exists-p config-file-path)
        (user-error "Failed to laod config: invalid file"))
      (with-temp-buffer
        (insert-file-contents config-file-path)
        (let ((config (json-parse-string (buffer-string))))
          (setq exercism--workspace (gethash "workspace" config))
          (setq exercism--api-token (gethash "token" config))))))

  (defun exercism-get-exercise-id ()
    (interactive)
    (let* ((track-exercise (concat exercism--current-track "/" exercism--current-exercise))
           (exercise-path (expand-file-name track-exercise exercism--workspace))
           (exercise-metadata-path (expand-file-name ".exercism/metadata.json" exercise-path))
           (exercise-id))
      (unless (file-exists-p exercise-metadata-path)
        (user-error "Failed to laod metadata: invalid file"))
      (with-temp-buffer
        (insert-file-contents exercise-metadata-path)
        (let ((config (json-parse-string (buffer-string))))
          (setq exercise-id (gethash "id" config))
          ))
      exercise-id))

  (defun exercism-complete ()
    "Mark exercise as completed."
    (interactive)
    (let* ((exercise-id (exercism-get-exercise-id)))
      (promise-new
       (lambda (resolve _)
         (request
           (concat "https://exercism.org/api/v2/solutions/" exercise-id "/complete")
           :type "PATCH"
           :headers `(("Authorization" . ,(format "Bearer %s" exercism--api-token)))
           :parser #'json-read
           :success (cl-function
                     (lambda (&key data &allow-other-keys)
                       (message "Response: %s" data)))
           :error (cl-function
                   (lambda (&key err &allow-other-keys)
                     (message "Request failed: %S" err)))
           )))
      ))

  (defun exercism-publish ()
    "Publish exercise solution."
    (interactive)
    (let* ((exercise-id (exercism-get-exercise-id)))
      (promise-new
       (lambda (resolve _)
         (request
           (concat "https://exercism.org/api/v2/solutions/" exercise-id "/publish")
           :type "PATCH"
           :headers `(("Authorization" . ,(format "Bearer %s" exercism--api-token)))
           :parser #'json-read
           :success (cl-function
                     (lambda (&key data &allow-other-keys)
                       (message "here!!!")
                       (message "Response: %s" data)))
           :error (cl-function
                   (lambda (&key err &allow-other-keys)
                     (message "Request failed: %S" err)))
           )))
      ))
  )

(provide 'packages-code)

;;; packages-code.el ends here
