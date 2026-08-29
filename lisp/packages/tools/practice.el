;;; tools/practice.el -*- lexical-binding: t; -*-

(require 'util-windows)

(use-package leetcode
  :load-path "/home/arekisannda/Code/leetcode.el"
  :custom
  (leetcode-prefer-language "python3")
  (leetcode-prefer-sql "postgresql")
  (leetcode-save-solutions t)
  (leetcode-focus t)
  (leetcode-cache-max-age-days 7)
  :init
  (when init-file-debug
    (leetcode-toggle-debug))
  :config
  (defvar leetcode--code-window nil)
  (defvar leetcode--frame nil)

  (defun leetcode-frame (fn &rest r)
    (interactive)
    (let ((parent-frame (selected-frame)))
      (unless (windex-live-visible-frame-p leetcode--frame)
        (setq leetcode--frame
              (make-frame
               (append
                `((no-other-frame . t)
                  (left           . 0.5)
                  (top            . 0.5)
                  (minibuffer     . t)
                  (title          . "Leetcode"))
                ))
              ))
      (select-frame-set-input-focus leetcode--frame t)
      (with-selected-frame leetcode--frame
        (let ((ignore-window-parameters t))
          (with-selected-window (util/windows-get-mru-in-main)
            (delete-other-windows)))
        (switch-to-buffer (get-buffer-create "*new*"))
        (apply fn r))
      ))

  (advice-add #'leetcode :around #'leetcode-frame)
  (advice-add #'leetcode-daily :around #'leetcode-frame)

  (defun +leetcode--maybe-focus ()
    "Delete other windows, keep only *leetcode* buffer."
    (when leetcode-focus
      (delete-other-windows)
      ))

  (advice-add #'leetcode--maybe-focus :override #'+leetcode--maybe-focus)

  (aio-defun leetcode-start-coding-daily (problem-id)
    (let* ((problem (leetcode--get-problem-by-id problem-id))
           (title-slug (leetcode-problem-title-slug problem))
           (problem-with-title (aio-await (leetcode--ensure-question-title problem)))
           (problem-with-content (aio-await (leetcode--ensure-question-content problem)))
           (problem-with-testcases (aio-await (leetcode--ensure-question-testcases problem)))
           (problem-with-snippets (aio-await (leetcode--ensure-question-snippets problem))))
      (leetcode--show-problem problem-with-snippets)
      (with-current-buffer (get-buffer-create (leetcode--detail-buffer-name problem-id))
        (let ((btn (button-at (point))))
          (when btn
            (button-activate btn))))
      ))

  (aio-defun leetcode (&optional force)
    "Start Leetcode."
    (with-selected-frame leetcode--frame
      (when (leetcode--check-deps)
        (if (get-buffer leetcode--buffer-name)
            (switch-to-buffer leetcode--buffer-name)
          (aio-await (leetcode--ensure-login t))
          (aio-await (leetcode-refresh-fetch force))
          (switch-to-buffer leetcode--buffer-name))
        (leetcode--maybe-focus))))

  (aio-defun leetcode-daily ()
    "Open the daily challenge."
    (interactive)
    (unless (get-buffer leetcode--buffer-name)
      (aio-await (leetcode--ensure-login t))
      (aio-await (leetcode-refresh-fetch)))
    (let* ((url-request-method "POST")
           (url-request-extra-headers `(,@(aio-await (leetcode--common-extra-headers))
                                        ,(leetcode--referer leetcode--url-login)))
           (url-request-data
            (json-encode
             `((operationName . "questionOfToday")
               (query . ,leetcode--url-daily-challenge)))))
      (with-current-buffer (url-retrieve-synchronously leetcode--url-graphql)
        (goto-char url-http-end-of-headers)
        (let-alist (json-read)
          (let ((qid .data.activeDailyCodingChallengeQuestion.question.qid))
            (leetcode-start-coding-daily qid))
          ))))

  (defun +leetcode--solving-window-layout-override ()
    (unless (or leetcode-solution-mode
                (derived-mode-p 'leetcode--problem-detail-mode))
      (user-error "not in leetcode session"))

    (let ((windex-layout-buffer-list-apply-function
           (lambda ()
             (if (derived-mode-p 'leetcode--problem-detail-mode)
                 `(,(current-buffer))
               (let ((buffer-list '()))
                 (dolist (w `(,leetcode--description-window
                              ,leetcode--code-window
                              ,leetcode--testcase-window
                              ,leetcode--result-window))
                   (when (window-live-p w)
                     (setq buffer-list (append buffer-list (list (window-buffer w))))))
                 buffer-list))))

          (windex-layout-alist
           '((leetcode
              :description "leetcode layout."
              :tree
              ( :type col
                :nodes
                (( :type buf :apply (lambda (w) (setq leetcode--description-window w)))
                 ( :type buf :select t :apply (lambda (w) (setq leetcode--code-window w)))
                 )
                ))
             )))
      (windex-layout--run-recipe 'leetcode (windex-layout--retrieve-main-window-states))))

  (defun +leetcode--start-coding (problem)
    "Create a buffer for coding PROBLEM.
The buffer will be not associated with any file.  It will choose
major mode by `leetcode-prefer-language'and `auto-mode-alist'."
    (let* ((title (leetcode-problem-title problem))
           (slug-title (leetcode-problem-title-slug problem))
           (problem-id (leetcode-problem-id problem))
           (snippets (leetcode-problem-snippets problem))
           (testcases (leetcode-problem-testcases problem))
           (testcase-buf-name (leetcode--testcase-buffer-name problem-id))
           (result-buf-name (leetcode--result-buffer-name problem-id)))

      ;; Record windows opened for later cleanup.
      (unless (member title leetcode--problem-titles)
        (push title leetcode--problem-titles))

      (leetcode--solving-window-layout)

      ;; Set current programming language.
      (leetcode--set-lang snippets)

      ;; Setup code buffer
      (let* ((code-buf-name (leetcode--get-code-buffer-name title))
             (code-buf (leetcode--get-code-buffer code-buf-name))
             (suffix (assoc-default leetcode--lang leetcode--lang-suffixes)))
        (with-current-buffer code-buf
          (when (= (buffer-size code-buf) 0)
            (let* ((snippet (seq-find (lambda (s)
                                        (equal (leetcode-snippet-lang-slug s) leetcode--lang))
                                      snippets))
                   (template-code (leetcode-snippet-code snippet)))
              (leetcode--insert-code-start-marker)
              (insert template-code)
              (leetcode--insert-code-end-marker)
              (leetcode--replace-in-buffer "" "")))
          (funcall (assoc-default suffix auto-mode-alist #'string-match-p))
          (leetcode-solution-mode t))

        (display-buffer code-buf
                        '((display-buffer-reuse-window
                           leetcode--display-code)
                          (reusable-frames . visible))))

      ;; Setup testcase buffer
      (with-current-buffer (get-buffer-create testcase-buf-name)
        (erase-buffer)
        (insert (s-join "\n" testcases))
        (leetcode--display-testcase (current-buffer)))
      (with-current-buffer (get-buffer-create result-buf-name)
        (erase-buffer)
        (leetcode--display-result (current-buffer)))

      (select-window leetcode--code-window)
      ))

  (advice-add #'leetcode--start-coding :override #'+leetcode--start-coding)

  (defun +leetcode--display-result-override (buffer &optional alist)
    (display-buffer  buffer)
    (run-hook-with-args 'leetcode-setup-functions leetcode--result-window buffer)
    leetcode--result-window)

  (defun +leetcode--display-testcase-override (buffer &optional alist)
    (display-buffer buffer)
    (run-hook-with-args 'leetcode-setup-functions leetcode--testcase-window buffer)
    leetcode--testcase-window)

  (defun +leetcode--display-detail-override (buffer &optional _alist)
    (set-window-buffer leetcode--description-window buffer)
    (run-hook-with-args 'leetcode-setup-functions leetcode--description-window buffer)
    leetcode--description-window)

  (defun +leetcode--display-code-override (buffer &optional _alist)
    (set-window-buffer leetcode--code-window buffer)
    (run-hook-with-args 'leetcode-setup-functions leetcode--code-window buffer)
    leetcode--code-window)

  (aio-defun +leetcode-restore-layout-override ()
    "This command should be run in LeetCode code buffer.
It will restore the layout based on current buffer's name."
    (interactive)
    (let* ((slug-title (leetcode--get-slug-title (current-buffer)))
           (problem (leetcode--get-problem slug-title))
           (problem-id (leetcode-problem-id problem))
           (desc-buf (get-buffer (leetcode--detail-buffer-name problem-id)))
           (testcase-buf (get-buffer-create (leetcode--testcase-buffer-name problem-id)))
           (result-buf (get-buffer-create (leetcode--result-buffer-name problem-id))))
      (leetcode--solving-window-layout)
      (unless desc-buf
        (aio-await (leetcode-show-problem problem-id)))

      (display-buffer desc-buf
                      '((display-buffer-reuse-window
                         leetcode--display-detail)
                        (reusable-frames . visible)))
      (display-buffer testcase-buf
                      '((display-buffer-reuse-window
                         leetcode--display-testcase)
                        (reusable-frames . visible)))
      (display-buffer result-buf
                      '((display-buffer-reuse-window
                         leetcode--display-result)
                        (reusable-frames . visible)))
      (select-window leetcode--code-window)))

  (advice-add #'leetcode-restore-layout :override #'+leetcode-restore-layout-override)
  (advice-add #'leetcode--solving-window-layout :override #'+leetcode--solving-window-layout-override)
  (advice-add #'leetcode--display-result :override #'+leetcode--display-result-override)
  (advice-add #'leetcode--display-testcase :override #'+leetcode--display-testcase-override)
  (advice-add #'leetcode--display-detail :override #'+leetcode--display-detail-override)
  (advice-add #'leetcode--display-code :override #'+leetcode--display-code-override)

  (defun +leetcode-detail-setup ()
    (visual-line-mode 1)
    (word-wrap-whitespace-mode 1))

  :hook
  (leetcode--problem-detail-mode . +leetcode-detail-setup))

(use-package exercism :disabled
  :custom
  (exercism-enable-log-to-message-buffer nil)
  (exercism-open-url-on-submit nil)
  :hook
  (after-init . exercism-setup))
