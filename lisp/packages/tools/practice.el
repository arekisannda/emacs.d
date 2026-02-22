;;; tools/practice.el -*- lexical-binding: t; -*-

(require 'util-windows)

(use-package leetcode
  :load-path "/home/arekisannda/Code/leetcode.el"
  :custom
  (leetcode-prefer-language "python")
  (leetcode-prefer-sql "postgresql")
  (leetcode-save-solutions t)
  (leetcode-focus t)
  (leetcode-cache-max-age-days 7)
  :init
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
      (with-selected-frame leetcode--frame (apply fn r))
      ))

  (advice-add #'leetcode :around #'leetcode-frame)
  (advice-add #'leetcode-daily :around #'leetcode-frame)

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

  (aio-defun leetcode-daily ()
    "Open the daily challenge."
    (interactive)
    (unless (get-buffer leetcode--buffer-name)
      (aio-await (leetcode--ensure-login))
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
                 ( :type row
                   :nodes
                   ((:type buf :apply (lambda (w) (setq leetcode--testcase-window w)))
                    (:type buf :apply (lambda (w) (setq leetcode--result-window w)))
                    ))
                 )
                ))
             )))
      (windex-layout--run-recipe 'leetcode)))

  (defun +leetcode--display-result-override (buffer &optional alist)
    (set-window-buffer leetcode--result-window buffer)
    leetcode--result-window)

  (defun +leetcode--display-testcase-override (buffer &optional alist)
    (set-window-buffer leetcode--testcase-window buffer)
    leetcode--testcase-window)

  (defun +leetcode--display-detail-override (buffer &optional _alist)
    (set-window-buffer leetcode--description-window buffer)
    leetcode--description-window)

  (defun +leetcode--display-code-override (buffer &optional _alist)
    (set-window-buffer leetcode--code-window buffer)
    leetcode--code-window)

  (advice-add #'leetcode--solving-window-layout :override #'+leetcode--solving-window-layout-override)
  (advice-add #'leetcode--display-result :override #'+leetcode--display-result-override)
  (advice-add #'leetcode--display-testcase :override #'+leetcode--display-testcase-override)
  (advice-add #'leetcode--display-detail :override #'+leetcode--display-detail-override)
  (advice-add #'leetcode--display-code :override #'+leetcode--display-code-override))

(use-package exercism :disabled
  :custom
  (exercism-enable-log-to-message-buffer nil)
  (exercism-open-url-on-submit nil)
  :hook
  (after-init . exercism-setup))
