;;; tools/w3m.el -*- lexical-binding: t; -*-

(use-package w3m
  :custom
  (w3m-search-default-engine "duckduckgo")
  (w3m-default-display-inline-images t)
  (w3m-display-mode 'plain)
  (w3m-home-page "about:blank")
  (w3m-use-header-line t)
  (w3m-use-header-line-title nil)
  (w3m-use-tab nil)
  (w3m-use-tab-line nil)
  :custom-face
  (w3m-anchor
   ((nil :foreground ,(doom-lighten (doom-color 'dark-blue) 0.0))))
  (w3m-arrived-anchor
   ((nil :foreground ,(doom-darken (doom-color 'dark-blue) 0.3))))
  (w3m-insert
   ((nil :foreground ,(doom-color 'violet))))
  (w3m-error
   ((nil :foreground ,(doom-color 'error))))
  (w3m-form
   ((nil :foreground ,(doom-color 'magenta))))
  (w3m-form-button
   ((nil :foreground ,(doom-color 'yellow))))
  (w3m-form-button-mouse
   ((nil :foreground ,(doom-color 'yellow))))
  (w3m-form-button-pressed
   ((nil :foreground ,(doom-color 'orange))))
  (w3m-header-line-background
   ((nil :background ,(doom-color 'bg-alt))))
  (w3m-header-line-content
   ((nil :foreground ,(doom-color 'fg)
         :background ,(doom-color 'bg-alt))))
  (w3m-header-line-title
   ((nil :foreground ,(doom-color 'fg)
         :background ,(doom-color 'bg-alt))))
  (w3m-image
   ((nil :foreground ,(doom-color 'green))))
  (w3m-image-anchor
   ((nil :foreground ,(doom-color 'green)
         :background ,(doom-blend (doom-color 'green) (doom-color 'bg-alt) 0.1))))
  (w3m-unsafe-url-warning
   ((nil :foreground ,(doom-color 'bg-alt)
         :background ,(doom-color 'red))))
  :config
  (require 'mime-w3m)
  (require 'w3m-filter)
  (defun w3m-filter-readability (url)
    (let* ((u (url-generic-parse-url url))
           (proto (url-type u))
           (host (url-host u))
           (readable-cmd (format "readable --quiet --base '%s://%s' -" proto host)))
      (shell-command-on-region (point-min) (point-max) readable-cmd nil t)
      (goto-char (point-min))))

  (dolist (url-regexp '(("www.etymonline.com" . "\\`https?://[a-z]+\\.etymonline\\.")))
    (add-to-list
     'w3m-filter-configuration
     `(t ,(format "Readability for %s" (car url-regexp)) ,(cdr url-regexp) w3m-filter-readability)))

  (defun +w3m-doc-view (url)
    "View PDF/PostScript/DVI files using `pdf-view-mode'.

Where the document is displayed depends upon the `w3m-display-mode'."
    (let* ((basename (file-name-nondirectory (w3m-url-strip-query url)))
           (regexp (concat "\\`" (regexp-quote basename) "\\(?:<[0-9]+>\\)?\\'"))
           (buffers (buffer-list))
           buffer data case-fold-search)
      (save-current-buffer
        (while buffers
          (setq buffer (pop buffers))
          (if (and (string-match regexp (buffer-name buffer))
                   (progn
                     (set-buffer buffer)
                     (eq major-mode 'pdf-view-mode))
                   (equal buffer-file-name url))
              (setq buffers nil)
            (setq buffer nil))))
      (unless (prog1
                  buffer
                (unless buffer
                  (setq buffer (generate-new-buffer basename)
                        data (buffer-string)))
                (let ((pop-up-windows w3m-pop-up-windows)
                      (pop-up-frames w3m-pop-up-frames))
                  (pop-to-buffer buffer)))
        (set-buffer-multibyte nil)
        (insert data)
        (set-buffer-modified-p nil)
        (setq buffer-file-name url)
        (pdf-view-mode)
        (use-local-map w3m-doc-view-map)
        (set-keymap-parent w3m-doc-view-map doc-view-mode-map)
        'internal-view)))

  (advice-add #'w3m-doc-view :override #'+w3m-doc-view)

  :hook
  (w3m-mode . visual-line-mode)
  (w3m-mode . word-wrap-whitespace-mode))
