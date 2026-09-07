;;; emacs/eldoc.el -*- lexical-binding: t; -*-

(require 'util-helpers)

(defun +eldoc-box-max-width ()
  (let ((max-width (ceiling (* (frame-pixel-width) (/ 80.0 (frame-width)))))
        (set-width (ceiling (* (frame-pixel-width) 0.3))))
    (min max-width set-width)))

(defun +eldoc-box-max-height ()
  (let ((max-height (ceiling (* (frame-pixel-height) (/ 20.0 (frame-height)))))
        (set-height (ceiling (* (frame-pixel-height) 0.3))))
    (min max-height set-height)))

(use-package eldoc-box :after (eldoc windex-scroll)
  :custom
  (eldoc-box-max-pixel-width #'+eldoc-box-max-width)
  (eldoc-box-max-pixel-height #'+eldoc-box-max-height)
  :init
  (setq eldoc-box-frame-parameters
        `(;; make the childframe unseen when first created
          (left . -1)
          (top . -1)
          (width  . 0)
          (height  . 0)

          (no-accept-focus . t)
          (no-focus-on-map . t)
          (min-width . 80)
          (min-height . 0)
          (internal-border-width . 1)
          (vertical-scroll-bars . nil)
          (horizontal-scroll-bars . nil)
          (right-fringe . 3)
          (left-fringe . 3)
          (menu-bar-lines . 0)
          (tool-bar-lines . 0)
          (line-spacing . 0)
          (unsplittable . t)
          (undecorated . t)
          (visibility . nil)
          (mouse-wheel-frame . nil)
          (no-other-frame . t)
          (cursor-type . nil)
          (inhibit-double-buffering . t)
          (drag-internal-border . t)
          (no-special-glyphs . t)
          (desktop-dont-save . t)
          (tab-bar-lines . 0)
          (tab-bar-lines-keep-state . 1)))
  :config

  (utils/custom-set-faces
   (eldoc-box-body
    ((nil :inherit default
          :background ,(doom-color 'bg-alt))))
   (eldoc-box-border
    ((nil :inherit popup-border
          :background unspecified
          :foreground unspecified)))
   (eldoc-box-markdown-separator
    ((nil :foreground "#323232")))
   )

  (add-to-list 'eldoc-box-self-insert-command-list 'eldoc-box-scroll-up)
  (add-to-list 'eldoc-box-self-insert-command-list 'eldoc-box-scroll-down)
  (add-to-list 'eldoc-box-self-insert-command-list 'eldoc-box-scroll-left)
  (add-to-list 'eldoc-box-self-insert-command-list 'eldoc-box-scroll-right)

  (defmacro create-eldoc-scroll-defun (direction)
    (let ((dir (symbol-name direction)))
      `(defun ,(intern (format "eldoc-box-scroll-%s" dir)) ()
         ,(format "Scroll %s in `eldoc-box` frame." dir)
         (interactive)
         (if eldoc-box--frame
             (windex-with-selector eldoc-box--frame nil
               (,(intern (concat "windex-scroll-" dir)))
               )))))

  (create-eldoc-scroll-defun up)
  (create-eldoc-scroll-defun down)
  (create-eldoc-scroll-defun right)
  (create-eldoc-scroll-defun left)

  (defun eldoc-box--enable ()
    "Enable eldoc-box hover.
Intended for internal use."
    (if (not (boundp 'eldoc-display-functions))
        (add-function :before-while (local 'eldoc-message-function)
                      #'eldoc-box--eldoc-message-function)
      (setq-local eldoc-box--old-eldoc-functions
                  eldoc-display-functions)
      (remove-hook 'eldoc-display-functions #'eldoc-display-in-echo-area t)
      (add-hook 'eldoc-display-functions #'eldoc-box--eldoc-display-function -90 t))
    (advice-add #'keyboard-quit :before #'eldoc-box-quit-frame))

  (defun +eldoc-decode-entities ()
    (goto-char (point-min))
    (while (re-search-forward "&nbsp;" nil t) (replace-match " "))
    (goto-char (point-min))
    (while (re-search-forward "&lt;" nil t) (replace-match "<"))
    (goto-char (point-min))
    (while (re-search-forward "&gt;" nil t) (replace-match ">"))
    (goto-char (point-min))
    (while (re-search-forward "&amp;" nil t) (replace-match "&"))
    (goto-char (point-min))
    (while (re-search-forward "&quot;" nil t) (replace-match "\"")))

  (defun +eldoc--setup-buffer (buffer-name)
    (let (buf)
      (if (setq buf (get-buffer buffer-name))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (replace-buffer-contents eldoc--doc-buffer)))
        (with-current-buffer eldoc--doc-buffer
          (setq buf (clone-buffer buffer-name nil))))

      (with-current-buffer buf
        (emacs-set-alt-face)
        (setq-local truncate-lines t)
        (local-set-key (kbd "C-c C-o") #'markdown-follow-thing-at-point)
        (visual-line-mode 1)
        (word-wrap-whitespace-mode)
        (rename-buffer buffer-name)
        (goto-char (point-min))
        buf)))

  (defun +eldoc-doc-in-aux (&optional interactive)
    "Get or display ElDoc documentation buffer."
    (interactive (list t))
    (unless (buffer-live-p eldoc--doc-buffer)
      (user-error (format
                   "ElDoc buffer doesn't exist, maybe `%s' to produce one."
                   (substitute-command-keys "\\[eldoc]"))))
    (let* ((init-window (selected-window))
           (uuid (or (window-parameter init-window 'window-aux-id)
                     (util/windows--aux-uuid)))
           (buf-name (format "*eldoc %s*" uuid))
           (buf (+eldoc--setup-buffer buf-name)))
      (set-window-parameter init-window 'window-aux-id uuid)
      (display-buffer buf)))

  (defun +eldoc-doc-in-box (&optional interactive)
    "Get or display ElDoc documentation child frame."
    (interactive (list t))
    (unless (buffer-live-p eldoc--doc-buffer)
      (user-error (format
                   "ElDoc buffer doesn't exist, maybe `%s' to produce one."
                   (substitute-command-keys "\\[eldoc]"))))
    (let* ((buf-name eldoc-box--buffer)
           (buf (+eldoc--setup-buffer buf-name)))
      (let ((eldoc-box-position-function
             eldoc-box-at-point-position-function)
            (doc (with-current-buffer buf
                   (buffer-string))))
        (if (equal doc "")
            (eldoc-box--display "There’s no doc to display at this point")
          (eldoc-box--display doc)))

      (setq eldoc-box--help-at-point-last-point (point))
      (run-with-timer 0.1 nil #'eldoc-box--help-at-point-cleanup)
      (advice-add #'keyboard-quit :before #'eldoc-box-quit-frame)))

  (defun eldoc-box--help-at-point-cleanup ()
    "Try to clean up the childframe."
    (if (eq (point) eldoc-box--help-at-point-last-point)
        (run-with-timer 0.1 nil #'eldoc-box--help-at-point-cleanup)
      (eldoc-box-quit-frame)))

  (defun +eldoc-box-quit-frame ()
    "Hide documentation childframe."
    (interactive)
    (when (and eldoc-box--frame (frame-live-p eldoc-box--frame))
      (setq eldoc-box--show nil)
      (make-frame-invisible eldoc-box--frame t)))

  (advice-add #'eldoc-box-quit-frame :override #'+eldoc-box-quit-frame)

  (defun eldoc--format-doc-buffer-override (docs)
    "Ensure DOCS are displayed in an *eldoc* buffer."
    (with-current-buffer (if (buffer-live-p eldoc--doc-buffer)
                             eldoc--doc-buffer
                           (setq eldoc--doc-buffer
                                 (get-buffer-create " *eldoc*")))
      (let ((inhibit-read-only t)
            (things-reported-on))
        (special-mode)
        (erase-buffer)
        (setq-local nobreak-char-display nil)
        (cl-loop for (docs . rest) on docs
                 for (this-doc . plist) = docs
                 for thing = (plist-get plist :thing)
                 when thing do
                 (cl-pushnew thing things-reported-on)
                 (setq this-doc
                       (concat
                        (propertize (format "%s" thing)
                                    'face (plist-get plist :face))
                        ": "
                        this-doc))
                 do (insert this-doc)
                 when rest do
                 (insert eldoc-doc-buffer-separator)
                 finally
                 (+eldoc-decode-entities)
                 (goto-char (point-min)))

        ;; Rename the buffer, taking into account whether it was
        ;; hidden or not
        (rename-buffer (format "%s*eldoc%s*"
                               (if (string-match "^ " (buffer-name)) " " "")
                               (if things-reported-on
                                   (format " for %s"
                                           (mapconcat
                                            (lambda (s) (format "%s" s))
                                            things-reported-on
                                            ", "))
                                 "")))))
    eldoc--doc-buffer)

  (advice-add #'eldoc--format-doc-buffer :override #'eldoc--format-doc-buffer-override)

  (defun eldoc-display-in-buffer (docs interactive)
    "Display DOCS in a dedicated buffer.
If INTERACTIVE is t, also display the buffer."
    (eldoc--format-doc-buffer docs)
    (when interactive (+eldoc-doc-in-aux t)))

  (defun eldoc-display-in-child-frame (docs interactive)
    "Display DOCS in a child frame.
If INTERACTIVE is t, also display the buffer."
    (eldoc--format-doc-buffer docs)
    (when eldoc-box--show (+eldoc-doc-in-box t)))

  (defvar eldoc-box--show nil)

  (defun +eldoc-box ()
    (interactive)
    (if eldoc-box--show
        (+eldoc-box-quit-frame)
      (setq eldoc-box--show t)
      (setq eldoc--last-request-state nil)
      (eldoc)))

  (defun +eldoc ()
    (interactive)
    (let ((window (selected-window)))
      (when (or (util/windows-side-window-p window)
                (util/windows-popup-window-p window))
        (user-error "Window is cannot be split.")))
    (if-let* ((uuid (window-parameter (util/windows-get-aux-window (selected-window)) 'window-aux-other))
              (buf-name (format "*eldoc %s*" uuid))
              (window (get-buffer-window buf-name)))
        (quit-window nil window)
      (setq eldoc--last-request-state nil)
      (eldoc-box-quit-frame)
      (call-interactively #'eldoc)))

  (setq-default eldoc-display-functions '(eldoc-display-in-child-frame eldoc-display-in-buffer))
  )
