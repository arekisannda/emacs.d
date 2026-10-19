;;; diff-hl-show-hunk-display-buffer.el --- diff-hl-show-hunk using display-buffer  -*- lexical-binding: t -*-

;;; Commentary:
;;
;;  This provides `diff-hl-show-hunk-display-buffer' than can be used as
;;  `diff-hl-show-hunk-function'.
;;
;;; Code:

(require 'diff-hl-show-hunk)

(defgroup diff-hl-show-hunk-display-buffer nil
  "Show vc diff using `display-buffer'."
  :group 'diff-hl-show-hunk)

(defvar diff-hl-show-hunk-display-buffer--invoking-command nil "Command that invoked the popup.")
(make-variable-buffer-local 'diff-hl-show-hunk-display-buffer--invoking-command)

(defun diff-hl-show-hunk-display-buffer--hide-no-goto (orig &rest args)
  "Run `diff-hl-show-hunk-hide' without jumping back to the start bookmark.
Only applies when the display-buffer backend is the one being hidden."
  (if (eq diff-hl-show-hunk--hide-function
          #'diff-hl-show-hunk-display-buffer--hide)
      (unwind-protect
          (progn
            (advice-add 'diff-hl-show-hunk--goto-hunk-overlay :override #'ignore)
            (apply orig args))
        (advice-remove 'diff-hl-show-hunk--goto-hunk-overlay #'ignore))
    (apply orig args)))

(defun diff-hl-show-hunk-display-buffer--hide ()
  "Hide the window and clean up buffer."
  (interactive)
  (diff-hl-show-hunk-display-buffer--transient-mode -1)
  (when (get-buffer-window diff-hl-show-hunk-buffer-name)
    (delete-windows-on diff-hl-show-hunk-buffer-name)))

(defvar diff-hl-show-hunk-display-buffer--transient-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [escape]    #'diff-hl-show-hunk-hide)
    (define-key map (kbd "q")   #'diff-hl-show-hunk-hide)
    (define-key map (kbd "C-g") #'diff-hl-show-hunk-hide)
    (set-keymap-parent map diff-hl-show-hunk-map)
    map)
  "Keymap for command `diff-hl-show-hunk-display-buffer--transient-mode'.")

(define-minor-mode diff-hl-show-hunk-display-buffer--transient-mode
  "Temporal minor mode to control diff-hl window."
  :lighter ""
  :global t
  (if diff-hl-show-hunk-display-buffer--transient-mode
      (progn
        (add-hook 'post-command-hook #'diff-hl-show-hunk-display-buffer--post-command-hook nil)
        (advice-add 'diff-hl-show-hunk-hide
                    :around #'diff-hl-show-hunk-display-buffer--hide-no-goto)

        )
    (remove-hook 'post-command-hook #'diff-hl-show-hunk-display-buffer--post-command-hook nil)
    (advice-remove 'diff-hl-show-hunk-hide #'diff-hl-show-hunk-display-buffer--hide-no-goto)
    ))

(defun diff-hl-show-hunk-display-buffer--ignorable-command-p (command)
  "Decide if COMMAND is a command allowed while showing diff."
  (let ((keys (where-is-internal command (list diff-hl-show-hunk-display-buffer--transient-mode-map) t))
        (invoking (eq command diff-hl-show-hunk-display-buffer--invoking-command)))
    (or keys invoking)))

(defun diff-hl-show-hunk-display-buffer--post-command-hook ()
  "Called for each command while in `diff-hl-show-hunk-display-buffer--transient-mode."
  (let ((allowed-command (or (diff-hl-show-hunk-ignorable-command-p this-command)
                             (diff-hl-show-hunk-display-buffer--ignorable-command-p this-command)
                             )))
    (unless allowed-command
      (diff-hl-show-hunk-display-buffer--hide))))

(defun diff-hl-show-hunk-display-buffer--get-ov-at-pos (&optional pos)
  "Return the the diff-hl hunk at POS."
  (diff-hl-hunk-overlay-at (or pos (point))))

(defun diff-hl-hunk-display-buffer--scroll (n)
  "Scroll the diff-hl hunk window N lines (negative = towards top)."
  (when-let* ((buf (get-buffer diff-hl-show-hunk-buffer-name))
              (win (get-buffer-window buf t)))
    (with-selected-window win
      (ignore-errors
        (let (scroll-preserve-screen-position)
          (scroll-up n))))))

(defun diff-hl-show-hunk-display-buffer--poup-up (&optional n)
  (interactive "p")
  (diff-hl-hunk-display-buffer--scroll (- n)))

(defun diff-hl-show-hunk-display-buffer--poup-down (&optional n)
  (interactive "p")
  (diff-hl-hunk-display-buffer--scroll n))

(defvar diff-hl-show-hunk-display-buffer--current-ov nil)

(defun diff-hl-show-hunk-display-buffer--line-move (n)
  (let (line-move-visual
        (scroll-preserve-screen-position))
    (line-move n)
    (when-let ((ov (diff-hl-show-hunk-display-buffer--get-ov-at-pos)))
      (when (and (not (eq ov diff-hl-show-hunk-display-buffer--current-ov))
                 diff-hl-show-hunk-display-buffer--current-ov)
        (diff-hl-show-hunk))
      )))

(defun diff-hl-show-hunk-display-buffer-line-move-up (&optional n)
  (interactive)
  (let (line-move-visual)
    (diff-hl-show-hunk-display-buffer--line-move (- (or n 1)))))

(defun diff-hl-show-hunk-display-buffer-line-move-down (&optional n)
  (interactive)
  (let (line-move-visual)
    (diff-hl-show-hunk-display-buffer--line-move (or n 1))))

;;;###autoload
(defun diff-hl-show-hunk-display-buffer (buffer &optional _line)
  "Implementation to show the hunk using `display-buffer'."
  (setq diff-hl-show-hunk--hide-function #'diff-hl-show-hunk-display-buffer--hide)
  (setq diff-hl-show-hunk-display-buffer--invoking-command this-command)

  (diff-hl-show-hunk-display-buffer--transient-mode 1)

  (display-buffer buffer)
  (let* ((win (get-buffer-window buffer))
         (ov (diff-hl-show-hunk-display-buffer--get-ov-at-pos))
         (type (and ov (overlay-get ov 'diff-hl-hunk-type))))


    (setq diff-hl-show-hunk-display-buffer--current-ov ov)

    (with-current-buffer buffer
      (font-lock-ensure)
      (let ((pos (point-min)))
        (set-window-point win pos)
        (set-window-start win pos))

      (setq-local cursor-type nil
                  cursor-in-non-selected-windows nil)
      )
    ))

(provide 'diff-hl-show-hunk-display-buffer)
;;; diff-hl-show-hunk-display-buffer.el ends here
