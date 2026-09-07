;;; editor/fold.el -*- lexical-binding: t; -*-

(require 'util-lang)
(require 'mule-util)

(setq +fold-replacement (concat " " (truncate-string-ellipsis) " "))

(defcustom elisp-initial-hide-level '()
  "Set initial hide level ."
  :type '(repeat (cons (choice (const t) directory file)
                       (choice (const nil) (integer :tag "Level"))))
  :group 'hideshow)

(defun elisp-hide-check-level (buffer-name entry)
  "Check BUFFER-NAME against ENTRY predicate."
  (let* ((pred (car entry))
         (level (cdr entry)))
    (cond
     ((and (booleanp pred) pred) t)
     ((string-empty-p buffer-name) nil)
     ((and (directory-name-p pred) (file-in-directory-p buffer-name pred)) t)
     ((and (string= pred (file-name-nondirectory buffer-name))) t)
     )))

(defun elisp-hide-level ()
  "Set initial buffer fold using rules in `elisp-initial-hide-level'."
  (let* ((fn (apply-partially #'elisp-hide-check-level (or (buffer-file-name) "")))
         (level (cdr (cl-find-if fn elisp-initial-hide-level))))
    (save-excursion
      (goto-char (point-min))
      (when level (hs-hide-level level)))))

(defun buffer-toggle-display-line-numbers ()
  (when display-line-numbers-mode
    (pcase display-line-numbers-type
      ('relative (setq-local display-line-numbers-type t))
      (_ (setq-local display-line-numbers-type 'relative))
      )))

(use-package treesit-fold :after treesit
  :custom
  (treesit-fold-replacement +fold-replacement)
  :config
  (utils/custom-set-faces
   (treesit-fold-replacement-face
    ((nil :inherit fold-replacement-face
          :box unspecified
          :weight unspecified
          :foreground unspecified
          :background unspecified
          )))
   )
  :hook
  (treesit-fold-mode-on . (lambda () (setq-local util/fold-type 'treesit-fold
                                                 util/fold-show #'treesit-fold-open
                                                 util/fold-show-all #'treesit-fold-open-all
                                                 util/fold-hide #'treesit-fold-close)))
  (treesit-fold-mode-off . (lambda () (setq-local util/fold-type nil
                                                  util/fold-show nil
                                                  util/fold-show-all nil
                                                  util/fold-hide nil))))

(defun +hs-mode-fold-overlay (ov)
  "Format fold overlay OV."
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put
     ov
     'display
     (propertize +fold-replacement 'face 'treesit-fold-replacement-face))))

(use-package hideshow
  :defer t
  :custom
  (hs-set-up-overlay #'+hs-mode-fold-overlay)
  :hook
  (hs-minor-mode     . elisp-hide-level)
  (hs-minor-mode-on  . (lambda () (setq-local util/fold-type 'hs
                                              util/fold-show #'hs-show-block
                                              util/fold-show-all #'hs-show-all
                                              util/fold-hide #'hs-hide-block)))
  (hs-minor-mode-off . (lambda () (setq-local util/fold-type nil
                                              util/fold-show nil
                                              util/fold-show-all nil
                                              util/fold-hide nil))))
