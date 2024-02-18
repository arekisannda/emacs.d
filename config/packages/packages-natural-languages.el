;;; packages-natural-languages.el --- Natural Language Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package google-translate :ensure t)

(use-package mozc :ensure t)

(use-package mozc-cand-posframe :ensure t :after mozc :defer t)

(use-package migemo :ensure t :disabled)

(provide 'packages-natural-languages)

(elpaca-wait)

(defun mozc-cand-posframe-update (candidates)
  "Update the candidate window using posframes.
CANDIDATES must be the candidates field in a response protobuf."
  (let* ((current-index (mozc-protobuf-get candidates 'focused-index))
         (total (mozc-protobuf-get candidates 'size))
         (index-visible (mozc-protobuf-get candidates 'footer 'index-visible))
         (source (mozc-protobuf-get candidates 'candidate))
         candidate
         (before-current (let (result)
                           (when current-index
                             (while (and (setq candidate (pop source))
                                         (< (mozc-protobuf-get candidate 'index) current-index))
                               (push (mozc-cand-posframe--make-item candidate) result)))
                           (nreverse result)))
         (current (when (and current-index
                             candidate
                             (= (mozc-protobuf-get candidate 'index) current-index))
                    (mozc-cand-posframe--make-item candidate)))
         (after-current (mapcar #'mozc-cand-posframe--make-item source))
         (x-pixel-offset (+ (- (car (window-text-pixel-size nil
                                                            (overlay-start mozc-preedit-overlay)
                                                            (overlay-end mozc-preedit-overlay))))
                            (line-number-display-width t)))
         (posframe-width (apply #'max (mapcar #'mozc-cand-posframe-candidate-width
                                              (append before-current
                                                      (when current
                                                        (list current))
                                                      after-current))))
         (modeline (when (and index-visible current-index total
                              (> total 1))
                     (s-pad-left posframe-width " "
                                 (format "%d/%d" (1+ current-index) total)))))
    (with-current-buffer (get-buffer-create mozc-cand-posframe-buffer)
      (goto-char (point-min))
      (cl-labels ((format-candidate (cand)
                    (concat (mozc-cand-posframe-candidate-lstr cand)
                            (if (mozc-cand-posframe-candidate-rstr cand)
                                (concat mozc-cand-posframe-separator
                                        (make-string (- posframe-width
                                                        (mozc-cand-posframe-candidate-width cand))
                                                     ?\s)
                                        (mozc-cand-posframe-candidate-rstr cand))
                              "")
                            "\n"))
                  (put-face-overlay (begin end face)
                    (overlay-put (make-overlay begin end) 'face face)))
        (when before-current
          (insert (mapconcat #'format-candidate before-current "")))
        (put-face-overlay (point-min) (point) 'mozc-cand-posframe-normal-face)
        (when current
          (let ((begin (point)))
            (insert (format-candidate current))
            (put-face-overlay begin (point) 'mozc-cand-posframe-focused-face)))
        (when after-current
          (let ((begin (point)))
            (insert (mapconcat #'format-candidate after-current ""))
            (put-face-overlay begin (point) 'mozc-cand-posframe-normal-face))))
      (delete-region (point) (point-max))
      (when modeline
        (setq-local mode-line-format
                    (list (propertize modeline 'face 'mozc-cand-posframe-footer-face)))))
    (posframe-show mozc-cand-posframe-buffer
                   :foreground-color (face-foreground 'mozc-cand-posframe-normal-face nil t)
                   :background-color (face-background 'mozc-cand-posframe-normal-face nil t)
                   :poshandler 'posframe-poshandler-point-bottom-left-corner
                   :respect-mode-line (not (null modeline))
                   :x-pixel-offset x-pixel-offset
                   :y-pixel-offset mozc-cand-posframe-y-pixel-offset)))

(advice-add 'mozc-cand-posframe-update :after)

;;; packages-natural-languages.el ends here
