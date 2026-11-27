;;; org/org-tools.el -*- lexical-binding: t; -*-

(use-package gnuplot :defer t)

(use-package gnuplot-mode :defer t)

(use-package valign
  :defer t
  :custom
  (valign-fancy-bar t))

(use-package org-download :after org
  :custom
  (org-download-image-dir "./images")
  (org-download-screenshot-method "grim -g \"$(slurp)\" %s"))

(use-package om-dash
  :custom
  (om-dash-table-squeeze-empty nil)
  :config
  (defun +om-dash-shrink-after-update (orig &rest args)
    "Shrink columns with width cookies of an om-dash block after updating it."
    (let ((begin (save-excursion (org-beginning-of-dblock) (point-marker))))
      (prog1 (apply orig args)
        (save-excursion
          (goto-char begin)
          (when (looking-at "[ \t]*#\\+BEGIN:[ \t]+om-dash-")
            (let ((end (save-excursion (re-search-forward "^[ \t]*#\\+END:" nil t))))
              (while (re-search-forward "^[ \t]*|" end t)
                (org-table-shrink)
                (goto-char (org-table-end)))))))))

  (advice-add 'org-update-dblock :around #'+om-dash-shrink-after-update)
  )
