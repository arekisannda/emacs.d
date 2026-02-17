;;; org/org-tools.el -*- lexical-binding: t; -*-

(use-package gnuplot :defer t)

(use-package gnuplot-mode :defer t)

(use-package valign
  :defer t
  :custom
  (valign-fancy-bar t))

(use-package edraw :after (org ox)
  :defer t
  :custom
  (edraw-default-document-properties
   '((width . 600)
     (height . 400)
     (background . "#00000000")))
  (edraw-package-default-shape-properties
   `((rect
      (fill . ,(doom-color 'fg-alt))
      (stroke . ,(doom-color 'bg-alt))
      (stroke-width . 1))
     (ellipse
      (fill . ,(doom-color 'fg-alt))
      (stroke . ,(doom-color 'bg-alt))
      (stroke-width . 1))
     (path
      (fill . "none")
      (stroke . ,(doom-color 'bg-alt))
      (stroke-width . 1)
      ;; (marker-end . "arrow")
      )
     (text
      (fill . ,(doom-color 'bg-alt)) ;; Not edraw-package-default-fill
      (font-size . 16)
      (font-family . ,+fonts-variable-pitch-face)
      (text-anchor . "middle"))
     (image)))
  :config
  (require 'edraw-org)
  (edraw-org-setup-default))

(use-package org-download :after org
  :custom
  (org-download-image-dir "./images")
  (org-download-screenshot-method "grim -g \"$(slurp)\" %s"))
