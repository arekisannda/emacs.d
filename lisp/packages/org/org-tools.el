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
