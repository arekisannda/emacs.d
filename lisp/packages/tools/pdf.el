;;; tools/pdf.el -*- lexical-binding: t; -*-

(use-package pdf-tools
  :defer t
  :custom
  (pdf-view-display-size 'fit-page)
  :mode
  ("\\.pdf\\'" . pdf-view-mode)
  :hook
  (pdf-view-mode . auto-revert-mode)
  (pdf-view-mode . pdf-view-midnight-minor-mode)
  (after-init    . pdf-loader-install))
