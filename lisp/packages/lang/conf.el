;;; lang/conf.el -*- lexical-binding: t; -*-

(use-package toml-ts-mode
  :mode
  ("\\.toml\\'" . toml-ts-mode)
  :hook
  (toml-ts-mode . diff-hl-mode)
  (toml-ts-mode . display-line-numbers-mode)
  (toml-ts-mode . rainbow-delimiters-mode)
  (toml-ts-mode . +lang-conf-mode-setup))

(use-package yaml-ts-mode
  :custom
  (yaml-indent-offset 2)
  :mode
  ("\\.ya?ml\\'" . yaml-ts-mode)
  :hook
  (yaml-ts-mode . diff-hl-mode)
  (yaml-ts-mode . display-line-numbers-mode)
  (yaml-ts-mode . rainbow-delimiters-mode)
  (yaml-ts-mode . +lang-conf-mode-setup))
