;;; lang/cmake.el -*- lexical-binding: t; -*-

(use-package cmake-mode
  :mode
  ("CMakeLists\\.txt\\'" . cmake-mode))

(use-package meson-mode :defer t)
