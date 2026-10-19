;;; tools/excali.el -*- lexical-binding: t; -*-

(defun excali-mode-setup ()
  (setq excali--theme 'dark)
  (when excali--native-cache (clrhash excali--native-cache))
  (excali--render))

(use-package excali
  :config
  (defun +excali--empty-doc ()
    "Return an empty .excalidraw document, as upstream writes one."
    (list (cons 'type "excalidraw")
          (cons 'version 2)
          (cons 'source "https://excalidraw.com")
          (cons 'elements [])
          (cons 'appState (list (cons 'gridSize 20)
                                (cons 'gridStep 5)
                                (cons 'gridModeEnabled :false)
                                (cons 'viewBackgroundColor "#e4e1e6")
                                (cons 'lockedMultiSelections nil)))
          (cons 'files nil)))

  (advice-add #'excali--empty-doc :override #'+excali--empty-doc)
  :hook
  (excali-mode . excali-mode-setup))
