;;; lang/nix.el -*- lexical-binding: t; -*-

(use-package nix-ts-mode
  :mode
  ("\\.nix\\'" . nix-ts-mode)
  :bind
  ( :map nix-ts-mode-map
    ("C-c C-p" . nix-repl)))

(use-package nix-mode)

(with-eval-after-load 'eglot
  (add-to-list
   'eglot-default-configurations
   (cons
    "nix"
    '( :nixd
       ( :nixpkgs (:expr "import (builtins.getFlake (builtins.toString ./)).inputs.nixpkgs { }")
         :formatting (:command ["nixfmt"])
         :options ()
         ))
    ))
  )

(defun nix-flake-update-inputs ()
  (interactive)
  (if-let* ((default-directory (project-root (project-current nil default-directory)))
            (inputs (string-lines
                     (shell-command-to-string
                      "nix flake metadata --no-warn-dirty --json | jq -r '.locks.nodes.root.inputs | keys[]'")))
            (selected (completing-read-multiple "Update inputs: " inputs)))
      (detached-shell-command (concat "nix flake update " (string-join selected " ")))
    (user-error "Not a nix flake project.")))

(defun util/commands-run--add-nix-flake-commands (buffer)
  (with-current-buffer buffer
    (when-let* ((default-directory (project-root (project-current nil default-directory))))
      (when (file-exists-p "flake.nix")
        '(("Update Nix Flake Inputs"  . nix-flake-update-inputs)))
      )))

(add-hook 'util/commands-run-list-additional-command-hook #'util/commands-run--add-nix-flake-commands)
