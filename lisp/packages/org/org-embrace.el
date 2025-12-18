;;; org/org-embrace.el -*- lexical-binding: t; -*-

(use-package nil :after (org embrace) ;; org-embrace
  :init
  (defun +embrace-with-org-block ()
    (let ((block-type (completing-read
                       "Org block type: "
                       '(center comment example export justifyleft justifyright
                                quote src verse))))
      (cond ((string= block-type "src")
             (cons
              (concat (format "#+begin_src %s"
                              (completing-read "Language: "
                                               (embrace--get-org-src-block-modes)))
                      (let ((args (read-string "Arguments: ")))
                        (unless (string= args "")
                          (format " %s" args))))
              "#+end_src"))
            ((string= block-type "export")
             (cons (format "#+begin_export %s"
                           (completing-read "Format: "
                                            '(ascii beamer html latex texinfo)))
                   "#+end_export"))
            (t
             (setq block-type (downcase block-type))
             (cons (format "#+begin_%s" block-type)
                   (format "#+end_%s" block-type))))))

  (defun +embrace-org-mode-hook ()
    (dolist (lst '((?= "=" . "=")
                   (?~ "~" . "~")
                   (?/ "/" . "/")
                   (?* "*" . "*")
                   (?_ "_" . "_")
                   (?+ "+" . "+")
                   (?k "@@html:<kbd>@@" . "@@html:</kbd>@@")))
      (embrace-add-pair (car lst) (cadr lst) (cddr lst)))
    (embrace-add-pair-regexp ?l "#\\+begin_.*" "#\\+end_.*" 'embrace-with-org-block
                             (embrace-build-help "#+begin_*" "#+end") t))
  (advice-add #'embrace-with-org-block :override #'+embrace-with-org-block)
  (advice-add #'embrace-org-mode-hook :override #'+embrace-org-mode-hook)
  :hook
  (org-mode . embrace-org-mode-hook))
