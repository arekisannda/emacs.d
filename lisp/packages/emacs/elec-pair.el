;;; emacs/elec-pair.el -*- lexical-binding: t; -*-

(use-package elec-pair
  :custom
  (electric-pair-inhibit-predicate (lambda (c) t))
  (electric-pair-pairs `(
                         (?\" . ?\")
                         (?\{ . ?\})
                         (?\( . ?\))
                         (?\[ . ?\])
                         (,(nth 0 electric-quote-chars) . ,(nth 1 electric-quote-chars))
                         (,(nth 2 electric-quote-chars) . ,(nth 3 electric-quote-chars))
                         )))
