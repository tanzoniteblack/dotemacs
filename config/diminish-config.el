(setq diminish-modes '((elisp-slime-nav-mode . "")
                       (paredit-mode . "Par")
                       (elisp-slime-nav-mode . "")
                       (undo-tree-mode . "")
                       (git-gutter-mode . "")
                       (highlight-symbol-mode . "")
                       (eldoc-mode . "")
                       (magit-auto-revert-mode . "")))

(--each diminish-modes
  (let ((mode (car it))
        (lighter (cdr it)))
    (with-demoted-errors (diminish mode lighter))))
