(require 'diminish)

(defvar diminish-modes '(('paredit-mode "Par")
						 ('elisp-slime-nav-mode "")
						 ('undo-tree-mode "")
						 ('git-gutter-mode "")
						 ('highlight-symbol-mode "")
						 ('eldoc-mode "")
						 ('magit-auto-revert-mode "")))

(--each diminish-modes
  (let ((mode-name (car it))
        (lighter (cadr it)))
	(eval-after-load mode-name
      (diminish mode-name lighter))))
