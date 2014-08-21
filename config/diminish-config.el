(require 'diminish)

(diminish 'paredit-mode "Par")
(diminish 'elisp-slime-nav-mode "")
(diminish 'undo-tree-mode "")
(add-hook 'git-gutter-mode-hook (lambda () (diminish 'git-gutter-mode "")))
(diminish 'highlight-symbol-mode "")
(diminish 'eldoc-mode "")
(diminish 'magit-auto-revert-mode "")
