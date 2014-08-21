(require 'diminish)

(diminish 'paredit-mode "Par")
(diminish 'elisp-slime-nav-mode "")
(diminish 'undo-tree-mode "")
(eval-after-load 'git-gutter-mode
  (diminish 'git-gutter-mode ""))
(diminish 'highlight-symbol-mode "")
(diminish 'eldoc-mode "")
(diminish 'magit-auto-revert-mode "")
