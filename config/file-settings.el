;; use gfm-mode (github formatted md) instead of regular markdown-mode

(add-to-list 'auto-mode-alist '("\\.gate$" . xml-mode))

;;; open Cask files in lisp-mode
(add-to-list 'auto-mode-alist '("Cask" . lisp-mode))

;; recognize .zsh files for sh-mode
(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh-theme$" . sh-mode))

(add-to-list 'auto-minor-mode-alist '("My Clippings.txt" . read-only-mode))
