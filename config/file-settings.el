;; use gfm-mode (github formatted md) instead of regular markdown-mode
(add-to-list 'auto-mode-alist '("\\.md" . gfm-mode))

;;; web-mode
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; random
(add-to-list 'auto-mode-alist '("vagrantfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gate$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode))

;;; open Cask files in lisp-mode
(add-to-list 'auto-mode-alist '("Cask" . lisp-mode))

;; recognize .zsh files for sh-mode
(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh-theme$" . sh-mode))

(add-to-list 'auto-minor-mode-alist '("My Clippings.txt" . read-only-mode))

;; Random json files
(add-to-list 'auto-mode-alist '(".tern-project" . json-mode))
(add-to-list 'auto-mode-alist '(".jshintrc" . json-mode))
