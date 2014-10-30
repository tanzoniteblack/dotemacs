(require 'js2-mode)
(require 'tern)
(add-hook 'js2-mode-hook 'tern-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(require 'company-tern)
(add-to-list 'company-backends 'company-tern)
(define-key tern-mode-keymap (kbd "M-.") 'tern-find-definition)
(define-key tern-mode-keymap (kbd "C-M-.") 'tern-find-definition-by-name)
(define-key tern-mode-keymap (kbd "M-,") 'tern-pop-find-definition)
(define-key tern-mode-keymap (kbd "C-c C-r") 'tern-rename-variable)
(define-key tern-mode-keymap (kbd "C-c C-c") 'tern-get-type)
(define-key tern-mode-keymap (kbd "C-c C-d") 'tern-get-docs)
(define-key tern-mode-keymap (kbd "M-<return>") 'tern-get-docs)

(add-to-list 'auto-mode-alist '(".tern-project" . json-mode))
(add-to-list 'auto-mode-alist '(".jshintrc" . json-mode))
