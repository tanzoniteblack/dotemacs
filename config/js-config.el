(require 'js2-mode)
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode-hook 'tern-mode)

(eval-after-load 'tern
  '(progn
     (require 'company-tern)
     (add-to-list 'company-backends 'company-tern)
     (define-key tern-mode-keymap [(meta ?.)] 'tern-find-definition)
     (define-key tern-mode-keymap [(control meta ?.)] 'tern-find-definition-by-name)
     (define-key tern-mode-keymap [(meta ?,)] 'tern-pop-find-definition)
     (define-key tern-mode-keymap [(control ?c) (control ?r)] 'tern-rename-variable)
     (define-key tern-mode-keymap [(control ?c) (control ?c)] 'tern-get-type)
     (define-key tern-mode-keymap [(control ?c) (control ?d)] 'tern-get-docs)))

(add-to-list 'auto-mode-alist '(".tern-project" . json-mode))
(add-to-list 'auto-mode-alist '(".jshintrc" . json-mode))
