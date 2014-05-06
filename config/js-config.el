(require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'ac-modes 'js2-mode)

(autoload 'tern-mode "tern.el" nil t)

(add-hook 'js2-mode-hook (lambda ()
                           (flycheck-select-checker 'javascript-jshint)
                           (flycheck-mode)
                           (tern-mode t)))

(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)
     (define-key tern-mode-keymap [(meta ?.)] 'tern-find-definition)
     (define-key tern-mode-keymap [(control meta ?.)] 'tern-find-definition-by-name)
     (define-key tern-mode-keymap [(meta ?,)] 'tern-pop-find-definition)
     (define-key tern-mode-keymap [(control ?c) (control ?r)] 'tern-rename-variable)
     (define-key tern-mode-keymap [(control ?c) (control ?c)] 'tern-get-type)
     (define-key tern-mode-keymap [(control ?c) (control ?d)] 'tern-get-docs)))

(require 'handlebars-mode)

(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
