(require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'ac-modes 'js2-mode)

(autoload 'tern-mode "tern.el" nil t)

(add-hook 'js2-mode-hook (lambda ()
                           (flycheck-select-checker 'javascript-jshint)
                           (flycheck-mode)
                           (tern-mode t)))

(setq js2-basic-offset 2
	  js2-bounce-indent-p t)

(setq js2-mode-hook
  '(lambda () (progn
    (set-variable 'indent-tabs-mode nil))))

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

(require 'handlebars-mode)

(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
