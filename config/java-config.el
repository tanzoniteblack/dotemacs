(require 'cedet)
(require 'semantic)
(load "semantic/loaddefs.el")
(semantic-mode 1)
(require 'malabar-mode)
(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
(require 'dtrt-indent)
(add-hook 'java-mode-hook #'dtrt-indent-mode)

(add-to-list 'company-semantic-modes 'malabar-mode)

(define-key malabar-mode-map (kbd "M-.") #'malabar-jump-to-thing)
(define-key malabar-mode-map (kbd "M-,") #'pop-global-mark)
(define-key malabar-mode-map (kbd "C-c C-k") #'malabar-compile-file)
