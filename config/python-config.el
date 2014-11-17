(setq elpy-rpc-backend "jedi")
(elpy-enable)

(define-key elpy-mode-map (kbd "M-.") #'elpy-goto-definition)
(define-key elpy-mode-map (kbd "M-,") #'pop-tag-mark)

(add-hook 'python-mode-hook #'rainbow-delimiters-mode)
(add-hook 'python-mode-hook #'highlight-symbol-mode)
