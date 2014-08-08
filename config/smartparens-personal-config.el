(require 'smartparens-config)

(smartparens-global-mode t)
(show-smartparens-global-mode t)

(--each sp--lisp-modes
  (eval-after-load (symbol-name it)
	'(progn
	   (eval-after-load 'rainbow-delimiters-mode
		 '(add-hook (intern (concat (symbol-name it) "-hook")) 'rainbow-delimiters-mode))
	   '(add-hook (intern (concat (symbol-name it) "-hook")) 'smartparens-strict-mode))))

;; custom keybindings for smartparens mode
(define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "M-(") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "M-)") 'sp-forward-slurp-sexp)

(define-key smartparens-strict-mode-map (kbd "M-d") 'sp-kill-sexp)


(sp-with-modes '(clojure-mode cider-mode)
  (sp-local-pair "#{" "}")
  (sp-local-pair "`" nil :actions nil))

(sp-local-pair 'markdown-mode "`" nil :actions nil)
