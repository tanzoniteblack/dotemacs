(require 'smartparens-config)

(smartparens-global-mode t)
;; highlights matching pairs
(show-smartparens-global-mode t)

;; turn on rainbow delimiters-mode and smartparens-strict for all lisps
(require 'rainbow-delimiters)
(--each sp--lisp-modes
  (eval-after-load (symbol-name it)
    (progn
      (add-hook (intern (concat (symbol-name it) "-hook")) 'rainbow-delimiters-mode)
      (add-hook (intern (concat (symbol-name it) "-hook")) 'smartparens-strict-mode))))

;; custom keybindings for smartparens mode
(define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "M-(") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "M-)") 'sp-forward-slurp-sexp)

(define-key smartparens-strict-mode-map (kbd "M-d") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "s-S") 'sp-split-sexp)


(sp-with-modes '(clojure-mode cider-mode)
  (sp-local-pair "#{" "}")
  (sp-local-pair "`" nil :actions nil))

(sp-local-pair 'markdown-mode "`" nil :actions nil)
(sp-local-pair 'gfm-mode "`" nil :actions nil)
