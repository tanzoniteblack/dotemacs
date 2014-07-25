;; Default Emacs Live bindings

;; winner undo and redo
(global-set-key (kbd "C-c b") 'winner-undo)
(global-set-key (kbd "C-c f") 'winner-redo)

;;window and buffer movement
(global-set-key (kbd "C-c w s") 'swap-windows)
(global-set-key (kbd "C-c w r") 'rotate-windows)
(global-set-key (kbd "C-c w p") 'buf-move-up)
(global-set-key (kbd "C-c w n") 'buf-move-down)
(global-set-key (kbd "C-c w b") 'buf-move-left)
(global-set-key (kbd "C-c w f") 'buf-move-right)

;;; paredit
(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "C-M-e")   'paredit-backward-barf-sexp)
     (define-key paredit-mode-map (kbd "C-M-s")   'paredit-backward-slurp-sexp)
     (define-key paredit-mode-map (kbd "C-M-j")   'live-paredit-forward-slurp-sexp-neatly)
     (define-key paredit-mode-map (kbd "C-M-y")   'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "M-S")     'paredit-split-sexp)
     (define-key paredit-mode-map (kbd "M-s")     'paredit-splice-sexp)
     (define-key paredit-mode-map (kbd "M-j")     'paredit-join-sexps)
     (define-key paredit-mode-map (kbd "M-P")     'live-paredit-previous-top-level-form)
     (define-key paredit-mode-map (kbd "M-N")     'live-paredit-next-top-level-form)
     (define-key paredit-mode-map (kbd "C-M-f")   'live-paredit-forward)
     (define-key paredit-mode-map (kbd "M-q")     'live-paredit-reindent-defun)
     (define-key paredit-mode-map (kbd "M-d")     'live-paredit-forward-kill-sexp)
     (define-key paredit-mode-map (kbd "M-k")     'live-paredit-backward-kill)
     (define-key paredit-mode-map (kbd "M-\\")    'live-paredit-delete-horizontal-space)
     (define-key paredit-mode-map (kbd "M-T")     'transpose-sexps)))

;;browse kill ring (visual paste)
(global-set-key (kbd "M-y") 'browse-kill-ring)

;;use delete-horizontal-space to completely nuke all whitespace
(global-set-key (kbd "M-SPC ") 'live-delete-whitespace-except-one)

;;make ^h delete rather than help
(global-set-key (kbd "C-h") 'delete-backward-char)
(define-key paredit-mode-map (kbd "C-h") 'paredit-backward-delete)

;;redefine help shortcut
(global-set-key (kbd "M-h") 'help-command)
(define-key org-mode-map (kbd "M-h") 'help-command)

;;allow the deletion of words:
;;backward kill word (forward kill word is M-d)
(global-set-key (kbd "C-\\") 'backward-kill-word)
(define-key ido-file-completion-map (kbd "C-\\") 'backward-kill-word)
(define-key paredit-mode-map (kbd "C-\\") 'paredit-backward-kill-word)

;;kill line backwards
(global-set-key (kbd "M-k") 'live-backwards-kill-line)

;;kill regions
(global-set-key (kbd "C-x C-k") 'kill-region)

;;set the mark
(global-set-key (kbd "C-SPC") 'set-mark-command)

;;fast vertical naviation
(global-set-key  (kbd "M-p") 'outline-previous-visible-heading)
(global-set-key  (kbd "M-n") 'outline-next-visible-heading)

;; Search
(global-set-key (kbd "C-s")   'isearch-forward)
(global-set-key (kbd "C-r")   'isearch-backward)
(global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward-regexp)

;; File
(global-set-key (kbd "C-x M-f")   'ido-find-file-other-window)
(global-set-key (kbd "C-x f")     'live-recentf-ido-find-file)
(global-set-key (kbd "C-x C-r")   'ido-recentf-open)
(global-set-key (kbd "C-x C-b")   'ibuffer)

;; If you want to be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Ace jump mode
(global-set-key (kbd "C-o") 'ace-jump-mode)

;; Show documentation/information with M-RET
(define-key lisp-mode-shared-map (kbd "M-RET") 'live-lisp-describe-thing-at-point)
(define-key cider-repl-mode-map (kbd "M-RET") 'cider-doc)
(define-key cider-mode-map (kbd "M-RET") 'cider-doc)

(global-set-key (kbd "C-x !") 'insert-date)

(setq shift-select-mode t)

;;; visual regexp replacements
(eval-after-load 'visual-regexp
  '(progn
     (define-key global-map (kbd "C-c r") 'vr/replace)
     (define-key global-map (kbd "C-c q") 'vr/query-replace)))

;;; next error
(global-set-key (kbd "M-'") 'next-error)

;;; json-mode beautify
(define-key json-mode-map (kbd "C-S-f") 'json-mode-beautify)

;;; eval-buffer
(define-key emacs-lisp-mode-map (kbd "C-c m b") 'eval-buffer)
