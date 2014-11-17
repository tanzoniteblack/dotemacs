;; Default Emacs Live bindings

;;window and buffer movement
(global-set-key (kbd "C-c w p") #'buf-move-up)
(global-set-key (kbd "C-c w n") #'buf-move-down)
(global-set-key (kbd "C-c w b") #'buf-move-left)
(global-set-key (kbd "C-c w f") #'buf-move-right)

;;browse kill ring (visual paste)
(global-set-key (kbd "M-y") #'browse-kill-ring)

;;use delete-horizontal-space to completely nuke all whitespace
(global-set-key (kbd "M-SPC ") #'live-delete-whitespace-except-one)

;;make ^h delete rather than help
(global-set-key (kbd "C-h") #'delete-backward-char)

;;redefine help shortcut
(global-set-key (kbd "M-h") #'help-command)
(define-key org-mode-map (kbd "M-h") #'help-command)

;;kill regions
(global-set-key (kbd "C-x C-k") #'kill-region)

;;set the mark
(global-set-key (kbd "C-SPC") #'set-mark-command)

;;fast vertical naviation
(global-set-key  (kbd "M-p") #'outline-previous-visible-heading)
(global-set-key  (kbd "M-n") #'outline-next-visible-heading)

;; Search
(global-set-key (kbd "C-s") #'isearch-forward)
(global-set-key (kbd "C-r") #'isearch-backward)

;; File
(global-set-key (kbd "C-x M-f") #'ido-find-file-other-window)
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; If you want to be able to M-x without meta
(global-set-key (kbd "C-x C-m") #'execute-extended-command)

;; Show documentation/information with M-RET
(define-key lisp-mode-shared-map (kbd "M-RET") #'live-lisp-describe-thing-at-point)
(define-key cider-repl-mode-map (kbd "M-RET") #'cider-doc)
(define-key cider-mode-map (kbd "M-RET") #'cider-doc)

(global-set-key (kbd "C-x !") #'insert-date)

(setq shift-select-mode t)

;;; next error
(global-set-key (kbd "M-'") #'next-error)

;;; eval-buffer
(define-key emacs-lisp-mode-map (kbd "C-c m b") #'eval-buffer)

(global-set-key (kbd "C-j") #'reindent-then-newline-and-indent)
(global-set-key (kbd "<RET>") #'electric-indent-just-newline)

(global-set-key (kbd "M-\\") #'delete-adjacent-whitespace)
