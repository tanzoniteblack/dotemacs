(require 'org)
(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-iswitchb)
(define-key global-map "\C-c7" 'org-mark-ring-goto)
(setq org-startup-indented nil)
(setq org-hide-leading-stars t)
(setq org-startup-truncated nil)

;; if all children of a TODO are done, then change status of TODO to DONE
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; set e-mail address for org-mode
(setq user-mail-address "rnsmith2@gmail.com")

;; global todo list files
(setq org-agenda-files (list "~/Dropbox/.org/yummly.org"
                             "~/Dropbox/.org/home.org"))

;; org folder
(setq org-directory "~/Dropbox/.org/")

;; set custom TODO keywords
(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE" "UNNEC")))

;; make source code blocks fontify in native formatting mode
(setq org-src-fontify-natively t)

;; show images
(setq org-display-inline-images t)

;;; turn off git-gutter-mode
(add-hook 'org-mode-hook '(lambda () (git-gutter-mode 0)))

;; turn off auto-complete for org-mode
(add-hook 'org-mode-hook '(lambda () (auto-complete-mode 0)))

;;; remap ace-jump-word-mode (org-mode automatically disables)
(add-hook 'org-mode-hook '(lambda () (define-key org-mode-map (kbd "C-c SPC") 'ace-jump-word-mode)))

;;; enable flyspell-mode on load of org buffer
(add-hook 'org-mode-hook 'flyspell-mode)

;;; pprint html output
(require 'htmlize)

;; windmove compatibility
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)
