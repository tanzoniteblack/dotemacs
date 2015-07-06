;; tmp-directory
(setq live-tmp-dir "~/.emacs.d/tmp/")

;; Use unix line endings by default, even in Windows
(setq-default buffer-file-coding-system 'utf-8-unix)

(use-package rainbow-delimiters
  :ensure t)

(use-package hydra
  :ensure t
  :config (global-set-key (kbd "<f2>")
                          (defhydra hydra-zoom (:color blue)
                            "zoom"
                            ("t" font-size-thunderbolt "thunderbolt")
                            ("l" font-size-mac-laptop "laptop")
                            ("+" text-scale-increase "zoom in")
                            ("-" text-scale-decrease "zoom out"))))

;;; global-company-mode for completions
(use-package company
  :ensure t
  :commands global-company-mode
  :diminish "comp"
  :config (progn (setq company-idle-delay .2
                       company-tooltip-flip-when-above t)
                 (bind-key "C-<tab>" 'company-manual-begin)
                 (bind-key "C-n" 'company-select-next company-active-map)
                 (bind-key "C-p" 'company-select-previous company-active-map)
                 (global-company-mode)))

;;; Save backup files in dedicated directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode)
  :diminish "")

(use-package ido
  :ensure t
  :config (progn (use-package flx-ido
                   :ensure t
                   :config (flx-ido-mode 1))
                 (setq ido-enable-prefix nil
                       ido-create-new-buffer 'always
                       ido-max-prospects 10
                       ido-default-file-method 'selected-window
                       ido-everywhere 1
                       ;; if exact match not found, look for other files containing these characters
                       ido-enable-flex-matching t
                       ;; don't leave the current directory if we don't find the file we typed
                       ido-auto-merge-work-directories-length -1
                       ;; ido file type ordering preferences
                       ido-file-extensions-order '(".org" ".clj"))
                 (icomplete-mode 1)
                 (ido-mode t)
				 (use-package ido-ubiquitous
				   :ensure t
				   :config (ido-ubiquitous-mode 1))))

;;use file path to ensure buffer name uniqueness
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward
                uniquify-separator "/"
                uniquify-after-kill-buffer-p t
                uniquify-ignore-buffers-re "^\\*"))

;;When you visit a file, point goes to the last place where it was
;;when you previously visited. Save file is set to live-tmp-dir/places
(use-package saveplace
  :config (progn (setq-default save-place t)
                 (make-directory live-tmp-dir t)
                 (setq save-place-file (concat live-tmp-dir "places"))))

(setq initial-major-mode 'lisp-interaction-mode
      redisplay-dont-pause t
      column-number-mode t
      echo-keystrokes 0.02
      inhibit-startup-message t
      transient-mark-mode t
      shift-select-mode nil
      require-final-newline t
      truncate-partial-width-windows nil
      delete-by-moving-to-trash t
      confirm-nonexistent-file-or-buffer nil
      query-replace-highlight t
      next-error-highlight t
      next-error-highlight-no-select t
      x-select-enable-clipboard t)

;;set all coding systems to utf-8
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; by default don't indent with tabs
(set-default 'indent-tabs-mode nil)
(auto-compression-mode t)
(show-paren-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

;;default to unified diffs
(use-package ediff
  :config (setq diff-switches "-u"
                ediff-window-setup-function 'ediff-setup-windows-plain))

;;remove all trailing whitespace and trailing blank lines before
;;saving the file
(defvar live-ignore-whitespace-modes '(markdown-mode))
(defun live-cleanup-whitespace ()
  (when (not (member major-mode live-ignore-whitespace-modes))
    (let ((whitespace-style '(trailing empty)))
      (whitespace-cleanup))))
(add-hook 'before-save-hook 'live-cleanup-whitespace)

;; savehist keeps track of some history
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (concat live-tmp-dir "savehist"))
(savehist-mode t)

;;; smex
(use-package smex
  :ensure t
  :config (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;; spell checking
(use-package ispell
  :config (progn (setq ispell-program-name "aspell" ; use aspell instead of ispell
                       ispell-extra-args '("--sug-mode=ultra"))
				 (add-hook 'text-mode-hook 'flyspell-mode)
                 (add-hook 'prog-mode-hook
                           (lambda ()
							 (flyspell-prog-mode)))
				 (add-hook 'sh-mode
						   (lambda ()
							 (turn-off-flyspell)))))

;;; magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  ;; Don't show setup instructions
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :config (progn (add-hook 'magit-log-edit-mode-hook
                           (lambda ()
                             (set-fill-column 72)
                             (auto-fill-mode 1)))
                 ;; (add-hook 'magit-mode-hook '(lambda () (auto-complete-mode 0)))
                 (setq
                  ;; use ido to look for branches
                  magit-completing-read-function 'magit-ido-completing-read
                  ;; don't put "origin-" in front of new branch names by default
                  magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
                  ;; highlight word/letter changes in hunk diffs
                  magit-diff-refine-hunk t
                  ;; don't attempt to save unsaved buffers
                  magit-save-some-buffers nil)
				 ;; Re-enable after magit 2.1.0 comes out
				 ;; (defun endless/visit-pull-request-url ()
				 ;;   "Visit the current branch's PR on Github."
				 ;;   (interactive)
				 ;;   (browse-url
				 ;; 	(format "https://github.com/%s/pull/new/%s"
				 ;; 			(replace-regexp-in-string
				 ;; 			 "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
				 ;; 			 (magit-get "remote"
				 ;; 						(magit-get-remote)
				 ;; 						"url"))
				 ;; 			(cdr (magit-get-remote-branch)))))

				 ;; (eval-after-load 'magit
				 ;;   '(define-key magit-mode-map "v"
				 ;; 	  #'endless/visit-pull-request-url))
                 (use-package gitconfig-mode
                   :ensure t)
                 (use-package gitignore-mode
                   :ensure t)))

(use-package erc
  :config (progn (setq erc-nick "tanzoniteblack")
                 (setq erc-hide-list '("JOIN" "PART" "QUIT"))))

;;; sql
(add-hook 'sql-mode-hook '(lambda ()
                            (sql-set-product 'postgres)
                            (setq indent-tabs-mode nil)))

(use-package go-mode
  :ensure t
  :commands go-mode
  :config (progn (use-package company-go
                   :ensure t
                   :if (executable-find "gocode")
                   :commands company-go
                   :config (add-to-list 'company-backends 'company-go))
                 (use-package go-eldoc
                   :ensure t
                   :if (executable-find "gocode")
                   :commands go-eldoc-setup
                   :config (add-to-list 'go-mode-hook 'go-eldoc-setup))
                 (bind-key "M-." 'godef-jump go-mode-map)
                 (bind-key "M-," 'pop-tag-mark go-mode-map)
                 (bind-key "C-S-F" 'gofmt go-mode-map)
                 (bind-key "M-<return>" 'godef-describe go-mode-map)))

;;; flycheck mode
(use-package flycheck
  :ensure t
  :commands global-flycheck-mode
  :config (progn (use-package popup
                   :ensure t)
                 (use-package flycheck-pos-tip
                   :ensure t)
                 (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)
                 (setq flycheck-display-errors-function 'flycheck-pos-tip-error-messages)
                 (global-flycheck-mode)))

(use-package avy
  :ensure t
  :bind (("C-c SPC" . avy-goto-word-1)
		 ("C-c S-SPC" . avy-goto-char)
         ("M-g G" . avy-goto-line)))


(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;;; enable (up/down)case-region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; set-up M-` as alternative to C-x 5 0 for switching frames
(global-set-key "\M-`" 'other-frame)



;;; highlight-symbol
(use-package highlight-symbol
  :ensure t
  :diminish ""
  :bind (("C-<f3>" . highlight-symbol-at-point)
         ("<f3>" . highlight-symbol-next)
         ("S-<f3>" . highlight-symbol-prev)
         ("M-<f3>" . highlight-symbol-prev))
  :config (progn (setq highlight-symbol-idle-delay 0.5)
                 (add-hook 'prog-mode-hook 'highlight-symbol-mode)
                 (highlight-symbol-mode)))

(use-package json-mode
  :ensure t
  :defer t
  :init (progn (add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
               (add-to-list 'auto-mode-alist '("\\.jsonld$" . json-mode))
               (add-to-list 'auto-mode-alist '(".tern-project" . json-mode))
               (add-to-list 'auto-mode-alist '(".jshintrc" . json-mode)))
  :config (progn (add-hook 'json-mode-hook 'flycheck-mode)
                 (bind-key "C-S-f" 'json-mode-beautify json-mode-map)))

(use-package nxml-mode
  :defer t
  :init (progn (add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
               (add-to-list 'auto-mode-alist '("\\.gapp$" . nxml-mode)))
  :config (progn (bind-key "C-S-f" 'beautify-xml nxml-mode-map)))

;;; quit settings
(global-set-key (kbd "C-x C-c") 'ask-before-closing)

;;; show human readable file sizes in dired
(setq dired-listing-switches "-alh")

;; turn on rainbow delimiters for all programming modes (in theory, in practice we need to specify a few more)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; overwrite selection rather than insert before selection
(delete-selection-mode 1)

;; start the emacs server
(use-package server
  :config (unless (server-running-p)
            (server-start)))

(cua-mode 0)

;;; browse kill ring
(use-package browse-kill-ring
  :ensure t
  :config (setq browse-kill-ring-highlight-current-entry t
                browse-kill-ring-no-duplicates t
                browse-kill-ring-display-duplicates nil
                browse-kill-ring-highlight-inserted-item nil))

;;; ace-window
(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window)
  :config (progn (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
                 (custom-set-faces
                  '(aw-leading-char-face
                    ((t (:inherit ace-jump-face-foreground :height 1.8)))))))

(use-package projectile
  :ensure t
  :config (progn (setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))
                 ;; add to the globally ignored files
                 (dolist (file-name '("*~" "*.elc"))
                   (add-to-list 'projectile-globally-ignored-files file-name))
                 (when (eq system-type 'windows-nt)
                   (setq projectile-indexing-method 'alien))
                 (projectile-global-mode)))

;;; respect ansi colors
(ansi-color-for-comint-mode-on)

;;; ansi colors in compilation mode
(ignore-errors
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;;; warn when opening files bigger than 100MB (default is 10MB)
(setq large-file-warning-threshold 100000000)

(defun large-file-protector ()
  "Function to be run when opening a file to detect if special large-file changes need made."
  (let ((too-many-bytes (> (buffer-size) (* 8 1024 1024)))
        (too-many-lines (> (count-lines (point-min) (point-max)) 9000)))
    (when too-many-bytes
      (setq buffer-read-only t)
      (buffer-disable-undo))
    (when (or too-many-bytes too-many-lines)
      (highlight-symbol-mode -1)
      (company-mode -1)
      (git-gutter-mode -1)
      (smartparens-mode -1)
      (show-paren-mode -1))))

(add-hook 'find-file-hook 'large-file-protector)

(use-package vlf
  :ensure t
  :config (require 'vlf-setup))

;; window-number-mode
(use-package window-number
  :ensure t
  :config (window-number-meta-mode 1))

(when (fboundp 'winner-mode)
  (winner-mode 1)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

(defvar auto-minor-mode-alist ()
  "Alist of filename patterns vs correpsonding minor mode functions, see `auto-mode-alist'
All elements of this alist are checked, meaning you can enable multiple minor modes for the same regexp.")

(defun enable-minor-mode-based-on-extension ()
  "check file name against auto-minor-mode-alist to enable minor modes
the checking happens for all pairs in auto-minor-mode-alist"
  (when buffer-file-name
    (let ((name buffer-file-name)
          (remote-id (file-remote-p buffer-file-name))
          (alist auto-minor-mode-alist))
      ;; Remove backup-suffixes from file name.
      (setq name (file-name-sans-versions name))
      ;; Remove remote file name identification.
      (when (and (stringp remote-id)
                 (string-match-p (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))

(add-hook 'find-file-hook 'enable-minor-mode-based-on-extension)

(use-package org
  :ensure t
  :config (progn (setq org-completion-use-ido t
                       org-outline-path-complete-in-steps nil
                       org-startup-indented nil
                       org-hide-leading-stars t
                       org-agenda-files (list "~/Dropbox/.org/yummly.org"
                                              "~/Dropbox/.org/home.org"
                                              "~/Dropbox/.org/beer.org")
                       org-directory "~/Dropbox/.org/"
                       org-src-fontify-natively t
                       org-display-inline-images t
                       org-deadline-warning-days 3
                       org-log-done 'time)
                 ;; if all children of a TODO are done, then change status of TODO to DONE
                 (defun org-summary-todo (n-done n-not-done)
                   "Switch entry to DONE when all subentries are done, to TODO otherwise."
                   (let (org-log-done org-log-states)   ; turn off logging
                     (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
                 (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
                 ;; remap ace-jump-word-mode (org-mode automatically disables)
                 (add-hook 'org-mode-hook '(lambda () (define-key org-mode-map (kbd "C-c SPC") 'avy-goto-word-1)))
                 (define-key org-mode-map (kbd "M-<tab>") 'org-table-insert-row)
                 (define-key org-mode-map (kbd "M-h") 'help-command)
                 (bind-key "C-c a" 'org-agenda-list)
                 ;; enable flyspell-mode on load of org buffer
                 (add-hook 'org-mode-hook 'flyspell-mode)
                 ;; (use-package htmlize
                 ;;   :ensure t)
                 ;; windmove compatibility
                 (add-hook 'org-shiftup-final-hook 'windmove-up)
                 (add-hook 'org-shiftleft-final-hook 'windmove-left)
                 (add-hook 'org-shiftdown-final-hook 'windmove-down)
                 (add-hook 'org-shiftright-final-hook 'windmove-right)
                 (add-hook 'org-mode-hook 'turn-on-auto-fill)))

(use-package clojure-mode
  :ensure t
  ;; :commands clojure-mode
  ;; :init (add-to-list 'auto-mode-alist '("\\.\\(clj[sx]?\\|dtm\\|edn\\)\\'" . clojure-mode))
  :config (progn (use-package clojure-mode-extra-font-locking
                   :ensure t)
                 (use-package cider
                   :ensure t
                   :config (progn (add-hook 'clojure-mode-hook 'cider-turn-on-eldoc-mode)
                                  (add-hook 'cider-repl-mode-hook 'subword-mode)
                                  (setq cider-annotate-completion-candidates t
                                        cider-mode-line " cider"
                                        cider-prompt-for-symbol nil)
                                  (define-key cider-repl-mode-map (kbd "M-RET") 'cider-doc)
                                  (define-key cider-mode-map (kbd "M-RET") 'cider-doc)))
                 (use-package clj-refactor
                   :ensure t
                   :config (progn (setq cljr-suppress-middleware-warnings t)
                                  (add-hook 'clojure-mode-hook (lambda ()
                                                                 (clj-refactor-mode 1)
                                                                 (cljr-add-keybindings-with-prefix "C-c C-m")))
                                  (define-key clojure-mode-map (kbd "C-:") 'clojure-toggle-keyword-string)
                                  (define-key clojure-mode-map (kbd "C->") 'cljr-cycle-coll)))
                 (add-hook 'clojure-mode-hook (lambda ()
                                                (setq buffer-save-without-query t)))
                 (add-hook 'clojure-mode-hook 'subword-mode)
                 ;; Fancy docstrings for schema/defn when in the form:
                 ;; (schema/defn NAME :- TYPE "DOCSTRING" ...)
                 (put 'schema/defn 'clojure-doc-string-elt 4)))

(use-package js2-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :config (use-package tern
            :commands tern-mode
            :init (add-hook 'js2-mode-hook 'tern-mode)
            :config (progn (use-package company-tern
                             :ensure t
                             :commands company-tern
                             :init (add-to-list 'company-backends 'company-tern))
                           (define-key tern-mode-keymap (kbd "M-.") 'tern-find-definition)
                           (define-key tern-mode-keymap (kbd "C-M-.") 'tern-find-definition-by-name)
                           (define-key tern-mode-keymap (kbd "M-,") 'tern-pop-find-definition)
                           (define-key tern-mode-keymap (kbd "C-c C-r") 'tern-rename-variable)
                           (define-key tern-mode-keymap (kbd "C-c C-c") 'tern-get-type)
                           (define-key tern-mode-keymap (kbd "C-c C-d") 'tern-get-docs)
                           (define-key tern-mode-keymap (kbd "M-<return>") 'tern-get-docs))))


(defun format-buffer ()
  "format buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(global-set-key (kbd "C-S-f") 'format-buffer)

(use-package cc-mode
  :config (progn (setq-default c-basic-offset 4 c-default-style "linux")
                 (setq-default tab-width 4 indent-tabs-mode t)
                 (add-hook 'java-mode-hook 'subword-mode)
                 (use-package dtrt-indent
                   :ensure t
                   :init (add-hook 'java-mode-hook 'dtrt-indent-mode))

                 (use-package eclim
                   :ensure emacs-eclim
                   :init (add-hook 'java-mode-hook 'eclim-mode)
                   :config (progn (use-package company-emacs-eclim
                                    :config (progn (require 'cl)
                                                   (company-emacs-eclim-setup)))
                                  (when (eq system-type 'darwin)
                                    (custom-set-variables
                                     '(eclim-eclipse-dirs '("/opt/homebrew-cask/Caskroom/eclipse-java/4.4.0/eclipse"))
                                     '(eclim-executable "/opt/homebrew-cask/Caskroom/eclipse-java/4.4.0/eclipse/plugins/org.eclim_2.4.0/bin/eclim")))
                                  (bind-key "M-." 'eclim-java-find-declaration eclim-mode-map)
                                  (bind-key "M-," 'pop-global-mark eclim-mode-map)
                                  (bind-key "M-<return>" 'eclim-java-show-documentation-for-current-element eclim-mode-map)))))

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

(use-package elpy
  :ensure t
  :config (progn (delete 'elpy-module-highlight-indentation elpy-modules)
                 (elpy-enable)
                 (setq elpy-rpc-backend "jedi")
                 (define-key elpy-mode-map (kbd "M-.") 'elpy-goto-definition)
                 (define-key elpy-mode-map (kbd "M-,") 'pop-tag-mark)
                 (define-key elpy-mode-map (kbd "M-<RET>") 'elpy-doc)
                 (add-hook 'python-mode-hook 'rainbow-delimiters-mode)
                 (add-hook 'python-mode-hook 'highlight-symbol-mode)))

(defun enable-lisp-hooks (mode-name)
  "Enable lisp-y goodness for MODE-NAME."
  (let ((mode-hook (intern (concat (symbol-name mode-name) "-hook"))))
    (add-hook mode-hook 'rainbow-delimiters-mode)
    (add-hook mode-hook 'smartparens-strict-mode)))

(use-package smartparens
  :ensure t
  :config (progn (use-package smartparens-config)
                 (smartparens-global-mode t)
                 ;; highlights matching pairs
                 (show-smartparens-global-mode t)
                 ;; custom keybindings for smartparens mode
                 (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
                 (define-key smartparens-mode-map (kbd "M-(") 'sp-forward-barf-sexp)
                 (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
                 (define-key smartparens-mode-map (kbd "M-)") 'sp-forward-slurp-sexp)

                 (define-key smartparens-strict-mode-map (kbd "M-d") 'kill-sexp)
                 (define-key smartparens-strict-mode-map (kbd "M-D") 'sp-kill-sexp)
                 (define-key smartparens-mode-map (kbd "s-S") 'sp-split-sexp)


                 (sp-with-modes '(clojure-mode cider-repl-mode)
                   (sp-local-pair "#{" "}")
                   (sp-local-pair "`" nil :actions nil)
                   (sp-local-pair "@(" ")")
                   (sp-local-pair "#(" ")"))

                 (sp-local-pair 'markdown-mode "`" nil :actions nil)
                 (sp-local-pair 'gfm-mode "`" nil :actions nil)

                 (sp-local-pair 'web-mode "{" "}" :actions nil)
                 (-each sp--lisp-modes 'enable-lisp-hooks)))

;;; elisp tag navigation
;;; many functions borrowed from emacs-live
(use-package elisp-slime-nav
  :ensure t
  :init (progn (add-to-list 'auto-mode-alist '("\\.el$" . emacs-lisp-mode))
               (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))
  :diminish "")

(use-package eldoc
  :init (progn (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
               (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
               (add-hook 'ielm-mode-hook 'eldoc-mode))
  :diminish "")

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)

(defun live-lisp-describe-thing-at-point ()
  "Show the documentation of the Elisp function and variable near point.
   This checks in turn:
     -- for a function name where point is
     -- for a variable name where point is
     -- for a surrounding function call"
  (interactive)
  (let (sym)
    ;; sigh, function-at-point is too clever.  we want only the first half.
    (cond ((setq sym (ignore-errors
                       (with-syntax-table emacs-lisp-mode-syntax-table
                         (save-excursion
                           (or (not (zerop (skip-syntax-backward "_w")))
                               (eq (char-syntax (char-after (point))) ?w)
                               (eq (char-syntax (char-after (point))) ?_)
                               (forward-sexp -1))
                           (skip-chars-forward "`'")
                           (let ((obj (read (current-buffer))))
                             (and (symbolp obj) (fboundp obj) obj))))))
           (describe-function sym))
          ((setq sym (variable-at-point)) (describe-variable sym)))))

(use-package buffer-move
  :ensure t
  :config (global-set-key (kbd "C-c w")
                          (defhydra hydra-buffer-move (:color blue)
                            "buffer-move"
                            ("p" buf-move-up "up")
                            ("n" buf-move-down "down")
                            ("b" buf-move-left "left")
                            ("f" buf-move-right "right"))))

;; Random other modes

(use-package dockerfile-mode
  :ensure t)

(use-package scala-mode2
  :ensure t)

(use-package d-mode
  :ensure t)

(use-package groovy-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode)))

;; (use-package vala-mode
;;   :ensure t)

(use-package rust-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :init (add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode)))

(use-package web-mode
  :ensure t
  :commands web-mode
  :init (progn (add-to-list 'auto-mode-alist '("\\.phtml$'" . web-mode))
               (add-to-list 'auto-mode-alist '("\\.tpl\\.php$'" . web-mode))
               (add-to-list 'auto-mode-alist '("\\.[gj]sp$'" . web-mode))
               (add-to-list 'auto-mode-alist '("\\.as[cp]x$'" . web-mode))
               (add-to-list 'auto-mode-alist '("\\.erb$'" . web-mode))
               (add-to-list 'auto-mode-alist '("\\.mustache$'" . web-mode))
               (add-to-list 'auto-mode-alist '("\\.djhtml$'" . web-mode))
               (add-to-list 'auto-mode-alist '("\\.html$'" . web-mode)))
  :config ;; Autocomplete end tag when finished writing opening tag
  (setq web-mode-auto-close-style 2))

(use-package ruby-mode
  :ensure t
  :commands ruby-mode
  :init (progn (add-to-list 'auto-mode-alist '("vagrantfile" . ruby-mode))
               (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))))

(setq line-move-visual t)

(use-package git-link
  :ensure t)

(use-package jape-mode
  :ensure t)

(use-package auto-package-update
  :ensure t
  :commands auto-package-update-maybe)

(use-package langtool
  :ensure t
  :config (when (eq system-type 'darwin)
            (setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/2.7/libexec/languagetool.jar")))

(setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")

(use-package rainbow-identifiers
  :ensure t
  :config (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))

(use-package anzu
  :ensure t
  :config (global-anzu-mode 1)
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))
