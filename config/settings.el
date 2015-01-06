;;; global-company-mode for completions
(use-package company
  :commands global-company-mode
  :idle (global-company-mode)
  :config (progn (setq company-idle-delay .2
                       company-tooltip-flip-when-above t)
                 (bind-key "C-<tab>" 'company-manual-begin)
                 (bind-key "C-n" 'company-select-next company-active-map)
                 (bind-key "C-p" 'company-select-previous company-active-map)))
;; (setq company-backends (remove 'company-eclim company-backends))

;; more intelligent paren highlighting
(paren-activate)

;;; Save backup files in dedicated directory
(setq backup-directory-alist '(("." . "~/.saves")))

;;; undo tree
(global-undo-tree-mode)

;;; autocompile emacs-lisp files
(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)

;;use file path to ensure buffer name uniqueness
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;;When you visit a file, point goes to the last place where it was
;;when you previously visited. Save file is set to live-tmp-dir/places
(require 'saveplace)
(setq-default save-place t)
(make-directory live-tmp-dir t)
(setq save-place-file (concat live-tmp-dir "places"))

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
      next-error-highlight-no-select t)

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
(require 'ediff)
(setq diff-switches "-u"
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

;;remove all trailing whitespace and trailing blank lines before
;;saving the file
(defvar live-ignore-whitespace-modes '(markdown-mode))
(defun live-cleanup-whitespace ()
  (when (not (member major-mode live-ignore-whitespace-modes))
    (let ((whitespace-style '(trailing empty)))
      (whitespace-cleanup))))
(add-hook 'before-save-hook #'live-cleanup-whitespace)

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
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") #'smex)
(global-set-key (kbd "M-X") #'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") #'execute-extended-command)

;;; spell checking
(require 'ispell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

;;; magit
(require 'magit)
(add-hook 'magit-log-edit-mode-hook
          (lambda ()
            (set-fill-column 72)
            (auto-fill-mode 1)))
(global-set-key (kbd "C-x g") #'magit-status)
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

;;; irc defaults
(require 'erc)
(setq erc-nick "tanzoniteblack")
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;;; sql
(add-hook 'sql-mode-hook '(lambda ()
                            (sql-set-product 'postgres)
                            (setq indent-tabs-mode nil)))

(use-package go-mode
  :commands go-mode
  :init (add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
  :config (progn (use-package company-go
                   :if (executable-find "gocode")
                   :commands company-go
                   :init (add-to-list #'company-backends #'company-go))
                 (use-package go-eldoc
                   :if (executable-find "gocode")
                   :commands go-eldoc-setup
                   :init (add-to-list #'go-mode-hook #'go-eldoc-setup))
                 (bind-key "M-." 'godef-jump go-mode-map)
                 (bind-key "M-," 'pop-tag-mark go-mode-map)
                 (bind-key "C-S-F" 'gofmt go-mode-map)
                 (bind-key "M-<return>" 'godef-describe go-mode-map)))

;;; flycheck mode
(require 'flycheck)
(add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)
(flycheck-define-checker postgresql
  "A SQL syntax checker using pgsanity. Linter is designed to work
  specifically with postgresql, but works with all non-product specific
  SQL as well.

  See URL `https://github.com/markdrago/pgsanity'."
  :command ("pgsanity" source)
  :error-patterns ((error line-start "line " line ": ERROR: " (message) line-end))
  :modes sql-mode)
(add-to-list 'flycheck-checkers 'postgresql)


(custom-set-variables
 '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(global-flycheck-mode)


(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;; enable (up/down)case-region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; set-up M-` as alternative to C-x 5 0 for switching frames
(global-set-key "\M-`" #'other-frame)

;;; ido settings
;; if exact match not found, look for other files containing these characters
(setq ido-enable-flex-matching t)
;; don't leave the current directory if we don't find the file we typed
(setq ido-auto-merge-work-directories-length -1)
;; ido file type ordering preferences
(setq ido-file-extensions-order '(".org" ".clj"))

;;; highlight-symbol
(use-package highlight-symbol
  :diminish ""
  :idle (highlight-symbol-mode)
  :bind (("C-<f3>" . highlight-symbol-at-point)
         ("<f3>" . highlight-symbol-next)
         ("S-<f3>" . highlight-symbol-prev)
         ("M-<f3>" . highlight-symbol-prev))
  :config (progn (setq highlight-symbol-idle-delay 0.5)
                 (add-hook 'prog-mode-hook #'highlight-symbol-mode)))

;;; c style
(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode t)

(use-package json-mode
  :defer t
  :init (progn (add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
               (add-to-list 'auto-mode-alist '("\\.jsonld$" . json-mode)))
  :config (progn (add-hook 'json-mode-hook #'flycheck-mode)
                 (bind-key "C-S-f" 'json-mode-beautify json-mode-map)))

(use-package nxml-mode
  :defer t
  :init (progn (add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
               (add-to-list 'auto-mode-alist '("\\.gapp$" . nxml-mode)))
  :config (progn (bind-key "C-S-f" 'beautify-xml nxml-mode-map)))

;;; quit settings
(global-set-key (kbd "C-x C-c") #'ask-before-closing)

;;; show human readable file sizes in dired
(setq dired-listing-switches "-alh")

;; turn on rainbow delimiters for all programming modes (in theory, in practice we need to specify a few more)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; overwrite selection rather than insert before selection
(delete-selection-mode 1)

;; start the emacs server
(load "server")
(unless (server-running-p)
  (server-start))

(cua-mode 0)

;;; browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-highlight-current-entry t)
(setq browse-kill-ring-no-duplicates t)
(setq browse-kill-ring-display-duplicates nil)
(setq browse-kill-ring-highlight-inserted-item nil)

;;; ace-window
(global-set-key (kbd "C-x o") #'ace-window)

;;; projectile-mode
(require 'projectile)
(projectile-global-mode)
(setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))
;;; add to the globally ignored files
(dolist (file-name '("*~" "*.elc"))
  (add-to-list 'projectile-globally-ignored-files file-name))

;;; respect ansi colors
(ansi-color-for-comint-mode-on)

;;; ansi colors in compilation mode
(ignore-errors
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook #'my-colorize-compilation-buffer))

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
      (paren-deactivate)
      (show-paren-mode -1))))

(add-hook 'find-file-hook #'large-file-protector)
(require 'vlf-setup)

;; window-number-mode
(require 'window-number)
(window-number-meta-mode 1)

(when (fboundp 'winner-mode)
  (winner-mode 1))

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

(add-hook 'find-file-hook #'enable-minor-mode-based-on-extension)

(add-hook 'prog-mode-hook #'rainbow-identifiers-mode)

(use-package org
  :commands org-mode
  :init (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  :config (progn (setq org-completion-use-ido t
                       org-outline-path-complete-in-steps nil
                       org-startup-indented nil
                       org-hide-leading-stars t
                       org-agenda-files (list "~/Dropbox/.org/yummly.org"
                                              "~/Dropbox/.org/home.org")
                       org-directory "~/Dropbox/.org/"
                       org-src-fontify-natively t
                       org-display-inline-images t)
                 ;; if all children of a TODO are done, then change status of TODO to DONE
                 (defun org-summary-todo (n-done n-not-done)
                   "Switch entry to DONE when all subentries are done, to TODO otherwise."
                   (let (org-log-done org-log-states)   ; turn off logging
                     (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
                 (add-hook 'org-after-todo-statistics-hook #'org-summary-todo)
                 ;; remap ace-jump-word-mode (org-mode automatically disables)
                 (add-hook 'org-mode-hook '(lambda () (define-key org-mode-map (kbd "C-c SPC") #'ace-jump-word-mode)))
                 (define-key org-mode-map (kbd "M-<tab>") 'org-table-insert-row)
                 (define-key org-mode-map (kbd "M-h") #'help-command)
                 ;; enable flyspell-mode on load of org buffer
                 (add-hook 'org-mode-hook #'flyspell-mode)
                 (use-package htmlize)
                 ;; windmove compatibility
                 (add-hook 'org-shiftup-final-hook #'windmove-up)
                 (add-hook 'org-shiftleft-final-hook #'windmove-left)
                 (add-hook 'org-shiftdown-final-hook #'windmove-down)
                 (add-hook 'org-shiftright-final-hook #'windmove-right)
                 (add-hook 'org-mode-hook #'turn-on-auto-fill)
                 (add-hook 'org-mode-hook #'rainbow-identifiers-mode)))

(use-package clojure-mode
  :commands clojure-mode
  :init (add-to-list 'auto-mode-alist '("\\.\\(clj[sx]?\\|dtm\\|edn\\)\\'" . clojure-mode))
  :config (progn (use-package cider
                   :init (progn (add-hook 'clojure-mode-hook #'cider-turn-on-eldoc-mode)
                                (add-hook 'cider-repl-mode-hook #'subword-mode))
                   :config (progn (setq cider-annotate-completion-candidates t)
                                  (define-key cider-repl-mode-map (kbd "M-RET") #'cider-doc)
                                  (define-key cider-mode-map (kbd "M-RET") #'cider-doc)))
                 (use-package clj-refactor
                   :init (progn (add-hook 'clojure-mode-hook (lambda ()
                                                               (clj-refactor-mode 1)
                                                               (cljr-add-keybindings-with-prefix "C-c C-m")))
                                (define-key clojure-mode-map (kbd "C-:") #'clojure-toggle-keyword-string)
                                (define-key clojure-mode-map (kbd "C->") #'cljr-cycle-coll)))
                 (add-hook 'clojure-mode-hook (lambda () (setq buffer-save-without-query t)))
                 (add-hook 'clojure-mode-hook #'subword-mode)
                 ;; Fancy docstrings for schema/defn when in the form:
                 ;; (schema/defn NAME :- TYPE "DOCSTRING" ...)
                 (put 'schema/defn 'clojure-doc-string-elt 4)))

(use-package js2-mode
  :init (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :config (use-package tern
            :commands tern-mode
            :init (add-hook 'js2-mode-hook 'tern-mode)
            :config (progn (use-package company-tern
                             :init (add-to-list 'company-backends #'company-tern))
                           (define-key tern-mode-keymap (kbd "M-.") #'tern-find-definition)
                           (define-key tern-mode-keymap (kbd "C-M-.") #'tern-find-definition-by-name)
                           (define-key tern-mode-keymap (kbd "M-,") #'tern-pop-find-definition)
                           (define-key tern-mode-keymap (kbd "C-c C-r") #'tern-rename-variable)
                           (define-key tern-mode-keymap (kbd "C-c C-c") #'tern-get-type)
                           (define-key tern-mode-keymap (kbd "C-c C-d") #'tern-get-docs)
                           (define-key tern-mode-keymap (kbd "M-<return>") #'tern-get-docs))))

;; Autocomplete end tag when finished writing opening tag
(setq web-mode-auto-close-style 2)

(defun format-buffer ()
  "format buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(global-set-key (kbd "C-S-f") #'format-buffer)

;; Java stuff
(add-hook #'java-mode-hook #'subword-mode)
(use-package dtrt-indent
  :init (add-hook #'java-mode-hook #'dtrt-indent-mode-hook))
(use-package eclim
  :init (add-hook #'java-mode-hook #'eclim-mode)
  :config (progn (use-package company-emacs-eclim
                   :init (company-emacs-eclim-setup))
                 (when (eq system-type 'darwin)
                   (custom-set-variables
                    '(eclim-eclipse-dirs '("/opt/homebrew-cask/Caskroom/eclipse-java/4.4.0/eclipse"))
                    '(eclim-executable "/opt/homebrew-cask/Caskroom/eclipse-java/4.4.0/eclipse/plugins/org.eclim_2.4.0/bin/eclim")))
                 (bind-key "M-." 'eclim-java-find-declaration eclim-mode-map)
                 (bind-key "M-," #'pop-global-mark eclim-mode-map)
                 (bind-key "M-<return>" #'eclim-java-show-documentation-for-current-element eclim-mode-map)))

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

(use-package elpy
  :init (elpy-enable)
  :config (progn (setq elpy-rpc-backend "jedi")
                 (define-key elpy-mode-map (kbd "M-.") #'elpy-goto-definition)
                 (define-key elpy-mode-map (kbd "M-,") #'pop-tag-mark)
                 (define-key elpy-mode-map (kbd "M-<RET>") #'elpy-doc)
                 (add-hook 'python-mode-hook #'rainbow-delimiters-mode)
                 (add-hook 'python-mode-hook #'highlight-symbol-mode)))
