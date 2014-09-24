;;; global-company-mode for completions
(require 'company)
(global-company-mode)
(setq company-idle-delay .2
	  company-tooltip-flip-when-above t)
(global-set-key (kbd "C-<tab>") 'company-manual-begin)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

;; more intelligent paren highlighting
(require 'mic-paren)
(paren-activate)

;;; Save backup files in dedicated directory
(setq backup-directory-alist '(("." . "~/.saves")))

;;; undo tree
(require 'undo-tree)
(global-undo-tree-mode)

;;; autocompile emacs-lisp files
(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)

;;; general emacs nicities
(require 'buffer-move)

;;use file path to ensure buffer name uniqueness
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;;store history of recently opened files
(require 'recentf)
(setq recentf-save-file (concat live-tmp-dir "recentf")
      recentf-max-saved-items 200)
(recentf-mode t)

;;When you visit a file, point goes to the last place where it was
;;when you previously visited. Save file is set to live-tmp-dir/places
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat live-tmp-dir "places"))

;;enable winner mode for C-c-(<left>|<right>) to navigate the history
;;of buffer changes i.e. undo a split screen
(when (fboundp 'winner-mode)
  (winner-mode 1))

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
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;; spell checking
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

;;; magit
(require 'git-commit-mode)
(require 'git-rebase-mode)
(require 'gitignore-mode)
(require 'gitconfig-mode)
(require 'magit)

(add-hook 'magit-log-edit-mode-hook
          (lambda ()
            (set-fill-column 72)
            (auto-fill-mode 1)))

(global-set-key (kbd "C-x g") 'magit-status)
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
(setq rcirc-default-nick "tanzoniteblack")
(setq erc-nick "tanzoniteblack")
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;;; sql
(add-hook 'sql-mode-hook '(lambda ()
                            (sql-set-product 'postgres)
                            (setq indent-tabs-mode nil)))

;;; golang
(require 'go-mode)
(when (executable-find "gocode")
  (require 'company-go)
  (add-to-list 'company-backends 'company-go)
  (require 'go-eldoc)
  (add-hook 'go-mode-hook 'go-eldoc-setup))
(define-key go-mode-map (kbd "M-.") 'godef-jump)
(define-key go-mode-map (kbd "M-,") 'pop-tag-mark)
(define-key go-mode-map (kbd "C-S-f") 'gofmt)
(define-key go-mode-map (kbd "M-<return>") 'godef-describe)

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
(global-flycheck-mode)

;;; random keybindings
(global-set-key "\C-x\C-k" 'kill-region)

;; bind kill-paragraph and backward-kill-paragraph
(global-set-key (kbd "C-M-}") 'kill-paragraph)
(global-set-key (kbd "C-M-{") 'backward-kill-paragraph)

;; bind ace-jump-mode to C-c spc
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; set key for expand-region
(global-set-key (kbd "C-=") 'er/expand-region)

;;; enable (up/down)case-region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; set-up M-` as alternative to C-x 5 0 for switching frames
(global-set-key "\M-`" 'other-frame)

;;; ido settings
;; if exact match not found, look for other files containing these characters
(setq ido-enable-flex-matching t)
;; don't leave the current directory if we don't find the file we typed
(setq ido-auto-merge-work-directories-length -1)
;; ido file type ordering preferences
(setq ido-file-extensions-order '(".org" ".clj"))

;;; highlight-symbol
(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.5)
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-prev)

;;; c style
(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode t)

;;; groovy
(require 'groovy-mode)
(add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode))
;;; make Groovy mode electric by default.
;; (add-hook 'groovy-mode-hook
;;           '(lambda ()
;;              (require 'groovy-electric)
;;              (groovy-electric-mode)))

;;; json
(require 'json-mode)
(add-hook 'json-mode-hook 'flycheck-mode)

;;; xml
(require 'nxml-mode)
(define-key nxml-mode-map (kbd "C-S-f") 'beautify-xml)
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.gapp$" . nxml-mode))

;;; quit settings
(global-set-key (kbd "C-x C-c") 'ask-before-closing)

;;; load-other modes
(require 'scala-mode2)
;; (require 'vala-mode)
;; (require 'd-mode)

;;; show human readable file sizes in dired
(setq dired-listing-switches "-alh")

;; turn on rainbow delimiters for all programming modes (in theory, in practice we need to specify a few more)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;; turn on highligh-symbol-mode for programming modes
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

;;; removing whitespace before saving annoys me
(remove-hook 'before-save-hook 'whitespace-cleanup)

(delete-selection-mode 1)

;; start the emacs server
(load "server")
(unless (server-running-p)
  (server-start))

(cua-mode 0)

;;; markdown-mode
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))
(setq markdown-command "gfm"
	  markdown-command-needs-filename t)

;;; browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-highlight-current-entry t)
(setq browse-kill-ring-no-duplicates t)
(setq browse-kill-ring-display-duplicates nil)
(setq browse-kill-ring-highlight-inserted-item nil)

;;; expand region
(require 'expand-region)

;;; ace-jump-mode
(require 'ace-jump-mode)
;;; ace-window
(require 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)

;;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;;; projectile-mode
(require 'projectile)
(projectile-global-mode)
(setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))
;;; add to the globally ignored files
(dolist (file-name '("*~" "*.elc"))
  (add-to-list 'projectile-globally-ignored-files file-name))

;;; respect ansi colors
(require 'ansi-color)
(ansi-color-for-comint-mode-on)

;;; ansi colors in compilation mode
(ignore-errors
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;;; view regexp in buffer
(require 'visual-regexp)

;;; warn when opening files bigger than 100MB (default is 10MB)
(setq large-file-warning-threshold 100000000)

;;; open Cask files in lisp-mode
(add-to-list 'auto-mode-alist '("Cask" . lisp-mode))

(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))

(defun large-file-protector ()
  "Function to be run when opening a file to detect if special large-file changes need made."
  (when (> (buffer-size) (* 8 1024 1024))
	(highlight-symbol-mode -1)
	(company-mode -1)
	(git-gutter-mode -1)
	(setq buffer-read-only t)
	(buffer-disable-undo)))

(add-hook 'find-file-hook 'large-file-protector)

(add-to-list 'auto-mode-alist '("vagrantfile" . ruby-mode))

(require 'jape-mode)
