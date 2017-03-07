(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(setq custom-file "~/.emacs-custom.el")
(let ((custom-file (locate-file custom-file load-path)))
  (when custom-file
	(load custom-file)))

(defun load-external-python-mode ()
  (load (locate-file "python-mode.el" load-path)))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (progn
	(package-refresh-contents)
	(package-install 'use-package)))

(defvar use-package-enable-imenu-support t)

(eval-when-compile
  (require 'use-package))

(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; http://milkbox.net/note/single-file-master-emacs-configuration/
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
	 '(progn ,@body)))

;; Ensure the exec-path honours the shell PATH
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package dash
  :ensure t)

;;; autocompile emacs-lisp files
(use-package auto-compile
  :ensure t
  :config (progn (auto-compile-on-load-mode 1)
				 (auto-compile-on-save-mode 1)))

(add-to-list 'load-path "~/.emacs.d/lib")
(add-to-list 'load-path "~/.emacs.d/config")

(use-package diminish
  :ensure t)

(setenv "LANG" "en_US.UTF-8")

;;; if local-environment.el file is found in load-path, load it, else skip
(let ((local-environment-file (locate-file "local-environment.el" load-path)))
  (when local-environment-file
	(load-file local-environment-file)))

;; handy util fns, many borrowed from emacs-live

(defun insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%D" (current-time))))

(defun live-delete-whitespace-except-one ()
  "Delete all whitespace around point except for 1 space,
includes the deletion of new lines."
  (interactive)
  (just-one-space -1))

(defun delete-adjacent-whitespace (&optional backward-only)
  "Delete all whitespace around point.
If BACKWARD-ONLY is non-nil, only delete them before point."
  (interactive "*P")
  (let ((orig-pos (point)))
	(delete-region
	 (if backward-only
		 orig-pos
	   (progn
		 (skip-chars-forward "[:space:]\n")
		 (constrain-to-field nil orig-pos t)))
	 (progn
	   (skip-chars-backward "[:space:]\n")
	   (constrain-to-field nil orig-pos)))))

(defun live-delete-and-extract-sexp ()
  "Delete the sexp and return it."
  (interactive)
  (let* ((begin (point)))
	(forward-sexp)
	(let* ((result (buffer-substring-no-properties begin (point))))
	  (delete-region begin (point))
	  result)))

(defun copy-file-path ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
					  (file-name-directory default-directory)
					(buffer-file-name))))
	(when filename
	  (kill-new filename nil))))

(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
	  (save-buffers-kill-terminal)
	(message "Canceled exit")))

(defun beautify-xml ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
		(e (if mark-active (max (point) (mark)) (point-max))))
	(shell-command-on-region b e "xmllint --format -" (current-buffer) t)))

(defun sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
	  (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
	(find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun get-ip-addr ()
  (interactive)
  (let ((ipconfig (shell-command-to-string "wget http://ipinfo.io/ip -qO -")))
	(string-match "\\(\\([0-9]+.\\)+[0-9]+\\)" ipconfig)
	(insert (match-string 0 ipconfig))))


(setq inhibit-splash-screen t ; don't load splash screen
	  indicate-buffer-boundaries nil ; don't show where buffer starts/ends
	  jit-lock-defer-time nil
	  jit-lock-stealth-nice 0.1
	  jit-lock-stealth-time 0.2
	  jit-lock-stealth-verbose nil)

;;; don't load menubar
(menu-bar-mode -1)

;; Disable fringe in the minibuffer
(add-hook 'emacs-startup-hook
		  (lambda () (set-window-fringes (minibuffer-window) 0 0 nil)))

;; replace title with buffername
(setq frame-title-format "%b")

;; Force splitting vertically to only happen if new frames will be atleast 90
;; lines tall
(setq split-height-threshold 180)

(setq default-font "Ubuntu Mono")
(setq default-font-height 120)
;;; fraktur font for when we're feeling odd:
;; (setq default-font "UnifrakturMaguntia")
;; (setq default-font-height 130)

;; OS X tends to display things small
(when (eq system-type 'darwin)
  (setq default-font-height 150))

;; Change font to enlarged Ubuntu Mono, if it exists
(set-face-attribute 'default nil :family default-font :height default-font-height :weight 'normal)
'(variable-pitch ((t (:family default-font :slant normal :weight regular :height default-font-height))))

;; Specify Tamil unicode block to use a larger font, otherwise I can't read it without straining
(set-fontset-font t '(#x0B80 . #x0BFF) (font-spec :height (+ default-font-height 20)
												  :family "Noto Sans Tamil"))

;; Specify Telugu unicode block to use a larger font, otherwise I can't read it without straining
(set-fontset-font t '(#x0C00 . #x0C7F) (font-spec :height (+ default-font-height 20)
												  :family "Noto Sans Telugu"))

(defun font-size-mac-laptop ()
  "Set font values to something good for a mac laptop"
  (interactive)
  (let ((default-font-height 160))
	(set-face-attribute 'default nil :height default-font-height :weight 'normal)
	'(variable-pitch ((t (:slant normal :weight regular :height default-font-height))))))

(defun font-size-thunderbolt ()
  "Set font values to something good for a mac laptop"
  (interactive)
  (let ((default-font-height 170))
	(set-face-attribute 'default nil :height default-font-height :weight 'normal)
	'(variable-pitch ((t (:slant normal :weight regular :height default-font-height))))))

(defun toggle-maximized ()
  "Toggle whether window is maximized or not (currently only supports X11 with wmctrl installed)"
  (interactive)
  (when (executable-find "wmctrl")
	(shell-command (concat "wmctrl -i -r "
						   (frame-parameter nil 'outer-window-id)
						   " -btoggle,maximized_vert,maximized_horz"))))

;;; On home laptop, start maximized
(toggle-maximized)

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (if (executable-find "wmctrl")
	  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen")
	(set-frame-parameter nil 'fullscreen (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(setq display-time-day-and-date nil
	  display-time-24hr-format t)
(display-time)

;;; always reuse frames when calling display-buffer
(setq-default display-buffer-reuse-frames t)

(use-package git-gutter-fringe
  :ensure t
  :if window-system
  :config (progn (setq git-gutter-fr:side 'right-fringe)
				 (define-fringe-bitmap 'git-gutter-fr:added
				   [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
				   nil nil 'center)
				 (define-fringe-bitmap 'git-gutter-fr:modified
				   [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
				   nil nil 'center)
				 (define-fringe-bitmap 'git-gutter-fr:deleted
				   [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
				   nil nil 'center)
				 (add-hook 'focus-in-hook 'git-gutter:update-all-windows)
				 (setq-default left-fringe-width (floor (* 0.4 (frame-char-width))))
				 (setq-default right-fringe-width (floor (* 1.2 (frame-char-width))))
				 (global-git-gutter-mode)
				 (diminish 'git-gutter-mode)))

(use-package powerline
  :ensure t)

(use-package moe-theme
  :ensure t
  :config (progn (powerline-moe-theme)
				 (moe-theme-set-color 'purple)
				 (moe-dark)))

;;; expression highlight
(setq show-paren-style 'parenthesis)

;;; global linum-mode
(global-linum-mode)
;;; modify linum space
(setq linum-format "%2d ")
;;; disable linum for certain modes
(defvar linum-disabled-modes-list
  '(eshell-mode wl-summary-mode compilation-mode org-mode dired-mode doc-view-mode image-mode cider-mode cider-repl-mode fundamental-mode shell-mode cider-inspector-mode magit-status-mode special-mode)
  "Modes which shouldn't display line numbers.")

(defun linum-on ()
  "* When linum is running globally, disable line number in modes defined in `linum-disabled-modes-list'. Changed by linum-off."
  (unless (or (minibufferp)
			  (apply #'derived-mode-p linum-disabled-modes-list)
			  (> (buffer-size) (* 5 1024 1024))) ;; disable linum on buffer greater than 5MB, otherwise it's unbearably slow
	(linum-mode 1)))

;;; color directories in file completion
(require 'dircolors)

(setq font-lock-maximum-decoration t
	  color-theme-is-global t)

;;; Line-wrapping
(set-default 'fill-column 80)

;;; ;get rid of clutter
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;;; remove bells
(setq ring-bell-function 'ignore)

;; Highlight tabulations
(setq highlight-tabs t)

;; Show trailing white spaces
(setq show-trailing-whitespace t)

;;; add font-lock for dash.el
(eval-after-load "dash" '(dash-enable-font-lock))

(defun adjust-gc-cons-threshold-max ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun reset-gc-cons-threshold ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'adjust-gc-cons-threshold-max)
(add-hook 'minibuffer-exit-hook #'reset-gc-cons-threshold)

;; tmp-directory
(setq live-tmp-dir "~/.emacs.d/tmp/")

;; Use unix line endings by default, even in Windows
(setq-default buffer-file-coding-system 'utf-8-unix)

(use-package rainbow-delimiters
  :ensure t
  :defer t)

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
  :diminish company-mode
  :config (progn (setq company-idle-delay .2
					   company-dabbrev-ignore-case t
					   company-dabbrev-code-ignore-case t
					   company-dabbrev-downcase nil
					   company-tooltip-flip-when-above t)
				 (bind-key "C-<tab>" 'company-manual-begin)
				 (bind-key "C-n" 'company-select-next company-active-map)
				 (bind-key "C-p" 'company-select-previous company-active-map)
				 (global-company-mode)))

;;; Save backup files in dedicated directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
	  auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
	  create-lockfiles nil)

(use-package ido
  :ensure t
  :config (progn (setq ido-enable-prefix nil
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
				 (ido-mode t)))

(use-package flx-ido
  :ensure t
  :config (flx-ido-mode 1))

(use-package ido-ubiquitous
  :ensure t
  :config (ido-ubiquitous-mode 1))

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

(setq initial-major-mode 'fundamental-mode
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
(setq indent-tabs-mode nil)
(auto-compression-mode t)
(show-paren-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

;;default to unified diffs
(use-package ediff
  :defer t
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
  :commands (flyspell-mode flyspell-prog-mode turn-off-flyspell)
  :init (progn (setq ispell-program-name "aspell" ; use aspell instead of ispell
					 ispell-extra-args '("--sug-mode=ultra"))
			   (add-hook 'text-mode-hook 'flyspell-mode)
			   (add-hook 'prog-mode-hook
						 (lambda ()
						   (flyspell-prog-mode)))
			   (add-hook 'sh-mode
						 (lambda ()
						   (turn-off-flyspell)))))

(eval-after-load "flyspell"
  '(diminish 'flyspell-mode))

;;; magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  ;; Don't show setup instructions
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  (defadvice vc-mode-line (after strip-backend () activate)
	(when (stringp vc-mode)
	  (let ((noback (replace-regexp-in-string
					 (format "^ %s[:-]?" (vc-backend buffer-file-name))
					 " " vc-mode)))
		(setq vc-mode noback))))
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
				  magit-save-repository-buffers nil
				  magit-popup-use-prefix-argument 'default)))

(use-package gitconfig-mode
  :ensure t)

(use-package gitignore-mode
  :ensure t)

(use-package erc
  :defer t
  :config (progn (setq erc-nick "tanzoniteblack")
				 (setq erc-hide-list '("JOIN" "PART" "QUIT"))))

;;; sql
(add-hook 'sql-mode-hook '(lambda ()
							(sql-set-product 'mysql)
							(setq indent-tabs-mode nil)
							(bind-key "C-j" 'newline sql-mode-map)))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(use-package sqlup-mode
  :ensure t
  :init (add-hook 'sql-mode-hook 'sqlup-mode))

(use-package go-mode
  :ensure t
  :commands go-mode
  :config (progn (bind-key "M-." 'godef-jump go-mode-map)
				 (bind-key "M-," 'pop-tag-mark go-mode-map)
				 (bind-key "C-S-F" 'gofmt go-mode-map)
				 (bind-key "M-<return>" 'godef-describe go-mode-map)))

(use-package company-go
  :ensure t
  :if (executable-find "gocode")
  :commands company-go
  :config (add-to-list 'company-backends 'company-go))

(use-package go-eldoc
  :ensure t
  :if (executable-find "gocode")
  :commands go-eldoc-setup
  :config (add-to-list 'go-mode-hook 'go-eldoc-setup))

;;; flycheck mode
(use-package flycheck
  :ensure t
  :commands global-flycheck-mode
  :config (progn (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers
																  '(javascript-jshint
																	emacs-lisp-checkdoc))
							   flycheck-mode-line-prefix " Φ")
				 (flycheck-add-mode 'javascript-eslint 'web-mode)
				 (global-flycheck-mode)
				 (flycheck-define-checker proselint
				   "A linter for prose."
				   :command ("proselint" source-inplace)
				   :error-patterns
				   ((warning line-start (file-name) ":" line ":" column ": "
							 (id (one-or-more (not (any " "))))
							 (message) line-end))
				   :modes (text-mode markdown-mode gfm-mode))

				 (add-to-list 'flycheck-checkers 'proselint)
				 (setq flycheck-scalastyle-jar (concat (getenv "HOME") "/.emacs.d/scalastyle_2.11-0.7.0-batch.jar")
					   flycheck-scalastylerc (concat (getenv "HOME") "/.emacs.d/scalastyle_config.xml")
					   flycheck-flake8-maximum-line-length 160)))

(use-package popup
  :ensure t)

(use-package flycheck-pos-tip
  :ensure t
  :config (setq flycheck-display-errors-function 'flycheck-pos-tip-error-messages))

(use-package avy
  :ensure t
  :bind (("C-c SPC" . avy-goto-char)
		 ;; ("C-c S-SPC" . avy-goto-char)
		 ("M-g g" . avy-goto-line))
  :config (progn (eval-after-load 'conf-mode
				   '(bind-key "C-c SPC" 'avy-goto-word-1 conf-mode-map))))

;; (use-package ace-pinyin
;;   :ensure t
;;   :commands ace-pinyin-mode
;;   ;; :config (ace-pinyin-global-mode +1)
;;   )

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
  :commands (highlight-symbol-mode)
  :bind (("C-<f3>" . highlight-symbol-at-point)
		 ("<f3>" . highlight-symbol-next)
		 ("S-<f3>" . highlight-symbol-prev)
		 ("M-<f3>" . highlight-symbol-prev))
  :init (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  :config (setq highlight-symbol-idle-delay 0.5))

(use-package json-mode
  :ensure t
  :defer t
  :init (progn (add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
			   (add-to-list 'auto-mode-alist '("\\.jsonld$" . json-mode))
			   (add-to-list 'auto-mode-alist '(".tern-project" . json-mode))
			   (add-to-list 'auto-mode-alist '(".tern-config" . json-mode))
			   (add-to-list 'auto-mode-alist '(".jshintrc" . json-mode))
			   (add-to-list 'auto-mode-alist '(".eslintrc" . json-mode)))
  :config (progn (add-hook 'json-mode-hook 'flycheck-mode)
				 (bind-key "C-S-f" 'json-mode-beautify json-mode-map)
				 (setq json-reformat:pretty-string? t)))

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

(use-package projectile
  :ensure t
  :config (progn (setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name)))
					   projectile-create-missing-test-files t)
				 ;; add to the globally ignored files
				 (dolist (file-name '("*~" "*.elc"))
				   (add-to-list 'projectile-globally-ignored-files file-name))
				 (when (eq system-type 'windows-nt)
				   (setq projectile-indexing-method 'alien))

				 (defun run-junit-test-unit (universal-arg)
				   (interactive "P")
				   (let* ((file-name (buffer-file-name))
						  (class-name (car (split-string
											(car (last (split-string file-name "/")))
											"\\.")))
						  (root (projectile-project-root))
						  (test-name (when universal-arg
									   (read-string (projectile-prepend-project-name "Run test method: ")
													(projectile-symbol-or-selection-at-point))))
						  (mvn-cmd (concat "cd " root " && "
										   "mvn -DfailIfNoTests=false -Dtest=" class-name (when test-name (concat "#" test-name))
										   " test ")))
					 (projectile-run-compilation mvn-cmd)))
				 (define-key projectile-mode-map (kbd "C-x t u") 'run-junit-test-unit)

				 (defun projectile-lein-ancient ()
				   (interactive)
				   (projectile-run-compilation "lein with-profile ancient ancient :all"))

				 (projectile-register-project-type 'npm '("package.json") "npm run build-dev" "npm run test")

				 (projectile-global-mode)))

;;; respect ansi colors
(ansi-color-for-comint-mode-on)

;;; ansi colors in compilation mode
(ignore-errors
  (add-to-list 'compilation-environment "TERM=xterm-256color")
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
  :defer t
  :config (progn (setq org-completion-use-ido t
					   org-outline-path-complete-in-steps nil
					   org-startup-indented nil
					   org-hide-leading-stars t
					   org-agenda-files (list "~/Dropbox/.org/yummly.org"
											  "~/Dropbox/.org/home.org"
											  "~/Dropbox/.org/beer.org")
					   org-directory "~/Dropbox/.org/"
					   org-display-inline-images t
					   org-deadline-warning-days 3
					   org-log-done 'time
					   org-src-fontify-natively t
					   org-src-tab-acts-natively t)
				 ;; if all children of a TODO are done, then change status of TODO to DONE
				 (defun org-summary-todo (n-done n-not-done)
				   "Switch entry to DONE when all subentries are done, to TODO otherwise."
				   (let (org-log-done org-log-states)   ; turn off logging
					 (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
				 (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
				 ;; remap ace--word-mode (org-mode automatically disables)
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
				 (add-hook 'org-mode-hook 'turn-on-auto-fill)

				 (defun endless/org-ispell ()
				   "Configure `ispell-skip-region-alist' for `org-mode'."
				   (make-local-variable 'ispell-skip-region-alist)
				   (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
				   (add-to-list 'ispell-skip-region-alist '("~" "~"))
				   (add-to-list 'ispell-skip-region-alist '("=" "="))
				   (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
				 (add-hook 'org-mode-hook #'endless/org-ispell)

				 (defun kill-org-src-buffers (&rest args)
				   "Kill temporary buffers created by
org-src-font-lock-fontify-block so they don't interfere with
magit-mode."
				   (dolist (b (buffer-list))
					 (let ((bufname (buffer-name b)))
					   (if (string-prefix-p " org-src-fontification:" bufname)
						   (kill-buffer b)))))

				 (advice-add 'org-src-font-lock-fontify-block
							 :after #'kill-org-src-buffers)))

;; org-mode export in github flavored markdown
(use-package ox-gfm
  :ensure t)

(use-package clojure-mode
  :ensure t
  :defer t
  :config (progn (setq clojure-align-forms-automatically t)
				 (add-hook 'clojure-mode-hook (lambda ()
												(setq buffer-save-without-query t)))
				 (add-hook 'clojure-mode-hook 'subword-mode)
				 (add-hook 'clojure-mode-hook
						   (lambda ()
							 (define-key clojure-mode-map (kbd "C-c SPC") 'avy-goto-word-1)))
				 ;; Fancy docstrings for schema/defn when in the form:
				 ;; (schema/defn NAME :- TYPE "DOCSTRING" ...)
				 (put 'schema/defn 'clojure-doc-string-elt 4)))

(use-package clojure-mode-extra-font-locking
  :ensure t)

(use-package cider
  :ensure t
  :config (progn (add-hook 'clojure-mode-hook 'cider-mode)
				 (add-hook 'clojure-mode-hook 'eldoc-mode)
				 (add-hook 'cider-repl-mode-hook 'eldoc-mode)
				 (add-hook 'cider-repl-mode-hook 'subword-mode)
				 (setq cider-annotate-completion-candidates t
					   cider-mode-line " cider"
					   cider-prompt-for-symbol nil
					   cider-cljs-lein-repl "(do (user/run) (user/browser-repl))")
				 (add-hook 'cider-mode-hook
						   (lambda ()
							 (define-key cider-repl-mode-map (kbd "M-RET") 'cider-doc)
							 (define-key cider-mode-map (kbd "M-RET") 'cider-doc)
							 (define-key cider-mode-map (kbd "C-c SPC") 'avy-goto-word-1)))
				 (add-to-list 'cider-jack-in-dependencies `("criterium" "0.4.3"))))

(use-package clj-refactor
  :ensure t
  :config (progn (setq cljr-suppress-middleware-warnings t)
				 (add-hook 'cider-mode-hook (lambda ()
											  (clj-refactor-mode 1)
											  (cljr-add-keybindings-with-prefix "C-c C-m")))
				 (add-hook 'cider-mode-hook 'yas-minor-mode)
				 (define-key cider-mode-map (kbd "C-:") 'clojure-toggle-keyword-string)
				 (define-key cider-mode-map (kbd "C-M-r") 'hydra-cljr-help-menu/body)
				 (define-key cider-mode-map (kbd "C-c C-x") 'cider-pprint-eval-last-sexp)
				 (define-key cider-repl-mode-map (kbd "C-c C-x") 'cider-pprint-eval-last-sexp)))

(use-package js2-mode
  :ensure t)

(use-package tern
  :ensure t
  :commands (tern-mode)
  :init (progn (add-hook 'js2-mode-hook 'tern-mode)
			   (setq js2-include-node-externs t
					 js2-include-browser-externs t
					 js2-basic-offset 2
					 js2-indent-line 2
					 js2-bounce-indent-p t
					 js2-pretty-multiline-declarations t))
  :config (progn (define-key tern-mode-keymap (kbd "M-.") 'tern-find-definition)
				 (define-key tern-mode-keymap (kbd "C-M-.") 'tern-find-definition-by-name)
				 (define-key tern-mode-keymap (kbd "M-,") 'tern-pop-find-definition)
				 (define-key tern-mode-keymap (kbd "C-c C-r") 'tern-rename-variable)
				 (define-key tern-mode-keymap (kbd "C-c C-c") 'tern-get-type)
				 (define-key tern-mode-keymap (kbd "C-c C-d") 'tern-get-docs)
				 (define-key tern-mode-keymap (kbd "M-<return>") 'tern-get-docs)))

(use-package company-tern
  :ensure t
  :commands company-tern
  :init (add-to-list 'company-backends 'company-tern))

(use-package stylus-mode
  :ensure t)

(use-package cc-mode
  :defer t
  :config (progn (setq-default c-basic-offset 4 c-default-style "linux")
				 (setq-default tab-width 4 indent-tabs-mode t)
				 (add-hook 'java-mode-hook 'subword-mode)))

(use-package dtrt-indent
  :ensure t
  :init (add-hook 'java-mode-hook 'dtrt-indent-mode))

(use-package meghanada
  :ensure t
  :commands meghanada-mode
  :init (add-hook 'java-mode-hook 'meghanada-mode))

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

(use-package python-mode
  :init (load-external-python-mode)
  :ensure t)

(use-package elpy
  :ensure t
  :commands (elpy-enable)
  :init (progn (with-eval-after-load 'python-mode (elpy-enable))
			   (add-to-list 'display-buffer-alist
							`(,"*Python*"
							  (display-buffer-reuse-window))))
  :config (progn ;; (delete 'elpy-module-highlight-indentation elpy-modules)
			(setq elpy-rpc-backend "jedi")
			(define-key elpy-mode-map (kbd "M-.") 'elpy-goto-definition)
			(define-key elpy-mode-map (kbd "M-,") 'pop-tag-mark)
			(define-key elpy-mode-map (kbd "M-<RET>") 'elpy-doc)
			(add-hook 'python-mode-hook 'rainbow-delimiters-mode)
			(add-hook 'python-mode-hook 'highlight-symbol-mode)
			(add-hook 'python-mode-hook 'subword-mode)
			(setenv "WORKON_HOME" "~/.pyenv/versions/")
			(setq ansi-color-for-comint-mode t)
			(elpy-use-ipython "ipython")
			(setq python-shell-prompt-detect-failure-warning nil
				  python-shell-interpreter-args "-i --simple-prompt")
			(add-hook 'elpy-mode-hook
					  (lambda ()
						(define-key elpy-mode-map (kbd "C-S-f") 'elpy-yapf-fix-code)
						(define-key inferior-python-mode-map (kbd "C-c SPC") 'avy-goto-word-1)))))

(defun enable-lisp-hooks (mode-name)
  "Enable lisp-y goodness for MODE-NAME."
  (let ((mode-hook (intern (concat (symbol-name mode-name) "-hook"))))
	(add-hook mode-hook 'rainbow-delimiters-mode)))

(use-package smartparens
  :ensure t
  :commands (smartparens-global-mode smartparens-mode)
  :init (smartparens-global-mode t)
  :config (progn (require 'smartparens-config)
				 ;; highlights matching pairs
				 (show-smartparens-global-mode t)
				 ;; custom keybindings for smartparens mode
				 (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
				 (define-key smartparens-mode-map (kbd "M-(") 'sp-forward-barf-sexp)
				 (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
				 (define-key smartparens-mode-map (kbd "M-)") 'sp-forward-slurp-sexp)

				 (define-key smartparens-mode-map (kbd "M-d") 'kill-sexp)
				 (define-key smartparens-mode-map (kbd "M-D") 'sp-kill-sexp)
				 (define-key smartparens-mode-map (kbd "s-S") 'sp-split-sexp)
				 (define-key smartparens-mode-map (kbd "M-<up>") 'sp-raise-sexp)

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
  :defer t
  :ensure t)

(use-package scala-mode
  :ensure t)

(use-package ensime
  :ensure t
  :commands (ensime-scala-mode-hook)
  :init (progn (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))
  :config (progn (setq ensime-startup-snapshot-notification nil
					   ensime-startup-notification nil)
				 (define-key ensime-mode-map (kbd "M-<RET>") 'ensime-show-doc-for-symbol-at-point)
				 (add-hook 'ensime-inf-mode '(lambda () (define-key ensime-inf-mode-map (kbd "C-c SPC") 'avy-goto-word-1)))))

(use-package d-mode
  :defer t
  :ensure t)

(use-package groovy-mode
  :ensure t
  :defer t
  :commands (groovy-mode)
  :init (add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode)))

;; (use-package vala-mode
;;   :ensure t)

(use-package rust-mode
  :defer t
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :init (add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode)))

(defun string/ends-with (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
					 string)
	   t))

(use-package web-mode
  :ensure t
  :commands web-mode
  :init (progn (add-to-list 'auto-mode-alist '("\\.phtml$" . web-mode))
			   (add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
			   (add-to-list 'auto-mode-alist '("\\.[gj]sp$" . web-mode))
			   (add-to-list 'auto-mode-alist '("\\.as[cp]x$" . web-mode))
			   (add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
			   (add-to-list 'auto-mode-alist '("\\.mustache$" . web-mode))
			   (add-to-list 'auto-mode-alist '("\\.djhtml$" . web-mode))
			   (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
			   (add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
			   (add-to-list 'auto-mode-alist '("\\.ejs$" . web-mode))
			   (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode)))
  :config (progn (defun my-web-mode-hook ()
				   (setq web-mode-enable-auto-pairing nil))

				 (add-hook 'web-mode-hook  'my-web-mode-hook)
				 (defadvice web-mode-highlight-part (around tweak-jsx activate)
				   (if (equal web-mode-content-type "jsx")
					   (let ((web-mode-enable-part-face nil))
						 ad-do-it)
					 ad-do-it))
				 (defun sp-web-mode-is-code-context (id action context)
				   (when (and (eq action 'insert)
							  (not (or (get-text-property (point) 'part-side)
									   (get-text-property (point) 'block-side))))

					 t))
				 (defun webmode-jsx-setup ()
				   (when (or (string/ends-with buffer-file-name ".js")
							 (string/ends-with buffer-file-name ".jsx"))
					 (yas-minor-mode)
					 (yas-activate-extra-mode 'js-mode)
					 (web-mode-set-content-type "jsx")
					 (setq-local web-mode-enable-auto-quoting nil)
					 ;; (setq-local web-mode-code-indent-offset 4)
					 ;; (setq-local web-mode-markup-indent-offset 4)
					 ;; (setq-local web-mode-attr-indent-offset 4)
					 ;; (setq-local web-mode-attr-value-indent-offset 4)
					 ;; (setq-default indent-tabs-mode nil)
					 (tern-mode)))
				 (add-hook 'web-mode-hook 'webmode-jsx-setup)
				 (add-hook 'web-mode-hook 'subword-mode)
				 (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))
				 (sp-local-pair 'web-mode "{" "}")))

(use-package ruby-mode
  :ensure t
  :commands ruby-mode
  :init (progn (add-to-list 'auto-mode-alist '("vagrantfile" . ruby-mode))
			   (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))))

(setq line-move-visual t)

(use-package git-link
  :commands (git-link git-link-commit)
  :ensure t)

(use-package jape-mode
  :defer t
  :ensure t)

(use-package yaml-mode
  :defer t
  :ensure t)

(use-package auto-package-update
  :ensure t
  :commands auto-package-update-maybe)

(setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")

(use-package rainbow-identifiers
  :ensure t
  :config (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))

(use-package anzu
  :ensure t
  :config (global-anzu-mode 1)
  :bind (("M-%" . anzu-query-replace)
		 ("C-M-%" . anzu-query-replace-regexp)))

;; (use-package graphviz-dot-mode
;;   :ensure t
;;   :mode "\\.dot$"
;;   :config (progn (defun graphviz-compile-and-preview ()
;;                    (interactive)
;;                    (if (buffer-file-name)
;;                        (progn (shell-command (concat graphviz-dot-dot-program
;;                                                      " -T" graphviz-dot-preview-extension " "
;;                                                      (shell-quote-argument buffer-file-name)
;;                                                      " -o "
;;                                                      (shell-quote-argument
;;                                                       (concat (file-name-sans-extension buffer-file-name)
;;                                                               "." graphviz-dot-preview-extension))))
;;                               (call-interactively 'graphviz-dot-preview))))
;;                  (add-hook 'graphviz-dot-mode-hook
;;                            (lambda ()
;;                              (add-hook 'after-save-hook 'graphviz-compile-and-preview nil 'make-it-local)))))

(use-package popup-imenu
  :ensure t
  :commands popup-imenu
  :bind ("M-i" . popup-imenu))

;; use gfm-mode (github formatted md) instead of regular markdown-mode

(add-to-list 'auto-mode-alist '("\\.gate$" . xml-mode))

;;; open Cask files in lisp-mode
(add-to-list 'auto-mode-alist '("Cask" . lisp-mode))

;; recognize .zsh files for sh-mode
(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh-theme$" . sh-mode))

(add-to-list 'auto-minor-mode-alist '("My Clippings.txt" . read-only-mode))

(when (eq system-type 'darwin)
  ;; Fix keyboard alt/meta keys
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'meta)

  ;; OS X specific configuration
  ;; ---------------------------

  ;; Make cut and paste work with the OS X clipboard

  (defun live-copy-from-osx ()
	(shell-command-to-string "pbpaste"))

  (defun live-paste-to-osx (text &optional push)
	(let ((process-connection-type nil))
	  (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
		(process-send-string proc text)
		(process-send-eof proc))))

  (when (not window-system)
	(setq interprogram-cut-function #'live-paste-to-osx)
	(setq interprogram-paste-function #'live-copy-from-osx))

  ;; Work around a bug on OS X where system-name is a fully qualified
  ;; domain name
  (setq system-name (car (split-string system-name "\\.")))

  ;; Ignore .DS_Store files with ido mode
  (add-to-list 'ido-ignore-files "\\.DS_Store"))

(when (eq system-type 'windows-nt)
  (load-library "windows-config.el"))

(load-library "live-fontify-hex-config.el")
(load-library "bindings.el")
(load-library "jekyll.el")

(use-package php-mode
  :ensure t
  :defer t)

(use-package crux
  :ensure t
  :bind (("C-a" . crux-move-beginning-of-line)
		 ("C-S-f" . crux-cleanup-buffer-or-region))
  :demand
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify))

(use-package imenu-anywhere
  :ensure t
  :commands imenu-anywhere
  :bind (("C-c ." . imenu-anywhere))
  :init (add-hook 'java-mode-hook
				  (lambda ()
					(define-key java-mode-map (kbd "C-c .") 'imenu-anywhere)))
  (add-hook 'python-mode-hook
			(lambda ()
			  (define-key python-mode-map (kbd "C-c .") 'imenu-anywhere))))

(eval-after-load "auto-revert-mode"
  '(diminish 'auto-revert-mode))

(eval-after-load 'yas-minor-mode
  '(diminish 'yas-minor-mode))

(diminish 'subword-mode)

(use-package systemd
  :ensure t)

(add-to-list 'auto-mode-alist '(".ovpn" . conf-mode))
(load-external-python-mode)

(use-package which-key
  :ensure t
  :demand
  :config (which-key-mode))

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;;; if local-environment.el file is found in load-path, load it, else skip
(let ((local-environment-file (locate-file "local-extras.el" load-path)))
  (when local-environment-file
	(load-file local-environment-file)))
