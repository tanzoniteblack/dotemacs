(let ((gc-cons-threshold most-positive-fixnum))
  (require 'package)
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

  (package-initialize)

  ;; Bootstrap `use-package'
  (unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

  (require 'use-package)

  ;; http://milkbox.net/note/single-file-master-emacs-configuration/
  (defmacro after (mode &rest body)
	"`eval-after-load' MODE evaluate BODY."
	(declare (indent defun))
	`(eval-after-load ,mode
	   '(progn ,@body)))

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

  (load-library "environment.el")

;;; if local-environment.el file is found in load-path, load it, else skip
  (let ((local-environment-file (locate-file "local-environment.el" load-path)))
	(when local-environment-file
	  (load-file local-environment-file)))

  (load-library "helper-functions.el")
  (load-library "window-settings.el")
  (load-library "settings.el")
  (load-library "file-settings.el")
  (when (eq system-type 'darwin)
	(load-library "osx-config.el"))
  (when (eq system-type 'windows-nt)
	(load-library "windows-config.el"))
  (load-library "live-fontify-hex-config.el")
  (load-library "bindings.el"))
