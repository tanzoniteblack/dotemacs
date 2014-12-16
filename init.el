(let ((cask-file (or (locate-file "cask.el" load-path)
                     "~/.cask/cask.el")))
  (require 'cask cask-file))

(cask-initialize)

(add-to-list 'load-path "~/.emacs.d/lib")
(add-to-list 'load-path "~/.emacs.d/config")

(load-library "environment.el")

;;; if local-environment.el file is found in load-path, load it, else skip
(let ((local-environment-file (locate-file "local-environment.el" load-path)))
  (when local-environment-file
    (load-file local-environment-file)))

(load-library "helper-functions.el")
(load-library "window-settings.el")
(load-library "settings.el")
(load-library "file-settings.el")
(load-library "elisp-config.el")
(when (eq system-type 'darwin)
  (load-library "osx-config.el"))
(load-library "ido-conf.el")
(load-library "python-config.el")
(load-library "org-config.el")
(load-library "smartparens-personal-config.el")
(load-library "clojure-mode-config.el")
(load-library "js-config.el")
(load-library "java-config.el")
(load-library "live-fontify-hex-config.el")
(load-library "bindings.el")
(load-library "diminish-config.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((eval web-mode-set-engine "ctemplate")
     (eval web-mode-set-engine "django")
     (eval when
           (and
            (buffer-file-name)
            (file-regular-p
             (buffer-file-name))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (emacs-lisp-mode)
           (when
               (fboundp
                (quote flycheck-mode))
             (flycheck-mode -1))
           (unless
               (featurep
                (quote package-build))
             (let
                 ((load-path
                   (cons ".." load-path)))
               (require
                (quote package-build))))
           (package-build-minor-mode))
     (eval font-lock-add-keywords nil
           (\`
            (((\,
               (concat "("
                       (regexp-opt
                        (quote
                         ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl"))
                        t)
                       "\\_>"))
              1
              (quote font-lock-variable-name-face)))))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
