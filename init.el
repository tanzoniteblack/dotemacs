(require 'cask "~/.cask/cask.el")
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
(load-library "elisp-config.el")
(when (eq system-type 'darwin)
  (load-library "osx-config.el"))
(load-library "ido-conf.el")
(load-library "python-config.el")
(load-library "org-config.el")
(load-library "jape-config.el")
(load-library "smartparens-personal-config.el")
(load-library "clojure-mode-config.el")
(load-library "js-config.el")
(load-library "java-config.el")
(load-library "live-fontify-hex-config.el")
(load-library "bindings.el")
(load-library "diminish-config.el")
