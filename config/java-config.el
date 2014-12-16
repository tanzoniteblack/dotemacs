(require 'dtrt-indent)
(add-hook #'java-mode-hook #'dtrt-indent-mode)

(require 'eclim)
(add-hook #'java-mode-hook #'eclim-mode)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)

(when (eq system-type 'darwin)
  (custom-set-variables
   '(eclim-eclipse-dirs '("/opt/homebrew-cask/Caskroom/eclipse-java/4.4.0/eclipse"))
   '(eclim-executable "/opt/homebrew-cask/Caskroom/eclipse-java/4.4.0/eclipse/plugins/org.eclim_2.4.0/bin/eclim")))

(define-key eclim-mode-map (kbd "M-.") #'eclim-java-find-declaration)
(define-key eclim-mode-map (kbd "M-,") #'pop-global-mark)
(define-key eclim-mode-map (kbd "M-<return>") #'eclim-java-show-documentation-for-current-element)

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)
