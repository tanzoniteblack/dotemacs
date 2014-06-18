(require 'eclim)
(global-eclim-mode)

(require 'company-emacs-eclim)
(company-emacs-eclim-setup)

(require 'dtrt-indent)
(add-hook 'java-mode-hook 'dtrt-indent-mode)
