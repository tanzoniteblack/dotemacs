(require 'dtrt-indent)
(add-hook 'java-mode-hook #'dtrt-indent-mode)

(define-key java-mode-map (kbd "M-.") #'find-tag)
(define-key java-mode-map (kbd "M-,") #'pop-tag-mark)
