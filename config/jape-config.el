;; jape-mode
; ---- JAPE editing mode for GATE's JAPE files, jape-mode.el
(add-to-list 'auto-mode-alist '("\\.jape$" . jape-mode))
(autoload 'jape-mode "jape-mode" "Mode for editing GATE's JAPE files")
(add-hook 'jape-mode-hook 'rainbow-delimiters-mode)

(defvar jape-indent)

;; set tab for jape-mode
(add-hook 'jape-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (setq jape-indent 4)))
