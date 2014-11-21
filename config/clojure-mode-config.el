(eval-after-load 'find-file-in-project
  '(add-to-list 'ffip-patterns "*.clj"))

(require 'clojure-mode)
(require 'cider)
(require 'cider-repl)

(add-hook 'clojure-mode-hook
          (lambda ()
            (setq buffer-save-without-query t)))

(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-m")))

(define-key clojure-mode-map (kbd "C-:") #'clojure-toggle-keyword-string)
(define-key clojure-mode-map (kbd "C->") #'cljr-cycle-coll)

(defun format-buffer ()
  "format buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(global-set-key (kbd "C-S-f") #'format-buffer)

(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'cider-repl-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'cider-turn-on-eldoc-mode)
(setq cider-annotate-completion-candidates t)
(require 'clojure-mode-extra-font-locking)

;; Fancy docstrings for schema/defn when in the form:
;; (schema/defn NAME :- TYPE "DOCSTRING" ...)
(put 'schema/defn 'clojure-doc-string-elt 4)
