(eval-after-load 'find-file-in-project
  '(add-to-list 'ffip-patterns "*.clj"))

(require 'clojure-mode)
(require 'cider)
(require 'cider-repl)

(add-hook 'clojure-mode-hook
          (lambda ()
            (setq buffer-save-without-query t)))

(require 'align-cljlet)

(defvar clojure-mode-with-hyphens-as-word-sep-syntax-table
  (let ((st (make-syntax-table clojure-mode-syntax-table)))
    (modify-syntax-entry ?- "w" st)
    st))

(defun live-transpose-words-with-hyphens (arg)
  "Treat hyphens as a word character when transposing words"
  (interactive "*p")
  (with-syntax-table clojure-mode-with-hyphens-as-word-sep-syntax-table
    (transpose-words arg)))

(define-key clojure-mode-map (kbd "M-t") 'live-transpose-words-with-hyphens)

(setq auto-mode-alist (append '(("\\.cljs$" . clojure-mode))
                              auto-mode-alist))

(dolist (x '(scheme emacs-lisp lisp clojure cider-repl))
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'smartparens-strict-mode)
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'rainbow-delimiters-mode))

(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-m")))

(define-key clojure-mode-map (kbd "C-:") 'cljr-cycle-stringlike)
(define-key clojure-mode-map (kbd "C->") 'cljr-cycle-coll)

(defun live-warn-when-cider-not-connected ()
  (interactive)
  (message "nREPL server not connected. Run M-x cider or M-x cider-jack-in to connect."))

(define-key clojure-mode-map (kbd "C-M-x")   'live-warn-when-cider-not-connected)
(define-key clojure-mode-map (kbd "C-x C-e") 'live-warn-when-cider-not-connected)
(define-key clojure-mode-map (kbd "C-c C-e") 'live-warn-when-cider-not-connected)
(define-key clojure-mode-map (kbd "C-c C-l") 'live-warn-when-cider-not-connected)
(define-key clojure-mode-map (kbd "C-c C-r") 'live-warn-when-cider-not-connected)

(defun live-delete-and-extract-sexp ()
  "Delete the sexp and return it."
  (interactive)
  (let* ((begin (point)))
    (forward-sexp)
    (let* ((result (buffer-substring-no-properties begin (point))))
      (delete-region begin (point))
      result)))

(defun live-toggle-clj-keyword-string ()
  "convert the string or keyword at (point) from string->keyword or keyword->string."
  (interactive)
  (let* ((original-point (point)))
    (while (and (> (point) 1)
                (not (equal "\"" (buffer-substring-no-properties (point) (+ 1 (point)))))
                (not (equal ":" (buffer-substring-no-properties (point) (+ 1 (point))))))
      (backward-char))
    (cond
     ((equal 1 (point))
      (message "beginning of file reached, this was probably a mistake."))
     ((equal "\"" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert ":" (substring (live-delete-and-extract-sexp) 1 -1)))
     ((equal ":" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "\"" (substring (live-delete-and-extract-sexp) 1) "\"")))
    (goto-char original-point)))

(define-key clojure-mode-map (kbd "C-:") 'live-toggle-clj-keyword-string)

(defun format-buffer ()
  "format buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(global-set-key (kbd "C-S-f") 'format-buffer)

(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)

(require 'clojure-mode-extra-font-locking)
