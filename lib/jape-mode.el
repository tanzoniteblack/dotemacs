;; An Emacs editing mode mode for GATE's JAPE files
;; Copyright (C) 2005 Ilya Goldin -- http://www.pitt.edu/~goldin

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version. If you have
;; received this program together with the GATE software, you may
;; choose to use this program under the whatever license applies to
;; GATE itself.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

;; version 0.1.1

;; Based heavily on "An Emacs language mode creation tutorial" by
;; Scott Andrew Borton, http://two-wugs.net/emacs/mode-tutorial.html

(defvar jape-mode-hook nil)

(defvar jape-mode-map
;  (let ((jape-mode-map (make-keymap)))
  (let ((jape-mode-map (make-sparse-keymap)))
;    (define-key jape-mode-map "\C-j" 'newline-and-indent)
    jape-mode-map)
  "Keymap for JAPE major mode")

(add-to-list 'auto-mode-alist '("\\.jape\\'" . jape-mode))

(defconst jape-font-lock-keywords-1
  (list
; (regexp-opt '("Phase:" "Multiphase:" "Input:" "Rule:" "Options:") t)
   '("\\(\\(?:Input\\|Multiphase\\|Macro\\|Options\\|\\(?:Phas\\|Rul\\)e\\):\\)" .
     font-lock-builtin-face)
   '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Minimal highlighting expressions for JAPE mode")

(defvar jape-font-lock-keywords jape-font-lock-keywords-1
  "Default highlighting expressions for JAPE mode")

(defvar jape-mode-syntax-table
  (let ((jape-mode-syntax-table (make-syntax-table)))

; Comment styles are same as C++
    (modify-syntax-entry ?/ ". 124b" jape-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" jape-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" jape-mode-syntax-table)
    jape-mode-syntax-table)
  "Syntax table for jape-mode")

(defun jape-mode ()
  "Major mode for editing GATE's JAPE files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table jape-mode-syntax-table)
  (use-local-map jape-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(jape-font-lock-keywords))
; (set (make-local-variable 'indent-line-function) 'jape-indent-line)
  (set (make-local-variable 'comment-start) "//")
  (setq major-mode 'jape-mode)
  (setq mode-name "JAPE")
  (run-hooks 'jape-mode-hook))


(provide 'jape-mode)
