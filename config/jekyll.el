(require 'cl)
(setq website-dir "~/Documents/wannabe-polyglot/")

(defun sluggify (s)
  "Turn a string S into a slug."
  (replace-regexp-in-string " " "-" (downcase (replace-regexp-in-string "[\]\[(){}!#$~^\\]" "" s))))

(defun new-post (title)
  (interactive "MTitle: ")
  (let ((slug (sluggify title))
        (date (current-time)))
    (find-file (concat website-dir "_drafts/" slug ".markdown"))
    (insert "---\n")
    (insert "layout: post\n")
    (insert "title: \"") (insert title) (insert "\"\n")
	(insert "---\n\n")))
