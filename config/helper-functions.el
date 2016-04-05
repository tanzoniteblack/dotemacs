;;handy util fns, many borrowed from emacs-live

(defun insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%D" (current-time))))

(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun live-delete-whitespace-except-one ()
  "Delete all whitespace around point except for 1 space,
includes the deletion of new lines."
  (interactive)
  (just-one-space -1))

(defun delete-adjacent-whitespace (&optional backward-only)
  "Delete all whitespace around point.
If BACKWARD-ONLY is non-nil, only delete them before point."
  (interactive "*P")
  (let ((orig-pos (point)))
    (delete-region
     (if backward-only
         orig-pos
       (progn
         (skip-chars-forward "[:space:]\n")
         (constrain-to-field nil orig-pos t)))
     (progn
       (skip-chars-backward "[:space:]\n")
       (constrain-to-field nil orig-pos)))))

(defun live-delete-and-extract-sexp ()
  "Delete the sexp and return it."
  (interactive)
  (let* ((begin (point)))
    (forward-sexp)
    (let* ((result (buffer-substring-no-properties begin (point))))
      (delete-region begin (point))
      result)))

(defun copy-file-path ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      (file-name-directory default-directory)
                    (buffer-file-name))))
    (when filename
      (kill-new filename nil))))

(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

(defun beautify-xml ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e "xmllint --format -" (current-buffer) t)))

(defun sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun get-ip-addr ()
  (interactive)
  (let ((ipconfig (shell-command-to-string "wget http://ipinfo.io/ip -qO -")))
    (string-match "\\(\\([0-9]+.\\)+[0-9]+\\)" ipconfig)
    (insert (match-string 0 ipconfig))))
