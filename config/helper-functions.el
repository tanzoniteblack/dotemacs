;;handy util fns, many borrowed from emacs-live

(defun insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

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
    (shell-command-on-region b e
                             (concat (if (executable-find "env") "env " "")
                                     "python3 -c 'import sys ;import xml.dom.minidom;s=sys.stdin.read();print(xml.dom.minidom.parseString(s).toprettyxml(newl=\"\"))'")
                             (current-buffer) t)))

(defun sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; monkypatch basic-save-buffer to make saving buffers with no
;; associated file name more intuitive.
(defun live-mp-new-basic-save-buffer ()
  "Save the current buffer in its visited file, if it has been modified.
The hooks `write-contents-functions' and `write-file-functions' get a chance
to do the job of saving; if they do not, then the buffer is saved in
the visited file in the usual way.
Before and after saving the buffer, this function runs
`before-save-hook' and `after-save-hook', respectively."
  (interactive)
  (save-current-buffer
    ;; In an indirect buffer, save its base buffer instead.
    (if (buffer-base-buffer)
        (set-buffer (buffer-base-buffer)))

    (if (or (buffer-modified-p)
            ;; handle the case when no modification has been made but
            ;; the file disappeared since visited
            (and buffer-file-name
                 (not (file-exists-p buffer-file-name))))
        (let ((recent-save (recent-auto-save-p))
              setmodes)
          ;; If buffer has no file name, ask user for one.
          (set-window-buffer (frame-selected-window) (current-buffer))
          (when (or buffer-file-name
                    (y-or-n-p "Buffer has no associated file and not saved. Save it? "))

            (or buffer-file-name
                (let ((filename
                       (expand-file-name
                        (read-file-name (concat "File to save buffer " (buffer-name) " in: ")) nil)))
                  (if (file-exists-p filename)
                      (if (file-directory-p filename)
                          ;; Signal an error if the user specified the name of an
                          ;; existing directory.
                          (error "%s is a directory" filename)
                        (unless (y-or-n-p (format "File `%s' exists; overwrite? "
                                                  filename))
                          (error "Canceled")))
                    ;; Signal an error if the specified name refers to a
                    ;; non-existing directory.
                    (let ((dir (file-name-directory filename)))
                      (unless (file-directory-p dir)
                        (if (file-exists-p dir)
                            (error "%s is not a directory" dir)
                          (error "%s: no such directory" dir)))))
                  (set-visited-file-name filename)))
            (or (verify-visited-file-modtime (current-buffer))
                (not (file-exists-p buffer-file-name))
                (yes-or-no-p
                 (format
                  "%s has changed since visited or saved.  Save anyway? "
                  (file-name-nondirectory buffer-file-name)))
                (error "Save not confirmed"))
            (save-restriction
              (widen)
              (save-excursion
                (and (> (point-max) (point-min))
                     (not find-file-literally)
                     (/= (char-after (1- (point-max))) ?\n)
                     (not (and (eq selective-display t)
                               (= (char-after (1- (point-max))) ?\r)))
                     (or (eq require-final-newline t)
                         (eq require-final-newline 'visit-save)
                         (and require-final-newline
                              (y-or-n-p
                               (format "Buffer %s does not end in newline.  Add one? "
                                       (buffer-name)))))
                     (save-excursion
                       (goto-char (point-max))
                       (insert ?\n))))
              ;; Support VC version backups.
              (vc-before-save)
              (run-hooks 'before-save-hook)
              (or (run-hook-with-args-until-success 'write-contents-functions)
                  (run-hook-with-args-until-success 'local-write-file-hooks)
                  (run-hook-with-args-until-success 'write-file-functions)
                  ;; If a hook returned t, file is already "written".
                  ;; Otherwise, write it the usual way now.
                  (setq setmodes (basic-save-buffer-1)))
              ;; Now we have saved the current buffer.  Let's make sure
              ;; that buffer-file-coding-system is fixed to what
              ;; actually used for saving by binding it locally.
              (if save-buffer-coding-system
                  (setq save-buffer-coding-system last-coding-system-used)
                (setq buffer-file-coding-system last-coding-system-used))
              (setq buffer-file-number
                    (nthcdr 10 (file-attributes buffer-file-name)))
              (if setmodes
                  (condition-case ()
                      (progn
                        (set-file-modes buffer-file-name (car setmodes))
                        (set-file-selinux-context buffer-file-name (nth 1 setmodes)))
                    (error nil))))
            ;; If the auto-save file was recent before this command,
            ;; delete it now.
            (delete-auto-save-file-if-necessary recent-save)
            ;; Support VC `implicit' locking.
            (vc-after-save)
            (run-hooks 'after-save-hook)))
      (message "(No changes need to be saved)"))))
(defalias 'live-mp-orig-basic-save-buffer #'basic-save-buffer)
(defalias 'basic-save-buffer #'live-mp-new-basic-save-buffer)
