(require 'eclim)
(require 'eclimd)
(setq eclimd-wait-for-process nil)
(global-eclim-mode)

(require 'company-emacs-eclim)
(company-emacs-eclim-setup)

(require 'dtrt-indent)
(add-hook 'java-mode-hook 'dtrt-indent-mode)

(defvar eclim-tags-location-ring (make-ring 20))

(defun eclim-goto-definition (point)
  "Goto definition using semantic-ia-fast-jump
save the pointer marker if tag is found"
  (interactive "d")
  (condition-case err
      (progn
        (ring-insert eclim-tags-location-ring (point-marker))
        (eclim-java-find-declaration))
    (error
     ;;if not found remove the tag saved in the ring
     (set-marker (ring-remove eclim-tags-location-ring 0) nil nil)
     (signal (car err) (cdr err)))))

(defun eclim-pop-tag-mark ()
  "popup the tag save by eclim-goto-definition"
  (interactive)
  (if (ring-empty-p eclim-tags-location-ring)
      (message "%s" "No more tags available")
    (let* ((marker (ring-remove eclim-tags-location-ring 0))
           (buff (marker-buffer marker))
           (pos (marker-position marker)))
      (if (not buff)
          (message "Buffer has been deleted")
        (switch-to-buffer buff)
        (goto-char pos))
      (set-marker marker nil nil))))

(define-key java-mode-map (kbd "M-.") 'eclim-goto-definition)
(define-key java-mode-map (kbd "M-,") 'eclim-pop-tag-mark)
(define-key java-mode-map (kbd "C-c C-d") 'eclim-java-show-documentation-for-current-element)

(defun eclim-run-test ()
  "Attempt to run all junit tests provided in current buffer."
  (interactive)
  (if (not (string= major-mode "java-mode"))
	  (message "Sorry cannot run current buffer.")
	(compile (concat eclim-executable " -command java_junit -p " eclim--project-name " -t " (eclim-package-and-class)))))
