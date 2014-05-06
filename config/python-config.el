(elpy-enable)

(defvar python-tags-location-ring (make-ring 20))

(defun python-goto-definition (point)
  "Goto definition using elpy-goto-definition
save the pointer marker if tag is found"
  (interactive "d")
  (condition-case err
      (progn
        (ring-insert python-tags-location-ring (point-marker))
        (elpy-goto-definition))
    (error
     ;;if not found remove the tag saved in the ring
     (set-marker (ring-remove python-tags-location-ring 0) nil nil)
     (signal (car err) (cdr err)))))

(defun python-pop-tag-mark ()
  "popup the tag save by semantic-goto-definition"
  (interactive)
  (if (ring-empty-p python-tags-location-ring)
      (message "%s" "No more tags available")
    (let* ((marker (ring-remove python-tags-location-ring 0))
              (buff (marker-buffer marker))
                 (pos (marker-position marker)))
      (if (not buff)
            (message "Buffer has been deleted")
        (switch-to-buffer buff)
        (goto-char pos))
      (set-marker marker nil nil))))

(define-key elpy-mode-map (kbd "M-.") 'python-goto-definition)
(define-key elpy-mode-map (kbd "M-,") 'python-pop-tag-mark)

(add-hook 'python-mode-hook 'rainbow-delimiters-mode)
(add-hook 'python-mode-hook 'highlight-symbol-mode)
