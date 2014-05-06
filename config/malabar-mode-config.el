(require 'cedet)
(require 'semantic)
(load "semantic/loaddefs.el")
(semantic-mode 1)

(require 'malabar-mode)

(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

(defvar semantic-tags-location-ring (make-ring 20))

(defun semantic-goto-definition (point)
  "Goto definition using semantic-ia-fast-jump
save the pointer marker if tag is found"
  (interactive "d")
  (condition-case err
      (progn
        (ring-insert semantic-tags-location-ring (point-marker))
        (semantic-ia-fast-jump point))
    (error
     ;;if not found remove the tag saved in the ring
     (set-marker (ring-remove semantic-tags-location-ring 0) nil nil)
     (signal (car err) (cdr err)))))

(defun semantic-pop-tag-mark ()
  "popup the tag save by semantic-goto-definition"
  (interactive)
  (if (ring-empty-p semantic-tags-location-ring)
      (message "%s" "No more tags available")
    (let* ((marker (ring-remove semantic-tags-location-ring 0))
           (buff (marker-buffer marker))
           (pos (marker-position marker)))
      (if (not buff)
          (message "Buffer has been deleted")
        (switch-to-buffer buff)
        (goto-char pos))
      (set-marker marker nil nil))))

(add-hook 'malabar-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'malabar-compile-file-silently nil t)))
(add-hook 'malabar-mode-hook 'subword-mode)
(add-hook 'malabar-mode-hook 'hs-minor-mode)

;;; use semantic-ia-fast-jump instead of find-tag in malabar-mode
(define-key malabar-mode-map (kbd "M-,") 'semantic-pop-tag-mark)
(define-key malabar-mode-map (kbd "M-.") 'semantic-goto-definition)
(define-key malabar-mode-map (kbd "C-c C-t") 'malabar-visit-corresponding-test)
(define-key malabar-mode-map (kbd "C-c t") 'malabar-run-test)
(define-key malabar-mode-map (kbd "C-c C-k") 'malabar-compile-file)
(define-key malabar-mode-map (kbd "C-c C-f") 'hs-hide-all)
(define-key malabar-mode-map (kbd "C-c C-o") 'hs-toggle-hiding)

(add-to-list 'ac-sources 'ac-source-semantic)

(require 'dtrt-indent)
(add-hook 'malabar-mode-hook 'dtrt-indent-mode)
