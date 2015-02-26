;; don't load splash screen
(setq inhibit-splash-screen t)

;;; don't load menubar
(menu-bar-mode -1)

;; replace title with buffername
(setq frame-title-format "%b")

;; Force splitting vertically to only happen if new frames will be atleast 90
;; lines tall
(setq split-height-threshold 180)
;; Force splitting horizontally to only happen if new frames will be atleast 70
(setq split-width-threshold 140)

(defvar default-font "Ubuntu Mono")
(defvar default-font-height 120)
;;; fraktur font for when we're feeling odd:
;; (setq default-font "UnifrakturMaguntia")
;; (setq default-font-height 130)

;; OS X tends to display things small
(when (eq system-type 'darwin)
  (setq default-font-height 165))

;; Change font to enlarged Ubuntu Mono, if it exists
(when (member default-font (font-family-list))
  (set-face-attribute 'default nil :family default-font :height default-font-height :weight 'normal)
  '(variable-pitch ((t (:family default-font :slant normal :weight regular :height default-font-height)))))

(defun font-size-mac-laptop ()
  "Set font values to something good for a mac laptop"
  (interactive)
  (let ((default-font-height 155))
	(set-face-attribute 'default nil :height default-font-height :weight 'normal)
	'(variable-pitch ((t (:slant normal :weight regular :height default-font-height))))))

(defun font-size-thunderbolt ()
  "Set font values to something good for a mac laptop"
  (interactive)
  (let ((default-font-height 165))
	(set-face-attribute 'default nil :height default-font-height :weight 'normal)
	'(variable-pitch ((t (:slant normal :weight regular :height default-font-height))))))

(defun toggle-maximized ()
  "Toggle whether window is maximized or not (currently only supports X11 with wmctrl installed)"
  (interactive)
  (when (executable-find "wmctrl")
    (shell-command (concat "wmctrl -i -r "
                           (frame-parameter nil 'outer-window-id)
                           " -btoggle,maximized_vert,maximized_horz"))))

;;; On home laptop, start maximized
(toggle-maximized)

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (if (executable-find "wmctrl")
      (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen")
    (set-frame-parameter nil 'fullscreen (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(setq display-time-day-and-date nil
      display-time-24hr-format t)
(display-time)

;;; always reuse frames when calling display-buffer
(setq-default display-buffer-reuse-frames t)

(use-package git-gutter-fringe
  :ensure t
  :if window-system
  :init (progn (setq git-gutter-fr:side 'right-fringe)
               (setq-default left-fringe-width 2)
               (setq-default right-fringe-width 12)
               (global-git-gutter-mode)
               (diminish 'git-gutter-mode "")))

(use-package powerline
  :ensure t)

(use-package moe-theme
  :ensure t
  :init (moe-dark)
  :config (progn (powerline-moe-theme)
                 (moe-theme-set-color 'purple)))

;;; expression highlight
(setq show-paren-style 'expression)

;;; global linum-mode
(global-linum-mode)
;;; modify linum space
(setq linum-format "%2d ")
;;; disable linum for certain modes
(setq linum-disabled-modes-list '(eshell-mode wl-summary-mode compilation-mode org-mode dired-mode doc-view-mode image-mode cider-mode cider-repl-mode fundamental-mode shell-mode cider-inspector-mode magit-status-mode special-mode))

(defun linum-on ()
  "* When linum is running globally, disable line number in modes defined in `linum-disabled-modes-list'. Changed by linum-off."
  (unless (or (minibufferp)
              (apply #'derived-mode-p linum-disabled-modes-list)
              (> (buffer-size) (* 5 1024 1024))) ;; disable linum on buffer greater than 5MB, otherwise it's unbearably slow
    (linum-mode 1)))

;;; color directories in file completion
(require 'dircolors)

(setq font-lock-maximum-decoration t
      color-theme-is-global t)

;;; Line-wrapping
(set-default 'fill-column 80)

;;; ;get rid of clutter
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;;; remove bells
(setq ring-bell-function 'ignore)

;; Highlight tabulations
(setq highlight-tabs t)

;; Show trailing white spaces
(setq show-trailing-whitespace t)

;;; add font-lock for dash.el
(eval-after-load "dash" '(dash-enable-font-lock))
