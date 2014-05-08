;; don't load splash screen
(setq inhibit-splash-screen t)

;;; don't load menubar
(menu-bar-mode -1)

;; replace title with buffername
(setq frame-title-format "%b")

(defvar default-font "Ubuntu Mono")
(defvar default-font-height 120)

(when (eq system-type 'darwin)
  (setq default-font "Menlo")
  (setq default-font-height 125))
;; (setq default-font "Meslo LG S")
;; (setq default-font "Consolas")

;; Change font to enlarged Ubuntu Mono, if it exists
(when (member default-font (font-family-list))
  (set-face-attribute 'default nil :family default-font :height default-font-height :weight 'normal)
  '(variable-pitch ((t (:family default-font :slant normal :weight regular :height default-font-height)))))

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(setq display-time-day-and-date nil)
(setq display-time-24hr-format t)
(display-time)

;;; always reuse frames when calling display-buffer
(setq-default display-buffer-reuse-frames t)

;;; git-gutter-fringe-mode
(require 'git-gutter-fringe)
(setq git-gutter-fr:side 'right-fringe)
(setq-default left-fringe-width 2)
(setq-default right-fringe-width 12)
(global-git-gutter-mode t)

;;; powerline
(require 'powerline)
(powerline-default-theme)

;;; color theme
(require 'moe-theme)
(moe-dark)
;; (moe-light)

;;; powerline color
(setq moe-theme-mode-line-color 'purple)
(powerline-moe-theme)

;;; expression highlight
(setq show-paren-style 'expression)

;; load linum for all programming modes
(add-hook 'prog-mode-hook 'linum-mode)
;; and for python, since it doesn't inherit from prog-mode
(add-hook 'python-mode-hook 'linum-mode)
;; and just because I like it, text-mode as well
(add-hook 'text-mode-hook 'linum-mode)
;; modify linum space
(setq linum-format "%2d ")

;;; color directories in file completion
(require 'dircolors)

;;make sure ansi colour character escapes are honoured
(require 'ansi-color)
(ansi-color-for-comint-mode-on)

(setq font-lock-maximum-decoration t
      color-theme-is-global t)

;;; Line-wrapping
(set-default 'fill-column 80)

;;; ;get rid of clutter
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;; remove bells
(setq ring-bell-function 'ignore)
