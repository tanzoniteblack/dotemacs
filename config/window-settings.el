;; don't load splash screen
(setq inhibit-splash-screen t)

;;; don't load menubar
(menu-bar-mode -1)

;; replace title with buffername
(setq frame-title-format "%b")

(defvar default-font "Ubuntu Mono")
(defvar default-font-height 120)
;;; fraktur font for when we're feeling odd:
;; (setq default-font "UnifrakturMaguntia")
;; (setq default-font-height 130)

(when (eq system-type 'darwin)
  (setq default-font "Menlo")
  (setq default-font-height 125))

;; Change font to enlarged Ubuntu Mono, if it exists
(when (member default-font (font-family-list))
  (set-face-attribute 'default nil :family default-font :height default-font-height :weight 'normal)
  '(variable-pitch ((t (:family default-font :slant normal :weight regular :height default-font-height)))))

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (if (executable-find "wmctrl")
      (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen")
    (set-frame-parameter nil 'fullscreen (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

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
(global-git-gutter-mode)

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

;;; global linum-mode
(global-linum-mode)
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

;; Highlight tabulations
(setq highlight-tabs t)

;; Show trailing white spaces
(setq show-trailing-whitespace t)
