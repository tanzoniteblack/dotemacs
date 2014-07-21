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
  (setq default-font "Menlo"
        default-font-height 125))

;; Change font to enlarged Ubuntu Mono, if it exists
(when (member default-font (font-family-list))
  (set-face-attribute 'default nil :family default-font :height default-font-height :weight 'normal)
  '(variable-pitch ((t (:family default-font :slant normal :weight regular :height default-font-height)))))

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
;;; modify linum space
(setq linum-format "%2d ")
;;; disable linum for certain modes
(setq linum-disabled-modes-list '(eshell-mode wl-summary-mode compilation-mode org-mode dired-mode doc-view-mode image-mode cider-mode shell-mode cider-inspector-mode magit-status-mode special-mode))

(defun linum-on ()
  "* When linum is running globally, disable line number in modes defined in `linum-disabled-modes-list'. Changed by linum-off."
  (unless (or (minibufferp)
              (apply 'derived-mode-p linum-disabled-modes-list)
              (> (buffer-size) (* 5 1024 1024))) ;; disable linum on buffer greater than 5MB, otherwise it's unbearably slow
    (linum-mode 1)))

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
