;;; init.el --- Horse M.D.'s Emacs init file.

;;; Commentary:
;; Just your everyday init file for Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun install-unless-exist (package)
  "Installs the given package, unless it's already installed."
  (unless (package-installed-p package)
    (package-install package)))

(defun setup-package (package)
  "Install (if necessary) and require the given package."
  (install-unless-exist package)
  (require package))

(defun font-exists-p (font)
  "Check if the given font exists on this system"
  (if (null (x-list-fonts font))
      nil
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'whitespace)

;; load the package manager
(require 'package)
(add-to-list 'package-archives 
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; vim-like text editing with evil
;(setup-package 'evil)
;(evil-mode t)

;; monokai theme
(setup-package 'monokai-theme)
(load-theme 'monokai t)

;; colorize code delimiters
(setup-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; elfeed feed reader
(setup-package 'elfeed)
(global-set-key (kbd "C-x w") 'elfeed)
(setq elfeed-feeds
      '("http://reddit.com/r/emacs/.rss"
	"http://xkcd.com/rss.xml"))

;; pretty-mode for displaying parts of the buffer as pretty symbols
(setup-package 'pretty-mode)
(global-pretty-mode t)

(setup-package 'nyan-mode)
(nyan-mode)

(setup-package 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; flycheck syntax checking
(setup-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c C-w") 'whitespace-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; line numbers!
(global-linum-mode t)
;; no scrollbars, toolbars or menubars
(dolist (mode '(menu-bar-mode scroll-bar-mode tool-bar-mode))
  (when (fboundp mode) (funcall mode -1)))
;; set the font to something a little nicer ^^
(if (font-exists-p "M+ 1mn Medium")
    (set-frame-font "M+ 1mn Medium-11"))
;; no wrap
(setq-default truncate-lines t)
;; show matching parentheses
(show-paren-mode 1)
;; cursor settings
(global-hl-line-mode)
;; font lock
(global-font-lock-mode 1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("bd115791a5ac6058164193164fd1245ac9dc97207783eae036f0bfc9ad9670e0" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
