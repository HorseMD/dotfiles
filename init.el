;;; init.el --- Horse M.D.'s Emacs init file.

;;; Commentary:
;; Just your everyday init file for Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun font-exists-p (font)
  "Check if the given font exists on this system"
  (if (null (x-list-fonts font))
      nil
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq package-list '(monokai-theme rainbow-delimiters pretty-mode nyan-mode
				   haskell-mode markdown-mode flycheck
				   json-mode))

(require 'whitespace)
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
		 
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(nyan-mode)
(load-theme 'monokai t)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'haskell-mode-hook 'turn-on-pretty-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
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
