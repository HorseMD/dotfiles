;;; init.el --- Horse M.D.'s Emacs init file.

;;; Commentary:
;; Just your everyday init file for Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun font-exists (font)
  "Check if the given font exists on this system"
  (x-list-fonts font))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar package-list '(monokai-theme rainbow-delimiters pretty-mode nyan-mode
				     haskell-mode markdown-mode flycheck
				     json-mode php-mode))

(require 'whitespace)
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")))
(package-initialize)

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; git clone https://github.com/slime/slime.git ~/.emacs.d/git/slime/
(when (executable-find "clisp")
  (add-to-list 'load-path "~/.emacs.d/git/slime")
  (require 'slime-autoloads)
  (setq inferior-lisp-program (executable-find "clisp"))
  (setq slime-contribs '(slime-fancy)))

(nyan-mode)
(load-theme 'monokai t)

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-pretty-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c C-l") 'global-linum-mode)
(global-set-key (kbd "C-c C-n") 'nyan-mode)
(global-set-key (kbd "C-c C-p") 'pretty-mode)
(global-set-key (kbd "C-c C-w") 'whitespace-mode)
(global-set-key (kbd "C-c C-d") 'delete-trailing-whitespace)

(global-unset-key (kbd "C-z"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; no scrollbars, toolbars or menubars
(dolist (mode '(menu-bar-mode scroll-bar-mode tool-bar-mode))
  (when (fboundp mode) (funcall mode -1)))
;; set the font to something a little nicer when using graphical system
(when (and (display-graphic-p) (font-exists "M+ 1mn Medium")))
(set-frame-font "M+ 1mn Medium-11")
;; no wrap
(setq-default truncate-lines t)
;; show matching parentheses
(show-paren-mode 1)
;; cursor settings
(global-hl-line-mode)
;; font lock
(global-font-lock-mode 1)
(setq inhibit-splash-screen t)
;; re-enabled commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
