;;; init.el --- Horse M.D.'s Emacs init file.

;;; Commentary:
;; Just your everyday init file for Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun font-exists (font)
  "Check if the given font exists on this system"
  (x-list-fonts font))

(defun header-ificate (start end)
  "Insert enough -'s either side of the text inside region to make the line 80 chars long."
  (interactive "r")
  (let* ((beginning (- start (line-beginning-position)))
        (offset (/ (- (- 79 beginning) (- end start)) 2)))
    (goto-char end)
    (insert-char ?\s)
    (insert-char ?- offset)
    (goto-char start)
    (insert-char ?- offset)
    (insert-char ?\s)
    )
  (move-end-of-line 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar custom-packages '(monokai-theme rainbow-delimiters pretty-mode nyan-mode
                                        haskell-mode markdown-mode flycheck
                                        json-mode php-mode web-mode))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

(unless (file-exists-p package-user-dir)
    (package-refresh-contents))

(dolist (package custom-packages)
  (unless (package-installed-p package)
    (when (y-or-n-p (format "You are missing %s. Install it? " package))
      (package-install package))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; git clone https://github.com/slime/slime.git ~/.emacs.d/git/slime/
(when (executable-find "clisp")
  (add-to-list 'load-path "~/.emacs.d/git/slime")
  (require 'slime-autoloads)
  (setq inferior-lisp-program (executable-find "clisp"))
  (setq slime-contribs '(slime-fancy)))

(when (executable-find "gforth")
  (unless (package-installed-p 'forth)
  (load  "/usr/share/emacs/site-lisp/gforth/gforth.el")))

(nyan-mode)
(load-theme 'monokai t)
(ido-mode t)

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-pretty-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.htm?\\'" . web-mode))

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
(when (and (display-graphic-p) (font-exists "M+ 1mn Medium"))
  (set-frame-font "M+ 1mn Medium-11"))
;; no wrap
(setq-default truncate-lines t)
;; show matching parentheses
(show-paren-mode 1)
;; insert "the other delimiter" everywhere
(electric-pair-mode 1)
;; cursor settings
(global-hl-line-mode)
;; font lock
(global-font-lock-mode 1)
(setq inhibit-splash-screen t)
;; use spaces, not tabs!
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
;; re-enabled commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
