;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load the package manager
(require 'package)

;; add the MELPA repository to package manager
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; load packages
(package-initialize)

;; init evil
;;(require 'evil)
;;(evil-mode t)

;; set the theme to moe-theme
(require 'moe-theme)
;; set the theme to the dark variant
(moe-dark)

;; powerline
(require 'powerline)
(powerline-moe-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; line numbers!
(global-linum-mode t)
;; hide the toolbar
(tool-bar-mode -1)
;; set the font to something a little nicer ^^
(set-default-font "M+ 1mn Medium-11")
;; no wrap
(setq-default truncate-lines t)
;; show matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)
;;(setq show-paren-style 'expression)

