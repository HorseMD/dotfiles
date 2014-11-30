;; Assumes that a copy of org-mode is in ~/.emacs.d/org-mode/.

(add-to-list 'load-path "~/.emacs.d/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/org-mode/contrib/lisp")

(package-initialize)
(setq package-enable-at-startup nil)

(require 'org)
(require 'ob-tangle)

(org-babel-load-file (expand-file-name "~/.emacs.d/daniel.org"))
