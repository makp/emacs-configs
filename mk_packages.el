;;; mk_packages.el --- config for packages

;;; Commentary:

;; 

;;; Code:


(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)

;; check if the packages is installed; if not, install it.
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package))))
 '(ac-math
   ace-jump-mode
   aggressive-indent
   auctex
   auto-complete
   autopair
   dired-details
   dropdown-list
   ;; ess
   elpy
   flycheck
   fuzzy
   gnuplot-mode
   gruvbox-theme
   helm
   helm-bibtex
   helm-ls-git
   highlight-parentheses
   ibuffer-git
   ibuffer-vc
   key-chord
   keyfreq
   magit
   markdown-mode
   multiple-cursors
   nov
   org-plus-contrib
   org-gcal
   pdf-tools
   popup
   region-bindings-mode
   undo-tree
   w3m
   web-mode
   wgrep
   wgrep-helm
   yaml-mode
   yasnippet
   zenburn-theme))

(package-initialize)

(provide 'mk_packages)

;;; mk_packages.el ends here
