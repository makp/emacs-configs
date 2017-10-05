(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;; check if the packages is installed; if not, install it.
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package)) 
           (package-install package))))
 '(
   ac-math
   ace-isearch
   ace-jump-mode
   aggressive-indent
   auctex 
   auto-complete
   autopair
   dired-details
   dropdown-list
   ess
   fuzzy
   gnuplot-mode
   gruvbox-theme
   helm
   helm-bibtex
   helm-ls-git
   helm-swoop
   highlight-parentheses
   ibuffer-git
   ibuffer-vc
   key-chord
   keyfreq
   magit 
   multiple-cursors
   org-gcal
   pdf-tools
   popup
   region-bindings-mode
   undo-tree
   w3m
   web-mode
   yaml-mode
   yasnippet
   zenburn-theme
   ))

(provide 'mk_packages)
;;; mk_packages ends here
