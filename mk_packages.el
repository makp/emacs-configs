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
 '(auctex magit helm helm-ls-git ace-jump-mode auto-complete autopair gnuplot-mode yasnippet undo-tree multiple-cursors keyfreq highlight-parentheses region-bindings-mode dropdown-list zenburn-theme ac-math popup emms w3m ess key-chord ibuffer-git web-mode yaml-mode))

(provide 'mk_packages)
;;; mk_packages ends here