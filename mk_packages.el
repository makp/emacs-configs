;;; mk_packages.el --- config for packages

;;; Commentary:

;; 

;;; Code:

(require 'package)

;;; Load installed packages


;; (setq package-enable-at-startup nil)
;; Disable automatic package loading at launch

;; package-load-list

(package-initialize)
;; package-initialize searches for all the packages that are installed
;; and loads each package's <pkg>-autoloads.el file.


;;; Package archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)


;;; Install missing packages

(defvar mk/pkg-list '(ac-math ace-jump-mode aggressive-indent
			      auctex company company-auctex
			      company-math dired-details
			      dropdown-list elpy flycheck fuzzy
			      gnuplot-mode gruvbox-theme helm
			      helm-bibtex helm-ls-git
			      highlight-parentheses ibuffer-vc
			      key-chord keyfreq magit
			      markdown-mode multiple-cursors nov
			      org-plus-contrib org-gcal pdf-tools
			      popup region-bindings-mode
			      undo-tree w3m web-mode wgrep
			      wgrep-helm yaml-mode yasnippet
			      zenburn-theme)
  "The list of packages installed at launch.")

;; ess, ibuffer-git

;; Fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

(setq package-selected-packages mk/pkg-list)
(package-install-selected-packages)


(provide 'mk_packages)

;;; mk_packages.el ends here
