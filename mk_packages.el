;;; mk_packages.el --- config for packages

;;; Commentary:

;; 

;;; Code:

(require 'package)

;; (setq package-enable-at-startup nil)
;; Disable automatic package loading at launch

;; package-load-list

;;; Package archives
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)


(package-initialize)
;; package-initialize searches for all the packages that are installed
;; and loads each package's <pkg>-autoloads.el file.


;; Fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))


;; Scans the list in mk/pkg-list. If the package listed is not already installed, install it
(defvar mk/pkg-list '(ace-link
		      avy
		      aggressive-indent
		      auctex
		      company
		      company-math
		      company-auctex
		      define-word
		      elpy
		      evil
		      flycheck
		      helm
		      helm-bibtex
		      helm-ls-git
		      highlight-parentheses
		      hl-todo
		      ibuffer-vc
		      ibuffer-git
		      keyfreq
		      lua-mode
		      magit
		      markdown-mode
		      nov
		      org
		      org-plus-contrib
		      org-gcal
		      org-ref
		      pdf-tools
		      synosaurus
		      visual-fill-column
		      web-mode
		      wgrep
		      wgrep-helm
		      yaml-mode
		      yasnippet
		      wolfram-mode
		      zenburn-theme)
  "The list of packages installed at launch.")

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      mk/pkg-list)

;; Older code:
;; (setq package-selected-packages mk/pkg-list)
;; (package-install-selected-packages)


(provide 'mk_packages)

;;; mk_packages.el ends here
