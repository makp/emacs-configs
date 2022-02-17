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
	     '("gnu" . "http://elpa.gnu.org/packages/") t)

(add-to-list 'package-archives
	     '("nongnu" . "http://elpa.nongnu.org/nongnu/") t) ; for org-contrib


(package-initialize)
;; package-initialize searches for all the packages that are installed
;; and loads each package's <pkg>-autoloads.el file.


;; Fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))


;; Scans the list in mk/pkg-list. If the package listed is not already installed, install it
(defvar mk/pkg-list '(ace-link
		      aggressive-indent
		      auctex
		      cider
		      company
		      company-math
		      company-auctex
		      define-word
		      elpy
		      evil
		      evil-surround
		      evil-commentary
		      evil-matchit
		      evil-numbers
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
		      org-contrib
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
