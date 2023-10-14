;;; mk_packages.el --- config for packages

;;; Commentary:

;; TODO: Consider using `use-package` library to defer loading of
;; packages until they're needed

;;; Code:

(require 'package)

;;; Package archives
(dolist (archive '(("melpa" . "https://melpa.org/packages/")
                   ("gnu" . "http://elpa.gnu.org/packages/")
                   ("nongnu" . "http://elpa.nongnu.org/nongnu/")
                   ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")))
  (add-to-list 'package-archives archive t))

;; Initialize pkgs
(package-initialize)
;; package-initialize searches for all the packages that are installed
;; and loads each package's <pkg>-autoloads.el file.


;; Refresh package contents if not already available
(unless package-archive-contents
  (package-refresh-contents))


;; Package list
(defvar mk/pkg-list '(ace-link
		      aggressive-indent
		      auctex
		      chatgpt
		      company
		      company-math
		      company-auctex
		      company-box
		      copilot
		      elpy
		      evil
		      evil-surround
		      evil-commentary
		      evil-matchit
		      evil-numbers
		      evil-collection
		      flycheck
		      helm
		      helm-bibtex
		      helm-ls-git
		      highlight-parentheses
		      hl-todo
		      ibuffer-vc
		      ivy
		      keyfreq
		      lua-mode
		      magit
		      markdown-mode
		      nov
		      org
		      org-contrib
		      org-gcal
		      org-ref
		      projectile
		      visual-fill-column
		      web-mode
		      wgrep
		      wgrep-helm
		      yaml-mode
		      yasnippet
		      zenburn-theme)
  "List of packages to install at launch.")

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (condition-case nil
		(package-install package)
	      (error (message "Eu nao pude instalar %s" package)))))
      mk/pkg-list)


(provide 'mk_packages)

;;; mk_packages.el ends here
