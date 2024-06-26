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
		      atomic-chrome
		      aggressive-indent
		      auctex
		      breadcrumb
		      chatgpt
		      company
		      company-math
		      company-auctex
		      company-box
		      copilot
		      elpy
		      evil
		      evil-collection
		      evil-commentary
		      evil-indent-plus
		      evil-matchit
		      evil-numbers
		      evil-surround
		      evil-textobj-tree-sitter
		      evil-traces
		      flycheck
		      helm
		      helm-bibtex
		      helm-ls-git
		      helm-projectile
		      helm-lsp
		      highlight-parentheses
		      hl-todo
		      keyfreq
		      lsp-mode
		      lsp-latex
		      ;; lsp-pyright
		      lsp-ui
		      lua-mode
		      magit
		      markdown-mode
		      nov
		      org
		      org-contrib
		      ;; org-gcal
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


(defun mk/unused-pkgs ()
  "List packages that are installed but not in `mk/pkg-list'."
  (let* ((installed-packages (cl-remove-if-not 'package--user-selected-p (mapcar 'car package-alist)))
         (used-packages mk/pkg-list)
         (unused-packages (cl-set-difference installed-packages used-packages :test 'equal)))

    ;; Print the packages in *Messages* buffer
    ;; (message "%s" (mapconcat 'symbol-name unused-packages ", "))

    unused-packages))


(defun mk/delete-unused-pkgs ()
  "Delete packages that are installed but not in `mk/pkg-list'."
  (interactive)
  (let ((unused-packages (mk/unused-pkgs)))
    (if (not unused-packages)
        (message "No unused packages.")
      (when (yes-or-no-p (format "Delete %d unused packages? (%s) "
                                 (length unused-packages)
                                 (mapconcat #'symbol-name
					    unused-packages ", ")))
        (dolist (package unused-packages)
          (when (package-installed-p package)
            (package-delete (cadr (assq package package-alist))
			    t)))))))

(provide 'mk_packages)

;;; mk_packages.el ends here
