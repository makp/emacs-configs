;;; version control
(autoload 'magit-status "magit" nil t)
(global-set-key (kbd "C-x g") 'magit-status)

(add-hook 'magit-mode-hook
	  (lambda ()
	    (define-key magit-status-mode-map (kbd "o") 'helm-ls-git-ls)))
;;; "o" was git submodule

(add-hook 'magit-log-edit-mode-hook
	  (lambda ()
	    (orgstruct-mode)))

(setq magit-repo-dirs '("~/elisp/agenda"
			"~/config-files/general"
			"~/scripts/myscripts"
			"~/Documents/mydocs/websites/main-website"
			"~/Documents/mydocs/dossier"
			"~/Documents/mydocs/papers"
			"~/Documents/mydocs/papers"
			"~/Documents/mydocs/teaching"
			"~/Documents/mydocs/tech-notes"))

;;; With one prefix argument, magit will provide magit-repo-dirs for
;;; you to complete. If you want to create a new repository, use two
;;; prefix-args.

(provide 'mk_magit)
