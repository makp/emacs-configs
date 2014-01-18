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
			"~/config-files/general/emacs-config"
			"/home/makmiller/config-files/general/term-configs"
			"/home/makmiller/config-files/general/x-configs"
			"/home/makmiller/scripts/myscripts"
			"/home/makmiller/Documents/mydocs/websites/main-website"
			"/home/makmiller/Documents/mydocs/dossier"
			"/home/makmiller/Documents/mydocs/papers/in-progress/manuscripts/social-evol-theory"
			"~/Documents/mydocs/teaching"))

;;; With one prefix argument, magit will provide magit-repo-dirs for
;;; you to complete. If you want to create a new repository, use two
;;; prefix-args.

(provide 'mk_magit)
