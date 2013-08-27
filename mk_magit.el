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

(setq magit-repo-dirs '("/home/makmiller/config-files/general/emacs-config"
			"/home/makmiller/Documents/mydocs/papers/in-progress/reviews-and-comments/comment_fagan-chapter"
			"/home/makmiller/Documents/mydocs/papers/in-progress/manuscripts/how-bacteria-socialize"))
;;; With one prefix argument, magit will provide magit-repo-dirs for
;;; you to complete. If you want to create a new repository, use two
;;; prefix-args.

(provide 'mk_magit)
