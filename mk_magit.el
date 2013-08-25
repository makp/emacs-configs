;;; version control
(autoload 'magit-status "magit" nil t)

(global-set-key (kbd "C-x g") 'magit-status)

(add-hook 'magit-log-edit-mode-hook
	  (lambda ()
	    (orgstruct-mode)))

(setq magit-repo-dirs '("/home/makmiller/config-files/general/emacs-config/"
			"/home/makmiller/Documents/mypapers/evolution/in-progress/relational-essentialism/"))
;;; With a prefix argument, magit will provide magit-repo-dirs for you
;;; to complete. If you want to create a new repository, type C-u twice.
;; FIXME: As of Aug 24, this wasn't working.

(provide 'mk_magit)
