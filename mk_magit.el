(autoload 'magit-status "magit" nil t)

(add-hook 'magit-status-mode-hook 
	  (lambda ()
	    (define-key magit-status-mode-map (kbd "C-b") 'helm-browse-project)
	    (define-key magit-status-mode-map "t" 'magit-section-backward)))

(global-set-key (kbd "C-x p")
		'(lambda ()
		   (interactive)
		   (let ((current-prefix-arg '(4)))
		     (call-interactively 'magit-status))
		   (call-interactively 'magit-pull)))

(defun mk/browse-project (&optional arg)
  "Function for browsing projects. If prefix is non-nil, provide
a list of all projects before running helm-ls-git-ls."
  (interactive "P")
  (when (consp arg) 		       
    (let ((current-prefix-arg '(4)))
      (call-interactively
       'magit-status)))
  (call-interactively 'helm-browse-project)) ;helm-ls-git-ls

(global-set-key (kbd "C-x b") 'mk/browse-project)

(global-set-key (kbd "C-x t")
		(lambda ()
		  "Select a project and run helm-find-files."
		  (interactive)
		  (let ((current-prefix-arg '(4)))
		    (call-interactively
		     'magit-status))
		  (call-interactively 'helm-find-files)))

(global-set-key (kbd "M-g") 'magit-status)
(global-set-key (kbd "C-x g") 'magit-pull)
(global-set-key (kbd "C-x d") 'mk/show-diffs)

(defun mk/show-diffs (&optional arg) 
  (interactive "P")
  (when (consp arg)
    (magit-log-buffer-file))
  (magit-diff-buffer-file))

(setq magit-diff-refine-hunk 'all)

;; (add-hook 'magit-log-edit-mode-hook
;; 	  (lambda ()
;; 	    (orgstruct-mode)))

(setq magit-repository-directories '("~/elisp/agendas"
				     "~/config-files/general"
				     "~/scripts/myscripts"
				     "~/Documents/mydocs"))

(provide 'mk_magit)
