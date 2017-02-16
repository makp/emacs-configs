;;; version control
(autoload 'magit-status "magit" nil t)

(global-set-key (kbd "C-x p") (lambda ()
				(interactive)
				(let ((current-prefix-arg '(4)))
				  (call-interactively
				   'magit-status))
				(call-interactively 'magit-pull)
				(read-key)
				(call-interactively 'helm-browse-project)))

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

;; (global-set-key (kbd "C-x C-p")
;; 		(lambda ()
;; 		  "Select a project and run helm-ls-git-ls."
;; 		  (interactive)
;; 		  (let ((current-prefix-arg '(4)))
;; 		    (call-interactively
;; 		     'magit-status))
;; 		  (call-interactively 'helm-ls-git-ls)))

;; ;;; tags
;; (defun mk/find-tags ()
;;   (interactive)
;;   (let ((current-prefix-arg '(4)))
;;     (call-interactively
;;      'magit-status))
;;   (call-interactively 'helm-etags-select))

;; (global-set-key (kbd "M-c") 'mk/find-tags)

;;; magit-pull
(global-set-key (kbd "C-x g") 'magit-pull)


(global-set-key (kbd "C-x o") 'magit-status)

(setq magit-diff-refine-hunk 'all)

;; (add-hook 'magit-log-edit-mode-hook
;; 	  (lambda ()
;; 	    (orgstruct-mode)))

(setq magit-repository-directories '("~/elisp/agendas"
				     "~/config-files/general"
				     "~/scripts/myscripts"
				     "~/Documents/mydocs"))

(provide 'mk_magit)
