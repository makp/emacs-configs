;;; mk_magit.el --- Magit config

;;; Commentary:

;; 

;;; Code:


;; main global key bindings
(defun mk/fetch(&optional arg)
  (interactive "P")
  (call-interactively 'magit-status)
  (call-interactively 'magit-fetch)
  (magit-section-show-level-3-all))

(global-set-key (kbd "C-x p") 'mk/fetch)
(global-set-key (kbd "C-x d") 'magit-diff-buffer-file)

(global-set-key (kbd "C-x g") 'magit-status)


(setq-default magit-repository-directories
	      '(("~/elisp/agendas" . 0)
		("~/config-files/general" . 1)
		("~/scripts/myscripts" . 0)
		("~/Documents/mydocs" . 3)))

;; ============
;; magit status
;; ============
(add-hook 'magit-status-mode-hook
	  (lambda ()
	    (define-key magit-status-mode-map "t" 'magit-section-backward)))

(add-hook 'magit-status-mode-hook
          (lambda ()
	    (setq truncate-lines t)))

(setq magit-diff-refine-hunk 'all)

;; ==============
;; helm and magit
;; ==============

(require 'helm-ls-git)
(setq helm-ls-git-status-command 'magit-status-internal) ; use magit

(defun mk/browse-project (&optional arg)
  "Browse git repo. If ARG is non-nil, provide a list of all repos before running helm-browse-project."
  (interactive "P")
  (when (consp arg)
    ;; (let ((current-prefix-arg '(4))))
    (call-interactively 'magit-status))
  (call-interactively 'helm-browse-project)) ;helm-ls-git-ls

(global-set-key (kbd "C-x DEL") 'mk/browse-project)

(global-set-key (kbd "C-x t")
		(lambda ()
		  "Select a project and run helm-find-files."
		  (interactive)
		  (let ((current-prefix-arg '(4)))
		    (call-interactively
		     'magit-status))
		  (call-interactively 'helm-find-files)))

(define-key helm-ls-git-map (kbd "M-s g") 'helm-ls-git-run-grep)
(define-key helm-ls-git-buffer-map (kbd "M-s g") 'helm-ls-git-run-grep)

(defun mk/grep-project (&optional arg)
  "Run git-grep the whole repository. If ARG is non-nil, ask for a git repo first."
  (interactive "P")
  (when (consp arg)
    (call-interactively 'magit-status))
  (let ((current-prefix-arg '(4)))
    (call-interactively 'helm-grep-do-git-grep)))

(global-set-key (kbd "M-s g") 'mk/grep-project)

(define-key helm-find-files-map (kbd "M-s g") 'helm-ff-run-git-grep)

(provide 'mk_magit)

;;; mk_magit.el ends here
