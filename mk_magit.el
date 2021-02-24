;;; mk_magit.el --- Magit config

;;; Commentary:

;; 

;;; Code:


;; main global key bindings
(defun mk/fetch(&optional arg)
  "Fetch git repo. If ARG is non-nil, provide a list of all repos before fetching git repo."
  (interactive "P")
  (call-interactively 'magit-status)
  (call-interactively 'magit-fetch)
  (magit-section-show-level-3-all))

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
	    (setq truncate-lines nil)))

(setq magit-diff-refine-hunk 'all)

;; ==============
;; helm and magit
;; ==============

(require 'helm-ls-git)
(setq helm-ls-git-status-command 'magit-status-internal) ; use magit

(defun mk/browse-project-buffers (&optional arg)
  "Browse git repo. If ARG is non-nil, provide a list of all repos before running helm-browse-project."
  (interactive "P")
  (when arg
    (call-interactively 'magit-status))
  (let ((current-prefix-arg nil))
    (call-interactively 'helm-browse-project)))


(defun mk/select-project-and-find-files ()
  "Select a project and run helm-find-files."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'magit-status))
  (call-interactively 'helm-find-files))


(defun mk/grep-project (&optional arg)
  "Run git-grep the whole repository. If ARG is non-nil, ask for a git repo first."
  (interactive "P")
  (when arg
    (call-interactively 'magit-status))
  (let ((current-prefix-arg '(4)))
    (call-interactively 'helm-grep-do-git-grep)))

(provide 'mk_magit)

;;; mk_magit.el ends here
