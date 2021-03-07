;;; mk_magit.el --- Magit config

;;; Commentary:

;; 

;;; Code:


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

(defun mk/select-project-and-browse-buffers ()
  "Browse buffers recursively after selecting a git project."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'magit-status)
    (call-interactively 'helm-browse-project)))


(defun mk/select-project-and-find-files ()
  "Run helm-find-files after selecting a git project."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'magit-status))
  (call-interactively 'helm-find-files))


(defun mk/select-project-and-run-git-grep ()
  "Select git repo and run git-grep on the whole repository."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'magit-status)
    (call-interactively 'helm-grep-do-git-grep)))

(provide 'mk_magit)

;;; mk_magit.el ends here
