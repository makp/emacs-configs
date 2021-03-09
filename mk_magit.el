;;; mk_magit.el --- Magit config

;;; Commentary:

;; 

;;; Code:


(setq-default magit-repository-directories
	      '(("~/elisp/agendas" . 0)
		("~/config-files/general" . 1)
		("~/scripts/myscripts" . 0)
		("~/Documents/mydocs" . 3)))

;; magit status buffer
(add-hook 'magit-status-mode-hook
	  (lambda ()
	    (setq truncate-lines nil)))

(setq-default magit-diff-refine-hunk 'all)

;; git fetch
(defun mk/magit-fetch (&optional arg)
  "Open magit status before running `magit-fetch'.
Select a git repo if ARG is non-nil."
  (interactive "p")
  (call-interactively 'magit-status)
  (call-interactively 'magit-fetch))

;; projects

(require 'helm-ls-git)
(setq-default helm-ls-git-status-command 'magit-status-internal) ; use magit

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

(defun mk/select-project-and-fetch-from-all-remotes(&optional arg)
  "Fetch git repo. If ARG is non-nil, provide a list of all repos before fetching git repo."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'magit-status))  ;; (magit-section-show-level-3-all)
  (call-interactively 'magit-fetch-all))


(provide 'mk_magit)

;;; mk_magit.el ends here
