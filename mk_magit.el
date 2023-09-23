;;; mk_magit.el --- Magit config

;;; Commentary:

;; 

;;; Code:


(setq magit-repository-directories
      '(("~/elisp/agendas" . 0)
	("~/config-files/general" . 1)
	("~/scripts/myscripts" . 0)
	("~/Documents/mydocs" . 3)))


;; Enable line wrapping in magit status buffer
(add-hook 'magit-status-mode-hook
	  (lambda ()
	    (setq truncate-lines nil)))


;; ===============
;; List git status
;; ===============

;; Check the status of opened git repos
(defun get-git-repo-dirs ()
  "Return a list of directories of all unique Git repositories that have open buffers."
  (let ((git-repo-dirs '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name)
                   (magit-toplevel)
                   (not (member (magit-toplevel) git-repo-dirs)))
          (push (magit-toplevel) git-repo-dirs))))
    git-repo-dirs))


(defun check-status-of-folder (folder)
  "Check the git status of a folder."
  (with-current-buffer (get-buffer-create "*magit-status*")
    (magit-status-setup-buffer folder)
    (magit-refresh)))

;; (defun check-git-status ()
;;   "Check the status of all Git repositories with open buffers."
;;   (dolist (dir (get-git-repo-dirs))
;;     (let ((output (shell-command-to-string (concat "cd " dir " && git status"))))
;;       (message "Git status for %s:\n%s" dir output))))


(defun mk/list-git-status-of-open-buffers ()
  "Create a temporary org-mode buffer with git status for each Git repository that has open buffers."
  (interactive)
  (with-current-buffer (get-buffer-create "*Git Status*")
    (org-mode)
    (erase-buffer)
    (dolist (dir (get-git-repo-dirs))
      (insert (format "* [[elisp:(check-status-of-folder \"%s\")][Jump to git status: \"%s\"]]\n" dir dir))
      (insert "#+begin_src sh :results output\n")
      (insert (format "  cd %s\n" dir))
      (insert "  git status --porcelain --branch\n")
      (insert "#+end_src\n\n"))
    (org-babel-execute-buffer)
    (org-hide-block-all)
    (setq org-link-elisp-confirm-function nil))
  (switch-to-buffer "*Git Status*")
  (goto-char 3))


;; Refine all hunks during diff
(setq magit-diff-refine-hunk 'all)


;; =============================
;; Custom funcs related to magit
;; =============================

(defun mk/magit-fetch (&optional arg)
  "Open magit status before running `magit-fetch'.
Select a git repo if ARG is non-nil."
  (interactive "p")
  (call-interactively 'magit-status)
  (call-interactively 'magit-fetch))


(defun mk/select-project-and-browse-buffers ()
  "Browse buffers recursively after selecting a git project."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'magit-status)
    (call-interactively 'helm-browse-project))
  (delete-other-windows))


(defun mk/select-project-and-find-files ()
  "Run helm-find-files after selecting a git project."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'magit-status))
  (call-interactively 'helm-find-files)
  (delete-other-windows))


(defun mk/select-project-and-run-git-grep ()
  "Select git repo and run git-grep on the whole repository."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'magit-status)
    (call-interactively 'helm-grep-do-git-grep))
  (delete-other-windows))


(defun mk/select-project-and-fetch-from-all-remotes(&optional arg)
  "Fetch git repo. If ARG is non-nil, provide a list of all repos before fetching git repo."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'magit-status))  ;; (magit-section-show-level-3-all)
  (call-interactively 'magit-fetch-all)
  (delete-other-windows))


(defun mk/select-project-and-display-status ()
  "Select git repo and display status."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'magit-status))
  (delete-other-windows))

;; ==================
;; helm-ls-git config
;; ==================
(require 'helm-ls-git)
(setq helm-ls-git-status-command 'magit-status-internal) ; use magit


(provide 'mk_magit)

;;; mk_magit.el ends here
