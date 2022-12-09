;;; mk_dired.el --- Custom dired config

;;; Commentary:

;; 

;;; Code:

;; (require 'dired-x)	;dired-jump seems to be defined in dired-x

(setq-default dired-isearch-filenames t ;; search limits to file names
	      dired-dwim-target t)      ;; move files more easily with split panes

(add-hook 'dired-mode-hook
	  (lambda ()
	    (setq mode-name "Dir")
	    (setq truncate-lines 1)
	    (auto-revert-mode 1))) ;; auto-refresh dired on file change

(defun mk/dired-toggle-edit-from-evil()
  "Toggle edit/read-only in Dired buffers from evil."
  (interactive)
  (evil-emacs-state)
  (dired-toggle-read-only)
  (evil-exit-emacs-state))

;; (defun dired-w3m-find-file ()
;;   "Browse file with Emacs-w3m."
;;   (interactive)
;;   (require'w3m)
;;   (let ((file (dired-get-filename)))
;;     (if (y-or-n-p (format "Use emacs-w3m to browse %s? "
;; 			  (file-name-nondirectory file)))
;; 	(w3m-find-file file))))

(provide 'mk_dired)

;;; mk_dired.el ends here
