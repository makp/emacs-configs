;;; mk_dired.el --- Custom dired config

;;; Commentary:

;; 

;;; Code:


(setq-default dired-isearch-filenames t ;; search limits to file names
	      dired-dwim-target t)      ;; move files more easily with split panes

(add-hook 'dired-mode-hook
	  (lambda ()
	    (setq mode-name "Dir")
	    (setq truncate-lines 1)
	    ))

;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; (require 'dired-x)	;dired-jump seems to be defined in dired-x

;; (defun dired-w3m-find-file ()
;;   "Browse file with Emacs-w3m."
;;   (interactive)
;;   (require'w3m)
;;   (let ((file (dired-get-filename)))
;;     (if (y-or-n-p (format "Use emacs-w3m to browse %s? "
;; 			  (file-name-nondirectory file)))
;; 	(w3m-find-file file))))

;; show directories first
;; (defun mydired-sort ()
;;   "Sort dired listings with directories first."
;;   (save-excursion
;;     (let (buffer-read-only)
;;       (forward-line 2) ;; beyond dir. header 
;;       (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
;;     (set-buffer-modified-p nil)))

;; (defadvice dired-readin
;;     (after dired-after-updating-hook first () activate)
;;   "Sort dired listings with directories first before adding marks."
;;   (mydired-sort))


(provide 'mk_dired)

;;; mk_dired.el ends here