;;; mk_dired.el --- Custom dired config

;;; Commentary:

;; 

;;; Code:

(setq dired-isearch-filenames t ;; search limits to file names
      dired-dwim-target t)      ;; move files more easily with split panes

(add-hook 'dired-mode-hook
	  (lambda ()
	    (setq mode-name "Dir")
	    (setq truncate-lines 1)
	    (auto-revert-mode 1))) ;; auto-refresh dired on file change


(provide 'mk_dired)

;;; mk_dired.el ends here
