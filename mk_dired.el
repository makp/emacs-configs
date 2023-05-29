;;; mk_dired.el --- Custom dired config

;;; Commentary:

;; 

;;; Code:

;; (require 'dired-x)	;dired-jump seems to be defined in dired-x

(setq dired-isearch-filenames t ;; search limits to file names
      dired-dwim-target t)      ;; move files more easily with split panes

(add-hook 'dired-mode-hook
	  (lambda ()
	    (setq mode-name "Dir")
	    (setq truncate-lines 1)
	    (auto-revert-mode 1))) ;; auto-refresh dired on file change

(define-key dired-mode-map (kbd "SPC") nil) ; it was dired-next-line

(defun mk/dired-toggle-edit-from-evil()
  "Toggle edit/read-only in Dired buffers from evil."
  (interactive)
  (evil-emacs-state)
  (dired-toggle-read-only)
  (evil-force-normal-state)
  (evil-forward-word-begin))

(provide 'mk_dired)

;;; mk_dired.el ends here
