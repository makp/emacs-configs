;;; mk_dired.el --- Custom dired config

;;; Commentary:

;; 

;;; Code:


(setq-default dired-isearch-filenames t ;; search limits to file names
	      dired-dwim-target t)      ;; move files more easily with split panes

(add-hook 'dired-mode-hook
	  (lambda ()
	    (setq truncate-lines 1) 	;not working (6-23-18)!
	    (setq mode-name "Dir")
	    (define-key dired-mode-map "E" 'dired-ediff-marked-files)
	    (define-key dired-mode-map "l" 'dired-up-directory)))


;; dired-x
(require 'dired-x)

(defun mk/dired-jump (&optional arg)
  "With ARG dired-jump in another window."
  (interactive "P")
  (if (consp arg)
      (dired-jump-other-window)
    (dired-jump)))

(global-set-key (kbd "C-x C-j") 'mk/dired-jump)
;; I need this line because dired-x contains a definition of C-x C-j.


;; Does this work?
(defun dired-w3m-find-file ()
  "Browse file with Emacs-w3m."
  (interactive)
  (require'w3m)
  (let ((file (dired-get-filename)))
    (if (y-or-n-p (format "Use emacs-w3m to browse %s? "
			  (file-name-nondirectory file)))
	(w3m-find-file file))))

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