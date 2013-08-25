(add-hook 'dired-mode-hook
         (lambda()
           (setq mode-name "Dir")
	   (dired-omit-mode 1)))

(setq dired-isearch-filenames t ;; search limits to file names
      dired-dwim-target t)      ;; move files more easily with split panes


(add-hook 'dired-mode-hook
	  '(lambda ()
	      (define-key dired-mode-map "E" 'dired-ediff-marked-files)
	      (load "dired-x")))


(defun mk/dired-jump (&optional arg)
  "With prefix arg dired-jump in another window."
  (interactive "P")
  (if (consp arg)
      (progn (switch-to-buffer-other-window (current-buffer))
	     (dired-jump))
    (dired-jump)))

(global-set-key (kbd "C-x C-j") 'mk/dired-jump)

;;;###autoload
(defun dired-w3m-find-file ()
  (interactive)
  (require'w3m)
  (let ((file (dired-get-filename)))
    (if (y-or-n-p (format "Use emacs-w3m to browse %s? "
			  (file-name-nondirectory file)))
	(w3m-find-file file))))

(provide 'mk_dired)