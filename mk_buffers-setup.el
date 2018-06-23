;;; mk_buffers-setup.el --- Custom setup for buffers

;;; Commentary:

;; 

;;; Code:

(global-set-key (kbd "C-x k") (lambda ()
				(interactive)
				(kill-buffer nil)))

;; (global-set-key (kbd "C-x c") 'bury-buffer)
(global-set-key (kbd "C-x C-n") 'bury-buffer)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Make buffer names unique
(setq-default
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":")


(provide 'mk_buffers-setup)

;;; mk_buffers-setup.el ends here