;;; mk_buffers-setup.el --- Custom setup for buffers

;;; Commentary:

;; 

;;; Code:

;; Make buffer names unique
(setq-default
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":")


(provide 'mk_buffers-setup)

;;; mk_buffers-setup.el ends here