;;; mk_buffers-and-windows.el --- Custom config for buffers and windows -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

;; Make buffer names unique
(setq-default
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":")

;; winner-mode
(setq-default winner-dont-bind-my-keys t)
(winner-mode 1)


(provide 'mk_buffers-and-windows)

;;; mk_buffers-and-windows.el ends here