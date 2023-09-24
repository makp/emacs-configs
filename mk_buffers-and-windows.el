;;; mk_buffers-and-windows.el --- Custom config for buffers and windows -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

;; Make buffer names unique by adding directory names to duplicate
;; buffer names
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":")

;; winner-mode
(winner-mode 1)
(setq winner-dont-bind-my-keys t)


(provide 'mk_buffers-and-windows)

;;; mk_buffers-and-windows.el ends here
