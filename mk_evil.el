;;; mk_evil.el --- Custom Evil config -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(setq-default evil-respect-visual-line-mode t)
(setq-default evil-undo-system 'undo-redo)
(setq-default evil-search-module 'evil-search)
(evil-mode 1)
(global-evil-surround-mode 1)


(provide 'mk_evil)

;;; mk_evil.el ends here