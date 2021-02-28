;;; mk_evil.el --- Custom Evil config -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(setq-default evil-respect-visual-line-mode t)
(setq-default evil-undo-system 'undo-redo)
(setq-default evil-search-module 'evil-search)

;; Make C-u behave more like vim
(setq-default evil-want-C-u-scroll t)
(setq-default evil-want-C-u-delete t)
;; (evil-define-key '(normal visual) 'global (kbd "C-u") 'evil-scroll-up)
;; (evil-define-key 'insert 'global (kbd "C-u") 'evil-delete-back-to-indentation)

;; C-w behavior in Emacs states
;; (setq-default evil-want-C-w-in-emacs-state t)
(with-eval-after-load 'helm
  (defvar helm-map)
  (define-key helm-map (kbd "C-w") 'backward-kill-word)) ;it was `helm-yank-text-at-point'

(evil-mode 1)

;; evil plugins
(global-evil-surround-mode 1)
(evil-commentary-mode)

(provide 'mk_evil)

;;; mk_evil.el ends here