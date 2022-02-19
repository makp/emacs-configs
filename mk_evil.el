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


;; C-w behavior
;; (setq-default evil-want-C-w-in-emacs-state t)
;; (require 'helm)
(define-key helm-map (kbd "C-w") 'backward-kill-word) ;it was `helm-yank-text-at-point'

;; (require 'company)
(define-key company-active-map (kbd "C-w") 'evil-delete-backward-word) ; it was `company-show-location'
(define-key company-active-map (kbd "M-w") 'company-show-location)


(evil-mode 1)


;; ESC always enters normal state
(define-key evil-emacs-state-map [escape] 'evil-normal-state)

(setq-default evil-insert-state-cursor '((bar . 3) "pale green")
	      evil-normal-state-cursor '(box "light grey")
	      evil-emacs-state-cursor '(box "PaleGoldenrod"))

(evil-set-initial-state 'ibuffer-mode 'emacs)
(evil-set-initial-state 'term-mode 'emacs)
;; evil-buffer-regexps

;; Make C-a/C-x behave like vim
(evil-define-key 'normal 'global (kbd "C-a") 'evil-numbers/inc-at-pt)
(evil-define-key 'normal 'global (kbd "C-x") 'evil-numbers/dec-at-pt)
(evil-define-key 'visual 'global (kbd "C-a") 'evil-numbers/inc-at-pt-incremental)
(evil-define-key 'visual 'global (kbd "C-x") 'evil-numbers/dec-at-pt-incremental)

;; Poor man's version of gp and gP
(evil-define-key 'normal 'global
  "gp" "p`]"
  "gP" "P`]")

;; evil plugins
(global-evil-surround-mode 1)
(evil-commentary-mode)
(global-evil-matchit-mode 1)

(provide 'mk_evil)

;;; mk_evil.el ends here
