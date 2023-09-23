;;; mk_evil.el --- Custom Evil config -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

;; Evil settings
;; enable respect for visual line mode in evil
(setq evil-respect-visual-line-mode t)

;; set evil undo system to use 'undo-redo
(setq evil-undo-system 'undo-redo)

;; set evil search module to 'evil-search
(setq evil-search-module 'evil-search)

(evil-mode 1)


;; ESC always enters normal state
(define-key evil-emacs-state-map [escape] 'evil-normal-state)

(setq evil-insert-state-cursor '((bar . 3) "pale green")
      evil-normal-state-cursor '(box "light grey")
      evil-emacs-state-cursor '(box "PaleGoldenrod"))

(evil-set-initial-state 'ibuffer-mode 'emacs)

(add-to-list 'evil-insert-state-modes 'term-mode)

;; Evil plugins
(global-evil-surround-mode 1)
(evil-commentary-mode)
(global-evil-matchit-mode 1)


;; Evil collection
(evil-collection-init)
;; (evil-collection-init '(dired ibuffer))


;; ;; C-w behavior
;; (define-key helm-map (kbd "C-w") 'backward-kill-word) ;it was `helm-yank-text-at-point'
;; ;; (setq evil-want-C-w-in-emacs-state t)
;; (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
;; ;; it was `company-show-location'
;; (define-key company-active-map (kbd "M-w") 'company-show-location)

;; ;; Vim-like behavior
;; ;; enable vim-like C-u scroll in Evil
;; (setq evil-want-C-u-scroll t)
;; ;; enable vim-like C-u delete in Evil
;; (setq evil-want-C-u-delete t)

;; ;; Make C-a/C-x behave like vim
;; (evil-define-key 'normal 'global (kbd "C-a") 'evil-numbers/inc-at-pt)
;; (evil-define-key 'normal 'global (kbd "C-x") 'evil-numbers/dec-at-pt)
;; (evil-define-key 'visual 'global (kbd "C-a") 'evil-numbers/inc-at-pt-incremental)
;; (evil-define-key 'visual 'global (kbd "C-x") 'evil-numbers/dec-at-pt-incremental)

;; ;; Poor man's version of gp and gP
;; (evil-define-key 'normal 'global
;;   "gp" "p`]"
;;   "gP" "P`]")


(provide 'mk_evil)

;;; mk_evil.el ends here
