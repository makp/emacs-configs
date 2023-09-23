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
;; (define-key evil-emacs-state-map [escape] 'evil-normal-state)

(setq evil-insert-state-cursor '((bar . 3) "pale green")
      evil-normal-state-cursor '(box "light grey")
      evil-emacs-state-cursor '(box "PaleGoldenrod"))

;; (evil-set-initial-state 'ibuffer-mode 'emacs)

;; (add-to-list 'evil-insert-state-modes 'term-mode)

;; Evil plugins
(global-evil-surround-mode 1)
(evil-commentary-mode)
(global-evil-matchit-mode 1)

(provide 'mk_evil)

;;; mk_evil.el ends here
