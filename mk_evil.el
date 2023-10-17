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


;; C-u behavior
;; NOTE: These lines have to load before evil is loaded
(setq evil-want-C-u-scroll t)
(setq evil-want-C-u-delete t)

(evil-mode 1)

;; tree-sitter text objects
;; function
(define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
(define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

;; block
(define-key evil-outer-text-objects-map "k" (evil-textobj-tree-sitter-get-textobj "block.outer"))
(define-key evil-inner-text-objects-map "k" (evil-textobj-tree-sitter-get-textobj "block.inner"))

;; conditional or loop
(define-key evil-outer-text-objects-map "o" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))
(define-key evil-inner-text-objects-map "o" (evil-textobj-tree-sitter-get-textobj ("conditional.inner" "loop.inner")))

;; comment
(define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj ("comment.outer")))
(define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj ("comment.inner")))


;; tree-sitter based movements
;; Goto start of next function
(define-key evil-normal-state-map
            (kbd "]f")
            (lambda ()
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj "function.outer")))

;; Goto start of previous function
(define-key evil-normal-state-map
            (kbd "[f")
            (lambda ()
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))

;; Goto end of next function
(define-key evil-normal-state-map
            (kbd "]F")
            (lambda ()
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))

;; Goto end of previous function
(define-key evil-normal-state-map
            (kbd "[F")
            (lambda ()
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj "function.outer" t t)))


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
(evil-traces-mode 1)
(evil-indent-plus-default-bindings)

(provide 'mk_evil)

;;; mk_evil.el ends here
