(add-hook 'emacs-lisp-mode-hook
         (lambda()
           (setq mode-name "El")))

(autoload 'paredit-mode "/home/makmiller/elisp/bin/paredit/paredit-beta"
  "Minor mode for pseudo-structurally editing Lisp code." t)

(mk/add-something-to-hooks '(emacs-lisp lisp lisp-interaction scheme) 'enable-paredit-mode)

(add-hook 'paredit-mode-hook (lambda ()
			       (define-key paredit-mode-map (kbd "M-s") nil)
			       (define-key paredit-mode-map (kbd "C-S-d") 'paredit-splice-sexp)
			       (define-key paredit-mode-map (kbd "M-s o") nil)
			       (define-key paredit-mode-map (kbd "M-r") nil)))

;; FIXME: Some of the paredit commands have been overwritten by other
;; commands I've defined.

;; ---------------------
;; highlight-parentheses
;; ---------------------
(require 'highlight-parentheses)

;; Enables highlight-par in all modes
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))

(global-highlight-parentheses-mode t)

(provide 'mk_elisp)