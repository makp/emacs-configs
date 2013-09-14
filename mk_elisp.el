;; =========
;; mode name
;; =========
(add-hook 'emacs-lisp-mode-hook
         (lambda()
           (setq mode-name "El")))


;; =======
;; paredit
;; =======
(autoload 'paredit-mode "~/elisp/bin/paredit/paredit-beta"
  "Minor mode for pseudo-structurally editing Lisp code." t)

(mk/add-something-to-hooks '(emacs-lisp lisp) 'paredit-mode)

;;; paredit customizations
(eval-after-load 'paredit '(progn
     (define-key paredit-mode-map (kbd "M-s") nil)
     (define-key paredit-mode-map (kbd "C-S-d") 'paredit-splice-sexp)
     (define-key paredit-mode-map (kbd "M-s o") nil)
     (define-key paredit-mode-map (kbd "M-r") nil)))

;; FIXME: Some of the paredit commands have been overwritten by other
;; commands I've defined.

;; =====================
;; highlight parenthesis
;; =====================
(require 'highlight-parentheses)
(highlight-parentheses-mode t)

(provide 'mk_elisp)