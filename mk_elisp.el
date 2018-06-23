;;; mk_elisp.el --- Config for Elisp mode

;;; Commentary:

;; TODO: Bind a few built-in funcs: find-func, find-library,
;; find-function.


;;; Code:


;; Shorten mode name
(add-hook 'emacs-lisp-mode-hook
          (lambda()
            (setq mode-name "El")
	    (highlight-parentheses-mode t)
	    (setq truncate-lines 1)))



(provide 'mk_elisp)

;;; mk_elisp.el ends here
