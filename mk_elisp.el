;;; mk_elisp.el --- Config for Elisp mode

;;; Commentary:

;; 

;;; Code:


;; Shorten mode name
(add-hook 'emacs-lisp-mode-hook
          (lambda()
            (setq mode-name "El")))

(provide 'mk_elisp)

;;; mk_elisp.el ends here
