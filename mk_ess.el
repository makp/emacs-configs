;; (setq load-path (cons "/usr/share/emacs/site-lisp/ess" load-path))
;; (load "/usr/share/emacs/site-lisp/ess/ess-site")

(setq ess-eval-visibly-p nil) ;otherwise C-c C-r (eval region) takes
			      ;forever

(setq ess-ask-for-ess-directory nil) ; otherwise you are prompted each
				     ; time you start an interactive R
				     ; session
(require 'ess-eldoc)
;; to show function arguments while you are typing them

(setq ess-use-ido nil)			; to use helm instead

(provide 'mk_ess)
