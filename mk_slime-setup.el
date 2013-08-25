;; =====
;; Slime
;; =====
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
(setq inferior-lisp-program "/usr/bin/sbcl") ; Lisp system
(require 'slime)
;; (slime-setup)

(slime-setup '(slime-fancy)) ;; slime-fancy is a contributed package

(provide 'mk_slime-setup)
