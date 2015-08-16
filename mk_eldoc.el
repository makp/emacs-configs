;; ElDoc
(require 'eldoc)

;;; paredit and eldoc
;; (eldoc-add-command
;;  'paredit-backward-delete
;;  'paredit-close-round)
;;; whenever the listed commands are used, ElDoc will automatically
;;; refresh the minibuffer.

(mk/add-something-to-hooks '(emacs-lisp lisp-interaction ielm) 'turn-on-eldoc-mode)

(provide 'mk_eldoc)