;;; mk_copilot.el --- Custom config for copilot mode
;;; Code:

(require 'copilot)

(add-hook 'prog-mode-hook 'copilot-mode)


(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion-by-word)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "C-<tab>") 'copilot-next-completion) ;; copilot-previous-completion

;; I don't think this line is necessary
;; (customize-set-variable 'copilot-enable-predicates '(evil-insert-state-p))


(provide 'mk_copilot)
;;; mk_copilot.el ends here
