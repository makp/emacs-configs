;;; mk_copilot.el --- Custom config for copilot mode
;;; Code:

(require 'copilot)

(add-hook 'prog-mode-hook 'copilot-mode)


(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion-by-word)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "C-<tab>") 'copilot-next-completion) ;; copilot-previous-completion

;; I don't think this line is necessary
;; Only enable copilot in insert mode
;; (customize-set-variable 'copilot-enable-predicates '(evil-insert-state-p))

;; Disable company inline previews
(with-eval-after-load 'company
  (delq 'company-preview-if-just-one-frontend company-frontends))

(provide 'mk_copilot)
;;; mk_copilot.el ends here
