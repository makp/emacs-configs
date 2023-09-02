;;; mk_copilot.el --- Custom config for copilot mode
;;; Code:

(require 'copilot)

(add-hook 'prog-mode-hook 'copilot-mode)

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;; copilot-accept-completion-by-word

(provide 'mk_copilot)
;;; mk_copilot.el ends here
