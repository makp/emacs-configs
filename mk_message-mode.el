;;; message mode
(defun my-message-mode-setup ()
  "My customizations for the message mode"
  (turn-on-auto-fill)
  ;; (turn-on-orgstruct++)
  (flyspell-mode))

(add-hook 'message-mode-hook 'my-message-mode-setup)

(provide 'mk_message-mode)