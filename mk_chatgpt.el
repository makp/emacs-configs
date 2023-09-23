;; (message "Carregando mk_chatgpt")

;; (evil-set-initial-state 'chatgpt-mode 'emacs)
(setq openai-key (getenv "OPENAI_KEY"))

(defun mk/chatgpt ()
  "Select GPT model to use."
  (interactive)
  (let ((gpt-version (completing-read "Que ce quer?: " '("gpt-4" "gpt-3.5-turbo"))))
    (setq chatgpt-model gpt-version))
  (chatgpt))


(provide 'mk_chatgpt)
;;; mk_private.el ends here
