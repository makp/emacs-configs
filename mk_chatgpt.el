(message "Carregando mk_chatgpt")

(evil-set-initial-state 'chatgpt-mode 'emacs)

(setq chatgpt-model "gpt-4" ;; "gpt-3.5-turbo"
      openai-key (getenv "OPENAI_KEY"))


(provide 'mk_chatgpt)
;;; mk_private.el ends here
