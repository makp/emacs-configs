;;; mk_chatgpt.el --- Custom config for chatgpt -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(setq openai-key (getenv "OPENAI_KEY"))

(defun mk/chatgpt ()
  "Select GPT model to use."
  (interactive)
  (let ((gpt-version (completing-read "Que ce quer?: " '("gpt-4" "gpt-3.5-turbo"))))
    (setq chatgpt-model gpt-version))
  (chatgpt))


(provide 'mk_chatgpt)
;;; mk_chatgpt.el ends here
