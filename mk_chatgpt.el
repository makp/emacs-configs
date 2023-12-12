;;; mk_chatgpt.el --- Custom config for chatgpt -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

;; 
(setq-default openai-key (getenv "OPENAI_KEY")
	      chatgpt-window-prompt " ")


(defun mk/chatgpt-write-message ()
  "Open a new buffer in `org-mode' to write a message for GPT."
  (interactive)
  (chatgpt-type-response)
  (org-mode)
  (evil-window-move-far-left)
  (evil-insert-state))


(defun mk/chatgpt-select-model ()
  "Select GPT model to use."
  (interactive)
  (let ((gpt-version (completing-read "Que ce quer?: " '("gpt-4-1106-preview" "gpt-3.5-turbo-1106"))))
    (setq chatgpt-model gpt-version))
  (chatgpt)
  (delete-other-windows)
  (mk/chatgpt-write-message))


(defun mk/chatgpt-send-message ()
  "Send the contents of the buffer to chatgpt and close the window."
  (interactive)
  (chatgpt-input-mode)
  (chatgpt-input-send))

(provide 'mk_chatgpt)
;;; mk_chatgpt.el ends here
