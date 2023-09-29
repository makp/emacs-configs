;;; mk_chatgpt.el --- Custom config for chatgpt -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(setq openai-key (getenv "OPENAI_KEY"))

(defun mk/chatgpt-select-model ()
  "Select GPT model to use."
  (interactive)
  (let ((gpt-version (completing-read "Que ce quer?: " '("gpt-4" "gpt-3.5-turbo"))))
    (setq chatgpt-model gpt-version))
  (chatgpt)
  (delete-other-windows))


(defun mk/chatgpt-write-message ()
  "Open a new buffer in `org-mode' to write a message for GPT."
  (interactive)
  (chatgpt-type-response)
  (org-mode)
  (evil-window-move-far-left)
  (local-set-key (kbd "C-c C-c") 'mk/chatgpt-send-message))


(defun mk/chatgpt-send-message ()
  "Send the contents of the buffer to chatgpt and close the window."
  (interactive)
  (chatgpt-input-mode)
  (chatgpt-input-send))

(provide 'mk_chatgpt)
;;; mk_chatgpt.el ends here
