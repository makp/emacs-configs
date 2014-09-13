;; =================
;; pop-up ansi-terms
;; =================
(defun mk/ansi-term-popup ()
  "Toggle an ansi-term buffer."
  (interactive)
  (when (not (get-buffer "*ansi-term*"))
      (save-window-excursion (ansi-term (getenv "SHELL"))))
  (if (equal (buffer-name) "*ansi-term*")
      (quit-window)
    (switch-to-buffer-other-window "*ansi-term*")))

(define-key my-keys-minor-mode-map (kbd "C-x C-t") 'mk/ansi-term-popup)

(define-key my-keys-minor-mode-map (kbd "C-x p") '(lambda ()
						    (interactive)
						    (ansi-term "/bin/zsh")))


(provide 'mk_ansi-term)
;;; mk-shell ends here