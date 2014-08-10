;; =============
;; pop-up shells
;; =============
(defun mk/ansi-term-popup ()
  "Toggle an ansi-term buffer."
  (interactive)
  (when (not (get-buffer "*ansi-term*"))
      (save-window-excursion (ansi-term (getenv "SHELL"))))
  (if (equal (buffer-name) "*ansi-term*")
      (quit-window)
    (switch-to-buffer-other-window "*ansi-term*")))

(global-set-key (kbd "C-x C-;") 'mk/ansi-term-popup)


(provide 'mk_shell)
;;; mk-shell ends here