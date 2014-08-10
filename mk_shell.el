;; =============
;; pop-up shells
;; =============
;; (defvar th-shell-popup-buffer nil)

;; (defun th-shell-popup ()
;;   "Toggle a shell popup buffer with the current file's directory as cwd."
;;   (interactive)
;;   (unless (buffer-live-p th-shell-popup-buffer)
;;     (save-window-excursion (shell "*Popup Shell*"))
;;     (setq th-shell-popup-buffer (get-buffer "*Popup Shell*")))
;;   (let ((win (get-buffer-window th-shell-popup-buffer))
;; 	(dir (file-name-directory (or (buffer-file-name)
;; 				      ;; dired
;; 				      dired-directory
;; 				      ;; use HOME
;; 				      "~/"))))
;;     (if win
;; 	(quit-window nil win)
;;       (pop-to-buffer th-shell-popup-buffer nil t)
;;       (comint-send-string nil (concat "cd " dir "\n")))))

(defun mk/ansi-term-popup ()
  "Toggle an ansi-term buffer."
  (interactive)
  (when (not (get-buffer "*ansi-term*"))
      (save-window-excursion (ansi-term (getenv "SHELL"))))
  (if (equal (buffer-name) "*ansi-term*")
      (quit-window)
    (switch-to-buffer-other-window "*ansi-term*")))

;; (defun visit-ansi-term-buffer ()
;;   "Create or visit a terminal buffer."
;;   (interactive)
;;   (if (not (get-buffer "*ansi-term*"))
;;       (progn
;;         (split-window-sensibly (selected-window))
;;         (other-window 1)
;;         (ansi-term (getenv "SHELL")))
;;     (switch-to-buffer-other-window "*ansi-term*")))

(global-set-key (kbd "C-x C-;") 'mk/ansi-term-popup)


(provide 'mk_shell)
;;; mk-shell ends here