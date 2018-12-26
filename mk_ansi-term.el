;; =================
;; pop-up ansi-terms
;; =================

(defun mk/ansi-term-popup ()
  "Toggle an ansi-term buffer."
  (interactive)
  (when (not (get-buffer "*ansi-term*"))
    (save-window-excursion (ansi-term "/usr/bin/fish")))
  (if (equal (buffer-name) "*ansi-term*")
      (quit-window)
    (switch-to-buffer-other-window "*ansi-term*")))

(defun mk/chama-ansi-term (&optional arg)
  "Function to call ansi-term. With a prefix argument, call a new
  instance of ansi-term"
  (interactive "P")
  (if (not (consp arg))
      (mk/ansi-term-popup)
    (ansi-term "/usr/bin/fish")))

(setq term-buffer-maximum-size 0)
;;; 

(provide 'mk_ansi-term)
;;; mk-shell ends here
