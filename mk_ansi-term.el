;;; mk_ansi-term.el --- Toggle ansi-term -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(defun mk/ansi-term-popup ()
  "Toggle an 'ansi-term' buffer."
  (interactive)
  (when (not (get-buffer "*ansi-term*"))
    (save-window-excursion (ansi-term "/usr/bin/zsh")))
  (if (equal (buffer-name) "*ansi-term*")
      (quit-window)
    (switch-to-buffer-other-window "*ansi-term*")))

(defun mk/call-ansi-term (&optional arg)
  "Call 'ansi-term'. If ARG is non-nil, call a new instance of 'ansi-term'."
  (interactive "P")
  (if (not (consp arg))
      (mk/ansi-term-popup)
    (ansi-term "/usr/bin/zsh")))

;; (setq term-buffer-maximum-size 0) ; in terms of lines


(provide 'mk_ansi-term)

;;; mk_ansi-term.el ends here
