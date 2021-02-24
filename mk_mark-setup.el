;;; my mark setup 

(setq set-mark-command-repeat-pop t)
;; If `set-mark-command-repeat-pop' is non-nil, repeating the C-SPC
;; command with no prefix argument pops the next position off the
;; local (or global) mark ring and jumps there.

;; ===================================
;; push-mark without activating region
;; ===================================
(defun mk/push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

;; -----------------
;; pop mark commands
;; -----------------
;; (global-set-key (kbd "C-z") 'pop-to-mark-command)
;;(global-set-key (kbd "C-S-z") 'mc/mark-pop) 

;; ;; =============
;; ;; mark commands
;; ;; =============
;; (global-set-key (kbd "M-r") 'mark-sexp)
;; (global-set-key (kbd "C-x C-r") 'set-mark-command)   ; it was find-file-read-only
;; (global-set-key (kbd "C-S-r") 'mark-paragraph)
;; ;; (global-set-key (kbd "M-R")

;;; 
(provide 'mk_mark-setup)
;;; my-mark-setup.el ends here