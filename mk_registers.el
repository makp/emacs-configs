;; -----------------------
;; file names in registers
;; -----------------------
(defun mk/jump-to-register (&optional arg)
  "If ARG non-nil, run `jump-to-register' in another window."
  (interactive "P")
  (when arg
    (switch-to-buffer-other-window (current-buffer)))
  (call-interactively 'jump-to-register))

;;; Notes
;; (set-register ?E '(file . "~/Documents/mydocs/notes/nontech-notes/english-and-spanish/english.org"))
;; (set-register ?c '(file . "~/Documents/mydocs/notes/"))
;; (set-register ?l '(file . "~/Documents/mydocs/notes/"))

;; (set-register ?T '(file . "~/Documents/"))

;; ---------
;; Bookmarks
;; ---------
;; (setq
;;  bookmark-default-file "~/Dropbox/shared-files/emacs-bookmarks"
;;  bookmark-save-flag 1)			; autosave each change

(provide 'mk_registers)