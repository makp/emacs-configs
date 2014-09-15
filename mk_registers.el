;; -----------------------
;; file names in registers
;; -----------------------
(defun mk/jump-to-register (&optional arg)
  "With prefix arg, run jump-to-register in other window."
  (interactive "P")
  (when (consp arg)
    (switch-to-buffer-other-window (current-buffer)))
  (call-interactively 'jump-to-register))

(global-set-key (kbd "C-x r j") 'mk/jump-to-register)


;;; Config files
(set-register ?E '(file . "~/config-files/general/emacs-config/"))
(set-register ?b '(file . "~/Documents/mydocs/tex-configs/references/dissert.bib"))
;; (set-register ?d '(file . "~/config-files/general/emacs-config/init.el"))

;;; Agendas
(set-register ?a '(file . "~/elisp/agenda/ag-academic.org"))
(set-register ?i '(file . "~/elisp/agenda/ag-it.org"))
(set-register ?o '(file . "~/elisp/agenda/ag-longterm.org"))
(set-register ?t '(file . "~/elisp/agenda/ag-teaching.org"))
(set-register ?g '(file . "~/elisp/agenda/ag-geral.org"))
(set-register ?w '(file . "~/elisp/agenda/wasteclock.org"))

;;; Notes
(set-register ?e '(file . "~/Documents/mydocs/notes/nontech-notes/english-and-spanish/english.org"))
;; (set-register ?c '(file . "~/Documents/mydocs/notes/"))
;; (set-register ?l '(file . "~/Documents/mydocs/notes/"))

;; (set-register ?T '(file . "~/Documents/"))

;; ---------
;; Bookmarks
;; ---------
(setq
 bookmark-default-file "~/Dropbox/shared-files/emacs-bookmarks"
 bookmark-save-flag 1)			; autosave each change

(provide 'mk_registers)