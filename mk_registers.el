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
(set-register ?E '(file . "/home/makmiller/config-files/general/emacs-config/"))
(set-register ?b '(file . "/home/makmiller/Documents/mydocs/references/dissert.bib"))
;; (set-register ?d '(file . "/home/makmiller/config-files/general/emacs-config/init.el"))

;;; Agendas
(set-register ?a '(file . "/home/makmiller/elisp/agenda/ag-academic.org"))
(set-register ?i '(file . "/home/makmiller/elisp/agenda/ag-it.org"))
(set-register ?o '(file . "/home/makmiller/elisp/agenda/ag-longterm.org"))
(set-register ?t '(file . "/home/makmiller/elisp/agenda/ag-teaching.org"))
(set-register ?g '(file . "/home/makmiller/elisp/agenda/ag-geral.org"))

;;; Notes
(set-register ?e '(file . "/home/makmiller/Documents/mydocs/notes/emacs-and-lisp/emacs.org"))
(set-register ?c '(file . "/home/makmiller/Documents/mydocs/notes/emacs-and-lisp/lisp.org"))
(set-register ?l '(file . "/home/makmiller/Documents/mydocs/notes/linux/linux.org"))

 ;; (set-register ?T '(file . "/home/makmiller/Documents/"))


;; ---------
;; Bookmarks
;; ---------
(setq
 bookmark-default-file "~/elisp/bookmarks"
 bookmark-save-flag 1)			; autosave each change

(provide 'mk_registers)