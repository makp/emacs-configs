;;; The intuition behind *ibuffer* is to have a buffer menu that lets
;;; you operate on buffers much in the same manner as Dired. Some
;;; operations:

;;; * ‘M-s a C-s’ - Do incremental search in the marked buffers.
;;; * ‘M-s a C-M-s’ - Isearch for regexp in the marked buffers.
;;; * ‘U’ - Replace by regexp in each of the marked buffers.
;;; * ‘Q’ - Query replace in each of the marked buffers.
;;; * ‘I’ - As above, with a regular expression.
;;; * '=' - Show differences between an unsaved buffer and the file on disk.

(global-set-key (kbd "C-x C-b") 'ibuffer)
;; (autoload 'ibuffer "ibuffer" "List buffers." t)
(require 'ibuffer)
(require 'ibuffer-git)

(setq ibuffer-saved-filter-groups
      (quote (("default"      
	       ("agendas"
		(or
		 (filename . "/home/makmiller/elisp/agenda/ag-geral.org")
		 (filename . "/home/makmiller/elisp/agenda/ag-teaching.org")
		 (filename . "/home/makmiller/elisp/agenda/ag-longterm.org")
		 (filename . "/home/makmiller/elisp/agenda/ag-academic.org")
		 (filename . "/home/makmiller/elisp/agenda/ag-it.org")))
	       ("org files" 
		(mode . org-mode))
	       ("info"
		(mode . info-mode))
	       ("latex"
		(or 
		 (mode . latex-mode)))
	       ("bibtex"
		(or 
		 (mode . bibtex-mode)))	    
	       ("shell"
		(or 
		 (mode . term-mode)))	   
	       ("scripting"
		(or
		 (mode . perl-mode)
		 (mode . sh-mode)
		 (mode . python-mode)
		 (mode . conf-xdefaults-mode)))
	       ("programming" 
		(or
		 (mode . c-mode)
		 (mode . emacs-lisp-mode)
		 (mode . haskell-mode)
		 (mode . lisp-mode))) 
	       ("pdfs"
		(or 
		 (mode . doc-view-mode)))
	       ("ERC"
		(mode . erc-mode))
	       ("dired"
		(or
		 (mode . dired-mode)))))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-auto-mode 1) 	;update
	    (ibuffer-switch-to-saved-filter-groups "default")))

;;; some setqs
(setq
 ibuffer-show-empty-filter-groups nil ;; don't show empty filter groups
 ibuffer-expert t) ;; don't ask for confirmation of "dangerous" operations.

(require 'ibuf-ext)
(dolist (ibfilter '("^\\*" "_region_"))
  (add-to-list 'ibuffer-never-show-predicates ibfilter))

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000) (format "%7.3fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000000) (format "%7.3fM" (/ (buffer-size) 1000000.0)))
   (t (format "%8d" (buffer-size)))))

;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only git-status-mini " "
	      (name 18 18 :left :elide)
	      " "
	      (size-h 9 -1 :right)
	      " "
	      (mode 16 16 :left :elide)
	      " "
	      (git-status 8 8 :left)
	      " " filename-and-process)))

(defun ibuffer-ediff-marked-buffers ()
  (interactive)
  (let* ((marked-buffers (ibuffer-get-marked-buffers))
         (len (length marked-buffers)))
    (unless (= 2 len)
      (error (format "%s buffer%s been marked (needs to be 2)"
                     len (if (= len 1) " has" "s have"))))
    (ediff-buffers (car marked-buffers) (cadr marked-buffers))))

(define-key ibuffer-mode-map "e" 'ibuffer-ediff-marked-buffers)

(define-key ibuffer-mode-map (kbd "U") 'ibuffer-unmark-all)
;;; to be consistent with dired-mode. It was ibuffer.*regexp

(provide 'mk_ibuffer-setup)
