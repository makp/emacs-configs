;;; bind the compile command
(add-hook 'c-mode-hook
	  (lambda ()
	    "C-c C-c for compile (it was comment region); C-c DEL
was rebiding my-helm to hungry-delete)."
	    (define-key c-mode-base-map (kbd "C-c C-c") nil) 
	    (define-key c-mode-base-map (kbd "C-c C-c") 'compile)
	    (define-key c-mode-base-map (kbd "C-c DEL") nil))) 	


(add-hook 'c-mode-hook
	  (lambda ()
	    "This will run Make if there is a Makefile in the
same directory as the source-file, or it will create a command
for compiling a single file and name the executable the same name
as the file with the extension stripped."
	    (unless (file-exists-p "Makefile")
	      (set (make-local-variable 'compile-command)
		   ;; emulate make's .c.o implicit pattern rule, but with
		   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
		   ;; variables:
		   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
		   (let ((file (file-name-nondirectory buffer-file-name)))
		     (format "%s -o %s %s"
			     (or (getenv "CC") "gcc")
			     (file-name-sans-extension file)
			     file))))))

;;; Note: I got the above function from EmacsWiki and it originally
;;; came with the following options (which I have to check what they
;;; are).

;; (format "%s -c -o %s.o %s %s %s"
;; 	(or (getenv "CC") "gcc")
;; 	(file-name-sans-extension file)
;; 	(or (getenv "CPPFLAGS") "-DDEBUG=9")
;; 	(or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
;; 	file)

(provide 'mk_cc)
