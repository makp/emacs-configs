;;; mk_latex.el --- Custom config for LaTeX

;;; Commentary:

;;

;; TODO list:

;; Write a function to do reverse-sync without the mouse using
;; pdftools. I could write a bash script and call it with
;; async-shell-command.

;;; Code:


;; =======
;; parsing
;; =======
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (add-to-list
	     'TeX-macro-global "~/texmf/tex/latex/") ; specify location TeX style files
	    (flyspell-mode 1)))

(setq
 TeX-auto-save t   ;; enable parse on save
 TeX-parse-self t) ;; enable parse on load


;; ==========
;; navigation
;; ==========

;; --------------
;; better C-a/C-e
;; --------------
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (define-key LaTeX-mode-map (kbd "C-a") 'mk/smarter-beginning-of-line)
	    (define-key LaTeX-mode-map (kbd "C-c g") 'pdf-sync-forward-search)
	    (define-key LaTeX-mode-map (kbd "C-x n") nil)))

(add-hook 'bibtex-mode-hook
	  (lambda ()
	    (define-key bibtex-mode-map (kbd "C-a") 'mk/smarter-beginning-of-line)))

;; -------
;; outline
;; -------

(add-hook 'LaTeX-mode-hook
	  (lambda () (outline-minor-mode 1)))

(add-hook 'outline-minor-mode-hook
	  (lambda () (local-set-key (kbd "C-S-c")
				    outline-mode-prefix-map)))

(defmacro define-context-key (keymap key dispatch)
  "Define KEY in KEYMAP to execute according to DISPATCH.

DISPATCH is a form that is evaluated and should return the
command to be executed.

If DISPATCH returns nil, then the command normally bound to KEY
will be executed.

Example:

  (define-context-key hs-minor-mode-map
     (kbd \"<C-tab>\")
     (cond
      ((not (hs-already-hidden-p))
       'hs-hide-block)
      ((hs-already-hidden-p)
       'hs-show-block)))

This will make <C-tab> show a hidden block.  If the block is
shown, then it'll be hidden."
  `(define-key ,keymap ,key
     `(menu-item "context-key" ignore
		 :filter ,(lambda (&optional ignored)
			    ,dispatch))))

(defun th-outline-context-p ()
  "Non-nil if `point' is on an outline-heading."
  (save-excursion
    (goto-char (line-beginning-position))
    (looking-at outline-regexp)))

(define-context-key outline-minor-mode-map
  (kbd "TAB")
  (when (th-outline-context-p)
    'org-cycle))


;; ==========
;; appearance
;; ==========

(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    ;; (turn-on-auto-fill)
	    (setq line-spacing 1)))


;; ---------
;; fold mode
;; ---------
(add-hook 'LaTeX-mode-hook (lambda ()
			     (TeX-fold-mode 1)))


;; (setq TeX-fold-preserve-comments t)
;; ;; Foldable items in your comments are not folded

;; (setq TeX-fold-env-spec-list
;;       '(("[comment]" ("comment"))))
;; ;; Environments taken into consideration in fold mode

;; (setq TeX-fold-macro-spec-list
;;       '(("[f]" ("footnote"))
;; 	("[c]" ("cite" "citet" "citep" "citeyearpar"))
;; 	("[l]" ("label"))
;; 	("[r]" ("ref" "pageref" "eqref"))
;; 	("[1]:||*" ("item"))
;; 	("..." ("dots"))
;; 	(1 ("part" "chapter" "section" "subsection" "subsubsection" "paragraph" "subparagraph" "part*" "chapter*" "section*" "subsection*" "subsubsection*" "paragraph*" "subparagraph*" "emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt" "textbf" "textsc" "textup"))))
;; ;; Macros taken into consideration in fold mode


;; ======
;; output
;; ======
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
;; revert the PDF-buffer only after the TeX compilation has finished

(setq TeX-save-query nil) ;; autosave before compiling

;; ---------------
;; default viewers
;; ---------------
(setq
 TeX-view-program-selection
 '((output-dvi "DVI Viewer")
   (output-pdf "PDF Viewer")
   (output-html "HTML Viewer"))
 ;; The 1st element of the TeX-view-program-selection is one or more
 ;; predicates defined by TeX-view-predicate-list/buitin.
 TeX-view-program-list
 '(("DVI Viewer" "okular %o")
   ;;   ("PDF Viewer" "zathura -s -x \"emacsclient --eval '(progn (switch-to-buffer  (file-name-nondirectory \"'\"'\"%{input}\"'\"'\")) (goto-line %{line}))'\" %o")
   ("PDF Viewer" "okular --unique %u")
   ("HTML Viewer" "firefox %o")))
;; Okular switches: %n is the line of the cursor; %b is source file;
;; --unique keeps a single version of okular running.

;; -----------------------
;; Forward/backward search
;; -----------------------
(setq
 TeX-source-correlate-mode t
 TeX-source-specials-mode t)
;; TeX-source-correlate-mode toggles support for forward/inverse
;; search

(add-hook 'LaTeX-mode-hook (lambda ()
			     (add-to-list
			      'TeX-command-list
			      '("mk" "latexmk %s" TeX-run-TeX nil t
				:help "Run Latexmk on file"))))

;; (add-hook 'LaTeX-mode-hook (lambda ()
;; 			     (add-to-list
;; 			      'TeX-command-list
;; 			      '("zathura" "zathura -s -x \"emacsclient --eval '(progn (switch-to-buffer  (file-name-nondirectory \"'\"'\"%{input}\"'\"'\")) (goto-line %{line}))'\" %o" TeX-run-TeX nil t
;; 				:help "Run zathura on file"))))

(add-hook 'LaTeX-mode-hook '(lambda ()
			      (add-to-list 'TeX-expand-list
					   '("%u" Okular-make-url))))

(defun Okular-make-url () (concat
			   "file://"
			   (expand-file-name (funcall file (TeX-output-extension) t)
					     (file-name-directory (TeX-master-file)))
			   "#src:"
			   (TeX-current-line)
			   (expand-file-name (TeX-master-directory))
			   "./"
			   (TeX-current-file-name-master-relative)))


;; ======
;; macros
;; ======

;; ------------------
;; sectioning command
;; ------------------
(setq LaTeX-section-hook
      '(LaTeX-section-heading
	LaTeX-section-title
	;; LaTeX-section-toc
	LaTeX-section-section
	LaTeX-section-label))

;; ======
;; quotes
;; ======

;; wrap active region in double quotes (from EmacsWiki)
(defadvice TeX-insert-quote (around wrap-region activate)
  (cond
   (mark-active
    (let ((skeleton-end-newline nil))
      (skeleton-insert `(nil ,TeX-open-quote _ ,TeX-close-quote) -1)))
   ((looking-at (regexp-opt (list TeX-open-quote TeX-close-quote)))
    (forward-char (length TeX-open-quote)))
   (t
    ad-do-it)))
(put 'TeX-insert-quote 'delete-selection nil)

;; wrap active region with single quotes (from EmacsWiki)
(defun TeX-insert-single-quote (arg)
  (interactive "p")
  (cond
   (mark-active
    (let ((skeleton-end-newline nil))
      (skeleton-insert
       `(nil ?` _ ?') -1)))
   ((or (looking-at "\\<")
	(looking-back "^\\|\\s-\\|`"))
    (insert "`"))
   (t
    (self-insert-command arg))))

(add-hook 'LaTeX-mode-hook
	  '(lambda ()
	     (local-set-key "'" 'TeX-insert-single-quote)))


;; ===========
;; helm-bibtex
;; ===========
(autoload 'helm-bibtex "helm-bibtex" "" t)

(global-set-key (kbd "C-c r") 'helm-bibtex)

(with-eval-after-load 'helm-bibtex
  (helm-delete-action-from-source "Insert citation" helm-source-bibtex)
  (helm-add-action-to-source "Insert citation" 'helm-bibtex-insert-citation helm-source-bibtex 0))

(setq bibtex-completion-bibliography
      '("~/Documents/mydocs/tex-configs/references/logic.bib"
	"~/Documents/mydocs/tex-configs/references/dissert.bib"
	"~/Documents/mydocs/tex-configs/references/evol.bib")
      bibtex-completion-cite-default-as-initial-input t
      bibtex-completion-cite-prompt-for-optional-arguments nil
      bibtex-completion-cite-commands '("citep" "citet"))

;; (setq bibtex-completion-cite-default-command "citet")


(defun mk/bibtex-completion-format-citation-for-org (keys)
  "Formatter for inserting bibtex refs in org-mode references."
  (s-join ", " (cl-loop
                for key in keys
                for entry = (bibtex-completion-get-entry key)
                for author = (bibtex-completion-shorten-authors
                              (or (bibtex-completion-get-value "author" entry)
                                  (bibtex-completion-get-value "editor" entry)))
                for year = (bibtex-completion-get-value "year" entry)
		for title = (bibtex-completion-get-value "title" entry)
		collect (format "[[bib:%s][%s (%s) %s]]" key author year title))))

;; check the function bibtex-completion-format-citation-cite for ideas

(setq bibtex-completion-format-citation-functions
      '((org-mode . mk/bibtex-completion-format-citation-for-org)
	(latex-mode . bibtex-completion-format-citation-cite)
	(markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
	(default . bibtex-completion-format-citation-default)))

(setq bibtex-completion-pdf-field "File")

;; ======
;; RefTeX
;; ======
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

(setq reftex-plug-into-AUCTeX t)	; integrate RefTeX with AUCTeX
(setq reftex-cite-format 'natbib)  	; natbib

(setq reftex-default-bibliography
      (quote
       ("~/Documents/mydocs/tex-configs/references/dissert.bib"
	"~/Documents/mydocs/tex-configs/references/logic.bib")))

;; So that RefTeX also recognizes \addbibresource. Note that you
;; can't use $HOME in path for \addbibresource but that "~"
;; works.
;; (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))

(add-hook 'reftex-mode-hook
	  (lambda ()
	    ;; (define-key reftex-mode-map (kbd "C-c r") 'reftex-citation)
	    (define-key reftex-mode-map (kbd "C-c v") 'reftex-view-crossref)
	    (define-key reftex-mode-map (kbd "C-c t") 'my-reftex-toc)))

(defun my-reftex-toc ()
  "Reloads toc."
  (interactive)
  (reftex-toc)
  (reftex-toc-rescan))

;; (setq reftex-toc-keep-other-windows t
;;       reftex-toc-split-windows-horizontally nil)


;;; 
(setq
 TeX-electric-sub-and-superscript t
 TeX-show-compilation nil
 TeX-newline-function 'reindent-then-newline-and-indent
 TeX-PDF-mode t)

;; (setq LaTeX-paragraph-commands '("minisec"))

;; ===========
;; Custom face
;; ===========
;; (setq font-latex-match-reference-keywords
;;       '(("citep" "[{")
;; 	("citet" "[{")))

;; ---------------
;; New evironments
;; ---------------
;; (add-hook 'LaTeX-mode-hook
;;           (lambda ()
;;             (LaTeX-add-environments
;; 	     '("Verbatim")
;; 	     '("align*")
;; 	     '("SideBySideExample")	;; from fvrb-ex package
;; 	     '("Example")))		;; 

;; ----------
;; New macros
;; ----------
;; (add-hook 'LaTeX-mode-hook
;;           (lambda ()
;;             (TeX-add-symbols '("DP" 1))))

;; (setq font-latex-user-keyword-classes
;;       '(("definitions" (("DP" "{"))
;; 	 (:weight bold :foreground "chocolate1") command)))

;; ====
;; Math
;; ====

;; automatically insert opening and closing symbols for inline eqn
(setq TeX-electric-math (cons "$" "$"))

;; ---------
;; math-mode
;; ---------
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode) ; always start math mode

;; -----------
;; math-abbrev
;; -----------
;; (customize-set-variable 'LaTeX-math-abbrev-prefix (kbd ""))

(defun LaTeX-my-leftright (charopen charclose)
  "Inserts the pattern '\leftC \rightD' where C is the open input
char and D the closed, and places the cursor in the center."
  (interactive)
  (setq out1 (concat "\\left" charopen))
  (setq out2 (concat " \\right" charclose))
  (insert out1)
  (push-mark)
  (insert out2)
  (exchange-point-and-mark))

(setq LaTeX-math-list
      '((?8 "infty" "Misc Symbol" 8734)
	("(" (lambda ()(interactive)(LaTeX-my-leftright "(" ")")) "" nil))) ;it was langle originally


;; LaTeX-math-abbrev-prefix wraps $$ around symbol when in text mode
;; (from StackExchange TeX)
(add-hook
 'LaTeX-mode-hook
 (lambda ()
   (let ((math (reverse (append LaTeX-math-list LaTeX-math-default))))
     (while math
       (let ((entry (car math))
	     value)
	 (setq math (cdr math))
	 (if (listp (cdr entry))
	     (setq value (nth 1 entry))
	   (setq value (cdr entry)))
	 (if (stringp value)
	     (fset (intern (concat "LaTeX-math-" value))
		   (list 'lambda (list 'arg) (list 'interactive "*P")
			 (list 'LaTeX-math-insert value
			       '(null (texmathp)))))))))))

;; -------
;; Preview
;; -------
(setq preview-auto-cache-preamble t)

(setq preview-image-type 'dvipng)

;; (setq preview-default-option-list (quote ("displaymath" "floats" "graphics" "textmath" "showlabels")))
;; all displayed math is subject to preview

;; (setq
;; preview-inner-environments (quote ("Bmatrix" "Vmatrix" "aligned" "array" "bmatrix" "cases" "gathered" "matrix" "pmatrix" "smallmatrix" "split" "subarray" "vmatrix")))
;;; Environments not to be previewed on their own.

(setq preview-preserve-indentation nil)
(setq preview-scale-function 1.2)
;; preview-scale-from-face




(provide 'mk_latex)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; mk_latex.el ends here
