;; ======
;; AUCTex
;; ======
(load "auctex.el" nil t t)

(add-hook 'LaTeX-mode-hook (lambda ()
			     (add-to-list
			      'TeX-macro-global "~/texmf/tex/latex/")))

;; --------------
;; better C-a/C-e
;; --------------
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (define-key LaTeX-mode-map (kbd "C-a") 'mk/smarter-beginning-of-line)
	    ;; (define-key TeX-mode-map (kbd "C-c '") nil)
	    ))

(add-hook 'bibtex-mode-hook
	  (lambda ()
	    (define-key bibtex-mode-map (kbd "C-a") 'mk/smarter-beginning-of-line)))


;; Description: TeX-macro-global lists the directories where TeX style
;; files are. Is this necessary?

;; ---------------
;; default viewers
;; ---------------
(setq
 TeX-view-program-selection
 '((output-dvi "DVI Viewer")
   (output-pdf "PDF Viewer")
   (output-html "HTML Viewer"))
 ;; Description: the first element of the TeX-view-program-selection is
 ;; one or more predicates defined by TeX-view-predicate-list/buitin.
 ;; Case you want to use Doc-View as a the pdf viewer, use the command
 ;; 
 TeX-view-program-list
 '(("DVI Viewer" "okular %o") 
;;   ("PDF Viewer" "zathura -s -x \"emacsclient --eval '(progn (switch-to-buffer  (file-name-nondirectory \"'\"'\"%{input}\"'\"'\")) (goto-line %{line}))'\" %o")
   ("PDF Viewer" "okular --unique %u")
   ("HTML Viewer" "firefox %o")))
;; Description: about the options with okular, %n is the line of the
;; cursor and %b is the source file; the --unique option keeps a
;; single version of okular running

;; ------------------
;; Sectioning command
;; ------------------
(setq LaTeX-section-hook
      '(LaTeX-section-heading
	LaTeX-section-title 
	;; LaTeX-section-toc
	LaTeX-section-section
	LaTeX-section-label))

;; -------
;; Parsing
;; -------
(setq
 TeX-auto-save t			; enable parse on save 
 TeX-parse-self t)			; enable parse on load

(setq-default TeX-master nil) 		; query for master file


(setq TeX-save-query nil) 
;; Description: autosave before compiling


;; ------
;; quotes
;; ------

;;; double quotes
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
;;; Description: this allows me to add quotes to regions. This also
;;; makes the ‘"’ key “move over” existing quotation marks. E.g., if
;;; point is at the beginning of ``word'', hitting " places it at the
;;; first letter.

;;; single quotes
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

;; ---------
;; auto-fill
;; ---------
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

;; --------
;; flyspell
;; --------
(add-hook 'LaTeX-mode-hook
	  (lambda()
	    (flyspell-mode 1)))

;; ---------
;; math-mode
;; ---------
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode) ; always start math mode 

;; ======================
;; fold and outline modes
;; ======================

;; loading fold and outline modes
(add-hook 'LaTeX-mode-hook (lambda ()
			     ;; (TeX-fold-mode 1)
			     (outline-minor-mode 1)))


(add-hook 'outline-minor-mode-hook
	  (lambda () (local-set-key (kbd "C-S-c")
				    outline-mode-prefix-map)))

(setq TeX-fold-preserve-comments t)
;; Description: foldable items in your comments are not folded

(setq TeX-fold-env-spec-list 
      '(("[comment]" ("comment"))))
;; Description: environments taken into consideration in fold mode

(setq TeX-fold-macro-spec-list
      '(("[f]" ("footnote"))
	("[c]" ("cite" "citet" "citep" "citeyearpar"))
	("[l]" ("label"))
	("[r]" ("ref" "pageref" "eqref"))
	("[1]:||*" ("item"))
	("..." ("dots"))
	(1 ("part" "chapter" "section" "subsection" "subsubsection" "paragraph" "subparagraph" "part*" "chapter*" "section*" "subsection*" "subsubsection*" "paragraph*" "subparagraph*" "emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt" "textbf" "textsc" "textup"))))
;; Description: macros taken into consideration in fold mode

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

;; =======
;; Preview
;; =======
(load "preview-latex.el" nil t t)

(setq preview-auto-cache-preamble t)

(setq preview-image-type 'dvipng)

(setq preview-default-option-list (quote ("displaymath" "floats" "graphics" "textmath" "showlabels")))
;; Description: displaymath: all displayed math is subject to preview
;; processing; textmath: text math is subject to preview graphics: all
;; \includegraphics commands to preview sections

;; (setq
;; preview-inner-environments (quote ("Bmatrix" "Vmatrix" "aligned" "array" "bmatrix" "cases" "gathered" "matrix" "pmatrix" "smallmatrix" "split" "subarray" "vmatrix")))
;;; Environments not to be previewed on their own.

(setq preview-preserve-indentation nil)
(setq preview-scale-function 1.2)
					; when appearing on screen
;; preview-scale-from-face

;; ======
;; RefTeX
;; ======
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

(setq reftex-plug-into-AUCTeX t)	; integrate RefTeX with AUCTeX
(setq reftex-cite-format 'natbib)  	; natbib 

(setq reftex-default-bibliography
      (quote
       ("~/Documents/mydocs/references/dissert.bib"
	"~/Documents/mydocs/references/logic.bib")))

;; So that RefTeX also recognizes \addbibresource. Note that you
;; can't use $HOME in path for \addbibresource but that "~"
;; works.
(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))

(add-hook 'reftex-mode-hook
	  (lambda ()
	   (define-key reftex-mode-map (kbd "C-c r") 'reftex-citation)
	   (define-key reftex-mode-map (kbd "C-c v") 'reftex-view-crossref)
	   (define-key reftex-mode-map (kbd "C-c t") 'my-reftex-toc)))

(defun my-reftex-toc ()
  "Reloads toc."
  (interactive)
  (reftex-toc)
  (reftex-toc-rescan))

;; (setq reftex-toc-keep-other-windows t
;;       reftex-toc-split-windows-horizontally nil)

;; =======================
;; Forward/backward search
;; =======================
(setq
 TeX-source-correlate-mode t
 TeX-source-specials-mode t)
;; Description: TeX-source-correlate-mode toggles support for
;; forward/inverse search

(add-hook 'LaTeX-mode-hook (lambda ()
			     (add-to-list
			      'TeX-command-list
			      '("mk" "latexmk -pvc -pdf %s" TeX-run-TeX nil t
				:help "Run Latexmk on file"))))

(add-hook 'LaTeX-mode-hook (lambda ()
			     (add-to-list
			      'TeX-command-list
			      '("zathura" "zathura -s -x \"emacsclient --eval '(progn (switch-to-buffer  (file-name-nondirectory \"'\"'\"%{input}\"'\"'\")) (goto-line %{line}))'\" %o" TeX-run-TeX nil t
				:help "Run zathura on file"))))

(add-hook 'LaTeX-mode-hook (lambda ()
			     (add-to-list
			      'TeX-command-list
			      '("View DocView" "emacsclient -n -e '(find-file-other-window \"%o\")'" TeX-run-TeX nil t
				:help "View with docview"))))

(add-hook 'LaTeX-mode-hook (lambda ()
			     (add-to-list
			      'TeX-command-list
			      '("Zathura"  "zathura -s -x \"emacsclient --eval '(progn (switch-to-buffer  (file-name-nondirectory \"'\"'\"%{input}\"'\"'\")) (goto-line %{line}))'\" %o" TeX-run-TeX nil t
				:help "View pdf with zathura"))))

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

;;; 
(setq
 TeX-electric-sub-and-superscript t
 TeX-show-compilation nil
 TeX-newline-function 'reindent-then-newline-and-indent
 TeX-PDF-mode t)

(defun reverse-sync-no-mouse ()
  (interactive)
  (async-shell-command "~/myscripts/simulating-key-presses.sh")
  ;; (winner-undo)
  (message "Reverse synctex synchronization without the rodent"))

(global-set-key (kbd "s-C-.") 'reverse-sync-no-mouse)

;; ----------------
;; TeX-command-list
;; ----------------
;; (eval-after-load "tex"
;;   '(add-to-list 'TeX-command-list
;; 		'("postscript" "/usr/bin/ps4pdf %s" TeX-run-command nil t) t))

;; (eval-after-load "tex"
;;   '(add-to-list 'TeX-command-list
;; 		'("ps2pdf" "ps2pdf %f" TeX-run-command nil t) t))


;;; I'm using this line but I shouldn't have to
;; (require 'latex-mode-expansions)

;; (setq LaTeX-paragraph-commands '("minisec"))

;; ===========
;; Custom face
;; ===========
(setq font-latex-match-reference-keywords
      '(("citep" "[{")
	("citet" "[{")))

;; ---------------
;; New evironments
;; ---------------
;; (add-hook 'LaTeX-mode-hook
;;           (lambda ()
;;             (LaTeX-add-environments
;; 	     '("Verbatim")
;; 	     '("align*")
;; 	     '("SideBySideExample")	;; from fvrb-ex package
;; 	     '("Example")		;; 
;; 	     '("compactenum")		;; from paralist package
;; 	     '("inparaenum"))))

;; ----------
;; New macros
;; ----------
;; (add-hook 'LaTeX-mode-hook
;;           (lambda ()
;;             (TeX-add-symbols '("DP" 1))))

;; (setq font-latex-user-keyword-classes
;;       '(("definitions" (("DP" "{"))
;; 	 (:weight bold :foreground "chocolate1") command)))

(require 'auto-complete-auctex)
(provide 'mk_latex-setup)
