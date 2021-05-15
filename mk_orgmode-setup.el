;;; mk_orgmode-setup.el --- Custom config for org-mode -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:


;; ===============
;; better defaults
;; ===============
(setq-default org-special-ctrl-a/e t	       ; C-a/C-e behavior in headlines
	      org-goto-auto-isearch t	       ; org-goto
	      org-return-follows-link t	       ; follow links with RET
	      org-pretty-entities nil	       ; UFT8 characters
	      org-special-ctrl-k t
	      org-fontify-quote-and-verse-blocks 1 ; add special face to #+begin_quote blocks
	      org-enforce-todo-dependencies t
	      ;; org-use-speed-commands t
	      org-imenu-depth 6)


;; ========
;; flyspell
;; ========
(add-hook 'org-mode-hook
	  (lambda()
	    (flyspell-mode 1)))

;; =========
;; refilling
;; =========
(setq-default org-refile-targets
	      '((org-agenda-files . (:maxlevel . 3)))
	      org-outline-path-complete-in-steps nil
	      org-refile-use-outline-path t
	      org-refile-allow-creating-parent-nodes 'confirm)

;; ========
;; clocking
;; ========
(org-clock-persistence-insinuate) ;; save the clock history across Emacs sessions

(setq-default org-clock-persist t
	      org-clock-out-remove-zero-time-clocks t ;; remove clock entries with zero duration
	      org-clock-mode-line-total 'today
	      org-clock-clocked-in-display 'both
	      org-timer-display 'mode-line)

;; =================
;; Structure editing
;; =================

(setq org-adapt-indentation nil)


;; -------
;; drawers
;; -------
;; (setq org-drawers '("NOTES" "PROPERTIES" "CLOCK" "LOGBOOK" "REFERENCES" "EMAIL" "PROGRESS"))

;; ==========
;; hyperlinks
;; ==========

;; ------------------
;; link abbreviations
;; ------------------

;;; link abbreviations syntax:
;;; [[linkword:tag][description]], where liknword must be a word

(setq-default org-link-abbrev-alist
	      '(("gg" . "http://www.google.com/search?q=")
		("gmap"	  . "http://maps.google.com/maps?q=%s")
		("bib" . "~/Documents/mydocs/tex-configs/references/evol.bib::%s")))

;; ==========
;; todo items
;; ==========
;; ---------------
;; custom keywords
;; ---------------
(setq-default org-todo-keywords
	      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
		(sequence "WRITE(w)" "REVISE(r)" "PROOF-READ(p)" "SUBMITTED(s)" "|" "DONE(d)")
		(type "FIXME(f)" "BUG(b)" "LEARN(l)" "|" "FIXED(F)" "DONE(d)")))

;; NOTE: "sequence" is used to indicate that the TODO items
;; form a sequence. In contrast, with "type" we simply have different
;; states without forming a sequence. The vertical bar separates the
;; todo keywords from the done state.

;; -----
;; faces
;; -----
(setq-default org-todo-keyword-faces
	      '(("TODO" . "green")
		("NEXT" . "Palegreen3")
		("PROOF-READ" . "yellow")
		("WRITE" . "orange")
		("REVISE" . "chocolate1")
		("PROOF-READ" . "chocolate1")
		("SUBMITTED" . "sienna2")
		("FIXME" . "tomato2")
		("BUG" . "firebrick1")
		("LEARN" . "Orangered1")))

;; ====
;; tags
;; ====
;;; custom tags
(setq-default org-tag-alist '(("PROJECT" . ?P)
			      ("paper" . ?p)
			      ("sideproject" . ?s)
			      ("IT" . ?i)
			      ("email" . ?e)
			      ("ref" . ?r)
			      ("@WAITING" . ?w)
			      ("@Today" . ?t)
			      ("export". ?E)
			      ("noexport" . ?N)))

;; don't inherit the following tags
(setq-default org-tags-exclude-from-inheritance '("@Today" "PROJECT" "@WAITING" "IT" "sideproject"))

;; =======
;; Agendas
;; =======

;; ------------
;; Agenda files
;; ------------
(setq-default org-agenda-files (list "~/elisp/agendas/ag-academic.org"
				     "~/elisp/agendas/ag-projects.org"
				     "~/elisp/agendas/ag-longterm.org"
				     "~/elisp/agendas/ag-teaching.org"
				     "~/elisp/agendas/ag-geral.org"
				     "~/elisp/agendas/gcal.org"
				     "~/elisp/agendas/ag-it.org"))

(defun mk/select-agenda()
  "Select and open one of the agenda files."
  (interactive)
  (let ((var (completing-read "Select agenda: "
			      org-agenda-files nil t "ag-")))
    (find-file var)))

;;; agenda dispatcher
(setq-default org-agenda-custom-commands
	      '(("n" "Today tasks"
		 ((tags "@Today")
		  (agenda "" ((org-agenda-span 1)
			      (org-agenda-show-all-dates nil)))))
		("p" "Papers and projects"
		 ((tags "PROJECT|paper"))) ; tags-todo
		("N" "Pending stuff"
		 ((tags "@Today")
		  (agenda "" ((org-agenda-span 1)
			      (org-agenda-show-all-dates nil)))
		  (tags "@WAITING")))
		("q" "Quick tasks"
		 ((tags "IT")
		  (tags "email")
		  (tags "ref")))
		("w" "Agenda pra semana"
		 ((agenda "" ((org-agenda-span 7)
			      (org-agenda-show-all-dates nil)))))
		("W" "Proximas duas semanas" agenda "" ((org-agenda-span 14)
							(org-agenda-show-all-dates nil)))))

;; (org-agenda-overriding-header "")
;; (setq org-priority-faces '((?A . (:foreground "yellow"))))
;; (setq org-icalendar-use-scheduled '(todo-start event-if-todo))

(setq-default org-columns-default-format ;; column view
	      "%1PRIORITY %5TODO %20ITEM(Task) %10SCHEDULED %10CLOCKSUM_T %10CLOCKSUM %10Effort(Effort) %TAGS"
	      org-global-properties ;; global effort estimate values
	      '(("Effort_ALL" . "1:00 2:00 3:00 4:00 0:15 0:30 0:45"))
	      org-clock-into-drawer t
	      org-log-done 'time
	      org-clock-idle-time nil)


;; -------
;; Capture
;; -------
(setq-default org-directory "~/elisp/agendas/")
(setq-default org-default-notes-file (concat org-directory "ag-geral.org"))

(setq-default org-capture-templates
	      '(("a" "Research" entry (file+headline "~/elisp/agendas/ag-academic.org" "DIVERSE")
		 "** TODO %?\n  %i\n" :empty-lines 1)
		("r" "Refs" entry (file+headline "~/elisp/agendas/ag-academic.org" "Ref hunt")
		 "** TODO %?\n  %i\n" :empty-lines 1)
		("t" "Teaching" entry (file+headline "~/elisp/agendas/ag-teaching.org" "DIVERSE")
		 "** TODO %?\n %i\n" :empty-lines 1)
		("g" "Geral" entry (file+headline "~/elisp/agendas/ag-geral.org" "NON-RECURRENT TODOs")
		 "** TODO %?\n %i\n" :empty-lines 1)
		("e" "Emacs stuff" entry (file+headline "~/elisp/agendas/ag-it.org" "EMACS")
		 "** TODO %? %(org-set-tags \":IT:\") \n %i  \n" :empty-lines 1)
		("l" "Linux stuff" entry (file+headline "~/elisp/agendas/ag-it.org" "LINUX")
		 "** TODO %? %(org-set-tags \":IT:\") \n %i  \n" :empty-lines 1)))

;; ----
;; gcal
;; ----
(require 'mk_gcal)

(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-fetch)))
;; (setq org-gcal-notify-p nil)


;; =============
;; inline images
;; =============
(setq org-startup-with-inline-images t)
;;; Non-nil means show inline images when loading a new Org file.

;; =========
;; Exporting
;; =========
;; (setq org-export-with-sub-superscripts "{}")
;;; Description: the braces are *required* in order to trigger
;;; interpretations as sub/superscript.

(setq-default org-export-with-toc nil
	      org-export-with-section-numbers nil
	      org-export-initial-scope 'subtree)

;; latex exporting
(setq-default org-latex-pdf-process (list "latexmk %s")
	      org-export-with-LaTeX-fragments 'dvipng) ;covert tex frags into imgs

;; -----
;; latex
;; -----
;; (require 'org-latex)

;; (unless (boundp 'org-export-latex-classes)
;;   (setq org-export-latex-classes nil))

;; (add-to-list 'org-export-latex-classes
;; 	     '("amsart"
;; 	       "\\documentclass[a4paper]{amsart}
;; \\usepackage{setspace}
;; \\usepackage{color}
;; \\usepackage{framed}
;; \\usepackage{comment}
;; \\definecolor{shadecolor}{gray}{0.875}
;; \\specialcomment{comm}{\\begin{shaded}}{\\end{shaded}}"
;; 	       ("\\section{%s}" . "\\section*{%s}")
;; 	       ("\\subsection{%s}" . "\\subsection*{%s}")
;; 	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;; 	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
;; 	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;; =====
;; babel
;; =====
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (lisp . t)
   (clojure . t)
   (python . t)
   (shell . t)
   (latex . t)
   (C . t)
   (mathematica . t)
   ;; (R . t)
   ))


(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-confirm-babel-evaluate nil)

(setq org-babel-clojure-backend 'cider)

(add-to-list 'org-src-lang-modes '("mathematica" . "wolfram")) ; use wolfram-mode instead of mma-mode
(setq-default org-babel-mathematica-command "wolframscript -f")



;; -----------------------
;; system-wide header args
;; -----------------------
(setq org-babel-default-header-args
      (cons '(:results . "value replace")
	    (assq-delete-all :results org-babel-default-header-args)))

;;; "output": the result is the collection of everything printed to
;;; STDOUT during the execution of the code block. An alternative is
;;; "value" in which the result is the value of the last statement in
;;; the code block.

;; (setq org-babel-default-header-args
;;       (cons '(:comments . "link")
;; 	    (assq-delete-all :comments org-babel-default-header-args)))

;; ---------------------
;; Display inline images
;; ---------------------
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(add-hook 'org-mode-hook 'org-display-inline-images)

;; ======================
;; Integration with LaTeX
;; ======================

;; ------
;; reftex
;; ------
;; (defun org-mode-reftex-setup ()
;;   (load-library "reftex")
;;   (and (buffer-file-name)
;;        (file-exists-p (buffer-file-name))
;;        (progn
;;          (reftex-parse-all)
;; 	 ;; add a custom reftex cite format to insert links
;;          (reftex-set-cite-format
;;           '((?b . "[[bib:%l][%l-bib]]")
;;             (?t . "[[bib:%l][%2a (%y)]]")
;; 	    (?h . "[[bib:%l][%A %y %t]]\n  ")))))
;;   (define-key org-mode-map (kbd "C-c r") 'reftex-citation))

;; ;;; Notes:
;; ;;; 1. using load-library instead of (reftex-mode t) is good because
;; ;;;    in that way org-mode keybindings are not overwritten by the
;; ;;;    reftex mode -- e.g., C-c ] which is bound to org-remove-file.
;; ;;; 2. this code will only work after I load my auctex config
;; ;;;    (otherwise reftex won't load my default bib file.

;; (add-hook 'org-mode-hook 'org-mode-reftex-setup)

;; -------
;; preview
;; -------
(setq org-format-latex-options '(:foreground default :background
					     default :scale
					     1.5 :html-foreground "Black" :html-background "Transparent" :html-scale
					     1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

;; (setq org-latex-create-formula-image-program 'dvipng)
;; NOTE: Apparently this var is obsolete

;; ============
;; org-annotate
;; ============
;; (require 'org-annotate-file)
;; (defun ache-annotate (&optional n)
;;   "Calls org-annotate-file except that, with a numeric argument,
;; it opens annoted.org in another window"
;;   (interactive "P")
;;   (if (null n)
;;       (org-annotate-file)
;;     (switch-to-buffer-other-window (current-buffer))
;;     (org-annotate-file)))

;; (global-set-key (kbd "C-c n") 'ache-annotate)
;; (setq org-annotate-file-storage-file "~/elisp/annotated.org")

;; (setq org-time-clocksum-format (quote (:days "%dd" :minutes "%dm" :require-minutes t)))
;; NOTE: Apparently this var is obsolete

(add-hook 'org-timer-done-hook
	  (lambda()
	    (message "END OF TIME BURST (%s)!" (current-time-string))))

(defun mk/clock-in (&optional n)
  "Modified version of `org-clock-in'.
If in an org buffer, run `org-clock-in'. Otherwise run `org-clock-in-last'.
If N > 1, open a list of previously clocked items to choose from."
  (interactive "p")
  (if (> n 1)
      (setq n '(4))
    (setq n nil))
  (let ((current-prefix-arg n))
    (if (eq major-mode 'org-mode)
	(call-interactively 'org-clock-in)
      (call-interactively 'org-clock-in-last)))
  ;; (org-save-all-org-buffers)
  )

(setq-default org-timer-default-timer 25)

;; Preferred applications
(setq org-file-apps (quote ((auto-mode . emacs)
			    ("\\.x?html?\\'" . default)
			    ("\\.pdf\\'" . "/usr/bin/okular %o"))))

;; ;; ===========
;; ;; scratch.org
;; ;; ===========
;; (find-file "~/elisp/cache/scratch-org.org")

;; (defun take-notes (&optional arg)
;;   "Toggle `scratch-org.org'. With a prefix arg, opens
;; `scratch-org.org' in another window."
;;   (interactive "P")
;;   (if (equal (buffer-name) "scratch-org.org")
;;       (bury-buffer)
;;     (if (consp arg)
;; 	(switch-to-buffer-other-window "scratch-org.org")
;;       (switch-to-buffer "scratch-org.org"))))

;; ;;; sound to use for notifications
;; ;; (setq org-clock-sound t)
;; ;;; not working

;; (defun org-column-view-uses-fixed-width-face ()
;;   ;; copy from org-faces.el
;;   (when (fboundp 'set-face-attribute)
;;     ;; Make sure that a fixed-width face is used when we have a column
;;     ;; table.
;;     (set-face-attribute 'org-column nil
;;                         :height (face-attribute 'default :height)
;;                         :family (face-attribute 'default :family))))

;; (when (and (fboundp 'daemonp) (daemonp))
;;   (add-hook 'org-mode-hook 'org-column-view-uses-fixed-width-face))

;; --------------
;; wasteclock.org
;; --------------
(defun clock-wasteclock ()
  (save-excursion
    (when (get-buffer "wasteclock.org")
      (switch-to-buffer "wasteclock.org"))
    (org-dblock-update 4)
    (goto-char (point-min))
    (outline-next-visible-heading 1)
    (org-end-of-line)
    (let ((base-pos (point)))
      (call-interactively
       'helm-imenu)
      (unless (equal base-pos (point))
	(org-clock-in))
      (save-buffer)
      (bury-buffer))))

(defun check-wasteclock ()
  "Open wasteclock.org and stay there."
  (switch-to-buffer "wasteclock.org")
  (org-dblock-update 4)
  (outline-next-visible-heading 2))

(defun goto-wasteclock (&optional arg)
  "bla"
  (interactive "P")
  (if (consp arg)
      (check-wasteclock)
    (clock-wasteclock)
    (org-save-all-org-buffers)))

;; --------------
;; Quick clock-in
;; --------------
(defun mk/quick-clockin ()
  "Quick way of clocking in using TAGS."
  (interactive)
  (unless (get-buffer "agendas")
    (find-file-noselect "~/elisp/agendas"))
  (with-current-buffer "agendas"
    (call-interactively 'helm-etags-select)
    (org-clock-in)
    (save-buffer)
    (bury-buffer)))

;; --------------
;; clocktable.org
;; --------------

(copy-file "/home/makmiller/config-files/general/emacs-configs/template_clocktable.org"
	   "/home/makmiller/elisp/agendas/clocktable.org" t)
(find-file-noselect "/home/makmiller/elisp/agendas/clocktable.org")

(defun chama-clock-table (&optional arg)
  "Toggle `clocktable.org'. With a prefix argument, open
wastetime.org."
  (interactive "P")
  (if (consp arg)
      (switch-to-buffer "clocktable.org")
    (save-window-excursion
      (delete-other-windows)
      (switch-to-buffer "clocktable.org")
      (org-dblock-update 4)
      (save-buffer)
      (goto-char (point-min))
      (outline-next-visible-heading 2)
      (org-tree-to-indirect-buffer)
      (split-window-right)
      (outline-next-visible-heading -1)
      (org-tree-to-indirect-buffer '4)
      (delete-window)
      (read-key "Press any key to exit.")
      (kill-buffer "clocktable.org-Today-1")
      (kill-buffer "clocktable.org-Yesterday-1"))))

;; (global-set-key (kbd "\e\e m") 'chama-clock-table)


(provide 'mk_orgmode-setup)
;;; mk_orgmode-setup.el ends here
