(message ">>>>> custom org-mode setup triggered <<<<<<")

;(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; info directory
;; (add-to-list 'Info-default-directory-list "~/...")


;; =====
;; setqs
;; =====
(setq org-special-ctrl-a/e t)	       ; C-a/C-e behavior in headlines
(setq org-goto-auto-isearch t)	       ;  org-goto
(setq org-return-follows-link t)       ; follow links with RET
(setq org-pretty-entities nil)		; Use UFT8 characters
(setq org-special-ctrl-k t)
;; Description:
;; - When the cursor is at the beginning of a headline, kill the entire
;;   line and possible the folded subtree below the line.
;; - When in the middle of the headline text, kill the headline up to the tags.
;; - When after the headline text, kill the tags.

;; ========
;; flyspell
;; ========
(add-hook 'org-mode-hook
	  (lambda()
	    (flyspell-mode 1)))

;; =====
;; imenu
;; =====
(setq org-imenu-depth 6)


;; =========
;; refilling
;; =========
(setq org-refile-targets
      '((org-agenda-files . (:maxlevel . 3)))
      org-outline-path-complete-in-steps nil
      org-refile-use-outline-path t
      org-refile-allow-creating-parent-nodes 'confirm)

;; (setq org-export-with-sub-superscripts "{}")
;;; Description: the braces are *required* in order to trigger
;;; interpretations as sub/superscript.


;; ===============
;; latex exporting
;; ===============
(setq org-export-with-LaTeX-fragments 'dvipng)
;;; All LaTeX fragments are converted into images and inlined into the
;;; document

;; ========
;; clocking
;; ========
(setq org-clock-persist t)
(org-clock-persistence-insinuate)
;; Save the clock history across Emacs sessions.

(setq org-clock-out-remove-zero-time-clocks t)
;; Remove clock entries with a zero duration

(setq org-clock-mode-line-total 'today)

(setq org-clock-clocked-in-display 'frame-title)
;;; where to display talk/time

;; =================
;; Structure editing
;; =================

;; ----------------------------
;; no arrow keys and shift-tabs
;; ----------------------------

(add-hook 'org-mode-hook
	  (lambda ()
	    ;; for promoting and demoting headings
	    (define-key org-mode-map (kbd "C-S-f") 'org-metaright)
	    (define-key org-mode-map (kbd "C-S-b") 'org-metaleft)
	    
	    ;; (define-key org-mode-map (kbd "M-T") 'org-metadown)
	    ;; (define-key org-mode-map (kbd "M-N") 'org-metaup)

	    ;; demoting and demoting a heading and its subtrees
	    (define-key org-mode-map (kbd "M-B") 'org-shiftmetaleft)
	    (define-key org-mode-map (kbd "M-F") 'org-shiftmetaright)
	    ;; (define-key org-mode-map (kbd "C-S-t") 'org-shiftup)
	    
	    (define-key org-mode-map (kbd "C-c t") 'org-shifttab)
	    (define-key org-mode-map (kbd "C-c SPC") nil)
	    (define-key org-mode-map (kbd "M-h") nil)

	    (define-key org-mode-map (kbd "C-c C-j") 'org-insert-todo-heading-respect-content)));; I don't use the org jump mode. 

;;; org-insert-heading

;; ----------
;; speed keys
;; ----------
(setq org-use-speed-commands t)

;;; org-speed-commands-user

;; -------
;; drawers
;; -------
;; (setq org-drawers '("NOTES" "PROPERTIES" "CLOCK" "LOGBOOK" "REFERENCES" "EMAIL" "PROGRESS"))


;; ==========
;; hyperlinks
;; ==========
(global-set-key (kbd "C-c l") 'org-store-link) 
(global-set-key (kbd "C-c C-S-l") 'org-insert-link-global)
(global-set-key (kbd "C-c C-S-o") 'org-open-at-point-global)

;; ------------------
;; Link abbreviations
;; ------------------

;;; Here is the syntax to use link abbreviations in org-mode:
;;; [[linkword:tag][description]], where liknword must be a word

(setq org-link-abbrev-alist
      '(("gg" . "http://www.google.com/search?q=")
	("gmap"	  . "http://maps.google.com/maps?q=%s")
	("bib" . "~/Documents/mydocs/references/dissert.bib::%s")))

;; ==========
;; TODO items
;; ==========
;; -----------
;; TODO states
;; -----------
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	(sequence "WRITE(w)" "REVISE(r)" "PROOF-READ(p)" "SUBMITTED(s)" "|" "DONE(d)")
	(type "FIXME(f)" "BUG(b)" "LEARN(l)" "|" "FIXED(F)" "DONE(d)")))

;; Description: "sequence" is used to indicate that the TODO items
;; form a sequence. In contrast, with "type" we simply have different
;; states without forming a sequence. The vertical bar separates the
;; TODO keywords from the DONE state. 

;; -----
;; faces
;; -----
(setq org-todo-keyword-faces
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

;; -----------------
;; todo dependencies
;; -----------------

(setq org-enforce-todo-dependencies t)
;; Description: undone TODO entries will block switching the parent to
;; DONE.

;; ====
;; tags
;; ====
;;; Tags are useful to filter your todo list.
(setq org-tag-alist '(("PROJECT" . ?p)
		      ("SideProjects" . ?s)
		      ("@CURRENT" . ?c)
		      ("@lineup" . ?l)
		      ("@WAITING" . ?w)
		      ("LitReview" . ?L)
		      ("writing" . ?W)
		      ("reading" . ?R)
		      ("organizing" . ?O)
		      ("email" . ?e)))

(setq org-tags-exclude-from-inheritance '("@CURRENT" "@lineup" "PROJECT" "@WAITING"))
;; These are the tags that I don't want to be inherited.

;; ======
;; Agenda
;; ======
(global-set-key "\C-ca" 'org-agenda)

;; ------------
;; Agenda files
;; ------------
(setq org-agenda-files (list "~/elisp/agenda/ag-geral.org"
			     "~/elisp/agenda/ag-academic.org"
			     "~/elisp/agenda/ag-longterm.org"
			     "~/elisp/agenda/ag-it.org"
			     "~/elisp/agenda/ag-teaching.org"))


;;; agenda dispatcher
(setq org-agenda-custom-commands
      '(("h" "Coisas pra hoje"
	 ((tags "@TODAY")
	  (agenda "" ((org-agenda-ndays 1)
		      (org-agenda-show-all-dates nil)))))
	("d" "Coisas pra fazes outro dia"
	 ((tags "PROJECT")
	  (tags "@TODAY")
	  (tags "@lineup")
	  (tags "SideProjects")
	  (agenda "" ((org-agenda-ndays 13)
		      (org-agenda-show-all-dates nil)))))
	("A" "Monthly schedule" agenda "" ((org-agenda-ndays 21)
			(org-agenda-show-all-dates nil)))
	("c" "Today schedule" agenda "" ((org-agenda-ndays 1)
					 (org-agenda-overriding-header "Today")))))

;; Description: the second parameter is the search type, followed by
;; the regexp to be matched. Use tags-todo to limit your search to
;; headlines that match both a tag *and* a TODO item.

;; ===========
;; column view
;; ===========
(setq org-columns-default-format "%1PRIORITY %5TODO %20ITEM(Task) %10SCHEDULED %10Effort(Effort){:} %10CLOCKSUM %TAGS")

;; ========
;; clocking
;; ========
(setq org-clock-into-drawer t)

;; --------------
;; Task estimates
;; --------------

;; global Effort estimate values
(setq org-global-properties '(("Effort_ALL" . "1:00 2:00 3:00 4:00 0:15 0:30 0:45")))

;; ----------------
;; Progress logging
;; ----------------

(setq
 org-log-done 'time
 ;; org-log-done 'note
 )

;; Description: add a timestamp and a note to a closed item

(setq org-clock-idle-time nil)
;; Emacs alert you after the computer is idle for X mins.

;; -------
;; Capture
;; -------
(global-set-key "\C-cc" 'org-capture)
(setq org-directory "~/elisp/agenda/")
(setq org-default-notes-file (concat org-directory "/ag-it.org"))

(setq org-capture-templates
      '(("f" "FIXME" entry (file+headline "~/elisp/agenda/ag-it.org" "GENERAL FIXES")
	 "** FIXME %?\n  %i\n \n")
	("l" "LEARN" entry (file+headline "~/elisp/agenda/ag-it.org" "GENERAL FIXES")
	 "** LEARN %?\n %i\n \n")
	("g" "Things to do" entry (file "~/elisp/agenda/ag-geral.org")
	 "* TODO \n %a \n")))

;; Description: the first string is the key to reach the template, the
;; second is a short description. Then follows the type of the entry
;; and a definition of the target location for storing the note.
;; Finally, the template itself, a string with %-scapes to fill in
;; information based on time and context. Here are some options:
;; %a the link created with org-store-link
;; %i the region when remember is called with C-u
;; %t timestamp (date only)
;; %T timestamp (date + time)
;; %u, %U like above but inactive.

;; =============
;; inline images
;; =============
(setq org-startup-with-inline-images t)
;;; Non-nil means show inline images when loading a new Org file.

;; =========
;; Exporting
;; =========

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
   (python . t)
   (sh . t)
   (latex . t)
   (R . t)
   (C . t)
   (octave . t)
   (gnuplot . t)))

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(setq org-confirm-babel-evaluate nil)


;; -----------------------
;; system-wide header args
;; -----------------------
(setq org-babel-default-header-args
      (cons '(:results . "output replace")
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
(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (progn
         (reftex-parse-all)
	 ;; add a custom reftex cite format to insert links
         (reftex-set-cite-format
          '((?b . "[[bib:%l][%l-bib]]")
            (?t . "[[bib:%l][%a (%y)]]")
	    (?h . "+ [[bib:%l][%a %y %t]]\n  ")))))

  (define-key org-mode-map (kbd "C-c r") 'reftex-citation))

;;; Notes:
;;; 1. using load-library instead of (reftex-mode t) is good because
;;;    in that way org-mode keybindings are not overwritten by the
;;;    reftex mode -- e.g., C-c ] which is bound to org-remove-file.
;;; 2. this code will only work after I load my auctex config
;;;    (otherwise reftex won't load my default bib file.

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

;; -------
;; preview
;; -------
(setq org-format-latex-options '(:foreground default :background
					     default :scale
					     1.5 :html-foreground "Black" :html-background "Transparent" :html-scale
					     1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

(setq org-latex-create-formula-image-program 'dvipng)

;; ;; ============
;; ;; org-annotate
;; ;; ============
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

;; ===========
;; org-notmuch
;; ===========
(require 'org-notmuch)

;; =====
;; timer
;; =====
(setq org-timer-default-timer 25)

;; (add-hook 'org-clock-in-hook '(lambda () 
;; 				(if (not org-timer-current-timer) 
;; 				    (org-timer-set-timer '(16)))))

;; (add-hook 'org-clock-out-hook '(lambda () 
;; 				 (setq org-mode-line-string nil)))
;;; I don't know if the previous add-hook is necessary.

(add-hook 'org-timer-done-hook
	  '(lambda()
	     (message "END OF TIME BURST!!")))

(global-set-key (kbd "\e\ec") '(lambda ()
				 (interactive)
				 (let ((current-prefix-arg '(4)))
				   (call-interactively 'org-clock-in))))

(global-set-key (kbd "\e\er") 'org-clock-out)
(global-set-key (kbd "\e\eh") 'org-clock-goto)

(defun timer-do-org ()
  (interactive)
  (set-buffer "clocktable.org")
  (org-timer-set-timer))
(global-set-key (kbd "\e\eg") 'timer-do-org)

;; ======
;; quotes
;; ======
(add-hook 'org-mode-hook (lambda ()
			   (key-chord-define org-mode-map "''"  "`'\C-b")))

;; ========
;; org-mime
;; ========
(require 'org-mime)

(add-hook 'message-mode-hook
          (lambda ()
            (local-set-key "\C-c\M-o" 'org-mime-htmlize)))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\C-c\M-o" 'org-mime-org-buffer-htmlize)))

;; Preferred applications
(setq org-file-apps (quote ((auto-mode . emacs) 
			    ("\\.x?html?\\'" . default)
			    ("\\.pdf\\'" . "/usr/bin/zathura %s"))))

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
(find-file-noselect "~/Dropbox/shared-files/wasteclock.org")

;;;###autoload
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
    (clock-wasteclock)))

(global-set-key (kbd "\e\e w") 'goto-wasteclock)

;; --------------
;; clocktable.org
;; --------------
(find-file-noselect "~/Dropbox/shared-files/clocktable.org")

;;;###autoload
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
      (beginning-of-buffer)
      (outline-next-visible-heading 2)
      (org-tree-to-indirect-buffer)
      (split-window-right)
      (outline-next-visible-heading -1)
      (org-tree-to-indirect-buffer '4)
      (delete-window)
      (read-key "Press any key to exit.")
      (kill-buffer "clocktable.org-1")
      (kill-buffer "clocktable.org-2"))))

(global-set-key (kbd "\e\e m") 'chama-clock-table)

(provide 'mk_orgmode-setup)