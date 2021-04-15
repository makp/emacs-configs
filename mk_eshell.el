;;; mk_eshell.el --- Config for the eshell

;;; Commentary:

;; 

;;; Code:


(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin
      eshell-review-quick-commands nil
      eshell-smart-space-goes-to-end t)

;; Visual commands
(add-hook 'eshell-mode-hook
	  (lambda ()
	    (add-to-list 'eshell-visual-commands "htop")))

;; (setq eshell-glob-case-insensitive t)

;; (setq eshell-cmpl-cycle-completions nil)
;; Stop with that cycling thing

(setq eshell-list-files-after-cd nil)

;; =============
;; toggle eshell
;; =============
(defvar mk/eshell-popup-buffer nil)

(defun mk/eshell-popup (&optional arg)
  "Toggle a eshell popup buffer with the current file's directory
as cwd."
  (interactive "P")
  (if (consp arg)
      (pop-to-buffer "*eshell*")
    (unless (buffer-live-p mk/eshell-popup-buffer)
      (save-window-excursion (eshell))
      (setq mk/eshell-popup-buffer (get-buffer "*eshell*")))
    (let ((win (get-buffer-window mk/eshell-popup-buffer))
	  (dir (file-name-directory (or (buffer-file-name)
					;; dired
					dired-directory
					;; use HOME
					"~/"))))
      (if win
	  (quit-window nil win)
	(pop-to-buffer mk/eshell-popup-buffer nil t)
	(insert dir)
	(eshell-send-input)))))

;; ====================
;; completion functions
;; ====================

;; ---------
;; bookmarks
;; ---------
(defun pcomplete/eshell-mode/bmk ()
  "Completion for `bmk'"
  (pcomplete-here (bookmark-all-names)))

(defun eshell/bmk (&rest args)
  "Integration between EShell and bookmarks.
For usage, execute without arguments."
  (setq args (eshell-flatten-list args))
  (let ((bookmark (car args))
        filename name)
    (cond
     ((eq nil args)
      (format "Usage: * bmk BOOKMARK to ** either change
directory pointed to by BOOKMARK ** or bookmark-jump to the
BOOKMARK if it is not a directory. * bmk . BOOKMARK to bookmark
current directory in BOOKMARK. Completion is available."))
     ((string= "." bookmark)
      ;; Store current path in EShell as a bookmark
      (if (setq name (car (cdr args)))
          (progn
            (bookmark-set name)
            (bookmark-set-filename name (eshell/pwd))
            (format "Saved current directory in bookmark %s" name))
        (error "You must enter a bookmark name")))
     (t
      ;; Check whether an existing bookmark has been specified
      (if (setq filename (cdr (car (bookmark-get-bookmark-record bookmark))))
	  ;; If it points to a directory, change to it.
	  (if (file-directory-p filename)
	      (eshell/cd filename)
	    ;; otherwise, just jump to the bookmark 
	    (bookmark-jump bookmark))
	(error "%s is not a bookmark" bookmark))))))

;; --------------
;; git completion
;; --------------
(defun pcmpl-git-commands ()
  "Return the most common git commands by parsing the git output."
  (with-temp-buffer
    (call-process-shell-command "git" nil (current-buffer) nil "help" "--all")
    (goto-char 0)
    (search-forward "available git commands in")
    (let (commands)
      (while (re-search-forward
	      "^[[:blank:]]+\\([[:word:]-.]+\\)[[:blank:]]*\\([[:word:]-.]+\\)?"
	      nil t)
	(push (match-string 1) commands)
	(when (match-string 2)
	  (push (match-string 2) commands)))
      (sort commands #'string<))))

(defconst pcmpl-git-commands (pcmpl-git-commands)
  "List of `git' commands.")

(defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
  "The `git' command to run to get a list of refs.")

(defun pcmpl-git-get-refs (type)
  "Return a list of `git' refs filtered by TYPE."
  (with-temp-buffer
    (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
    (goto-char (point-min))
    (let (refs)
      (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
	(push (match-string 1) refs))
      (nreverse refs))))

(defun pcmpl-git-remotes ()
  "Return a list of remote repositories."
  (split-string (shell-command-to-string "git remote")))

(defun pcomplete/git ()
  "Completion for `git'."
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-git-commands)
  (cond
   ((pcomplete-match "help" 1)
    (pcomplete-here* pcmpl-git-commands))
   ((pcomplete-match (regexp-opt '("pull" "push")) 1)
    (pcomplete-here (pcmpl-git-remotes)))
   ;; provide branch completion for the command `checkout'.
   ((pcomplete-match "checkout" 1)
    (pcomplete-here* (append (pcmpl-git-get-refs "heads")
			     (pcmpl-git-get-refs "tags"))))
   (t
    (while (pcomplete-here (pcomplete-entries))))))

;; ======
;; prompt
;; ======
(require 'vc-git)
(defun fg/eshell-git-info ()
  (let* ((branch (vc-git-working-revision (eshell/pwd))))
    (if (not (string-equal "" branch))
        (concat branch " ")
      "")))

(defun fg/eshell-replace-prompt-prefixes ()
  (let ((absolute-path (eshell/pwd)))
    (cond ((string-match (getenv "HOME") absolute-path)
           (replace-match "~" nil nil absolute-path))
          ((string-match "/ssh:\\(.+\\):" absolute-path)
           (replace-match (concat "@" (match-string 1 absolute-path) " ") nil nil absolute-path))
          (t
           absolute-path))))

(defun fg/eshell-prompt-function ()
  (concat
   (fg/eshell-git-info)
   (fg/eshell-replace-prompt-prefixes)
   "/ "))

(setq eshell-prompt-function #'fg/eshell-prompt-function)
(setq eshell-prompt-regexp "^[^\n]*/ ")


;; =============
;; execute shell
;; =============
;;;###autoload
(defun mk/eshell-execute-current-line ()
  "Insert text of current line in eshell and execute."
  (interactive)
  (require 'eshell)
  (let ((command (buffer-substring
                  (save-excursion
                    (beginning-of-line)
                    (point))
                  (save-excursion
                    (end-of-line)
                    (point)))))
    (let ((buf (current-buffer)))
      (unless (get-buffer eshell-buffer-name)
        (eshell))
      (display-buffer eshell-buffer-name t)
      (switch-to-buffer-other-window eshell-buffer-name)
      (end-of-buffer)
      (eshell-kill-input)
      (insert command)
      (eshell-send-input)
      (end-of-buffer)
      (switch-to-buffer-other-window buf))))

;; (global-set-key (kbd "") 'mk/eshell-execute-current-line)

(provide 'mk_eshell)

;;; mk_eshell.el ends here
