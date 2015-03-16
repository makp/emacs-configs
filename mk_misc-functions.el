;; ================
;; helper functions
;; ================

;;;###autoload
(defun mk/add-something-to-hooks (mode-list something)		       
  "Helper function to add a callback to multiple hooks.
For instance: (mk/add-something-to-hooks '(lisp emacs-lisp) 'do-X)."
  (dolist (mode mode-list)
    (add-hook (intern (concat (symbol-name mode) "-mode-hook")) something)))

;; =======
;; editing
;; =======
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (if (region-active-p)
      (duplicate-region arg)
    (duplicate-current-line arg)))

(defun duplicate-region (num &optional start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used. Adds the duplicated text to the kill ring."
  (interactive "p")
  (let* ((start (or start (region-beginning)))
         (end (or end (region-end)))
         (region (buffer-substring start end)))
    (kill-ring-save start end)
    (goto-char end)
    (dotimes (i num)
      (insert region))))

(defun duplicate-current-line (num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (duplicate-region num (point-at-bol) (1+ (point-at-eol)))
  (goto-char (1- (point))))


;; ===========================
;; ediff marked files in dired
;; ===========================
;;;###autoload
(defun dired-ediff-marked-files ()
  "Run ediff on marked ediff files."
  (interactive)
  (set 'marked-files (dired-get-marked-files))
  (when (= (safe-length marked-files) 2)
    (ediff-files (nth 0 marked-files) (nth 1 marked-files)))
  
  (when (= (safe-length marked-files) 3)
    (ediff3 (buffer-file-name (nth 0 marked-files))
            (buffer-file-name (nth 1 marked-files)) 
            (buffer-file-name (nth 2 marked-files)))))

;; ---------------------------
;; edit region indirect buffer
;; ---------------------------
(defvar indirect-mode-name nil
  "Mode to set for indirect buffers.")
(make-variable-buffer-local 'indirect-mode-name)

;;;###autoload
(defun indirect-region (start end) 
  "Edit the current region in another buffer. If the buffer-local
variable `indirect-mode-name' is not set, prompt for mode name to
choose for the indirect buffer interactively. Otherwise, use the
value of said variable as argument to a funcall."
  (interactive "r")
  (let ((buffer-name (generate-new-buffer-name "*indirect*"))
	(mode
	 (if (not indirect-mode-name)
	     (setq indirect-mode-name
		   (intern
		    (completing-read 
		     "Mode: "
		     (mapcar (lambda (e) 
			       (list (symbol-name e)))
			     (apropos-internal "-mode$" 'commandp))
		     nil t)))
	   indirect-mode-name)))
    (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
    (funcall mode)
    (narrow-to-region start end)
    (goto-char (point-min))
    (shrink-window-if-larger-than-buffer)))

;; =======================
;; cycling thru diff fonts
;; =======================
;;;###autoload
(defun cycle-font (num)
  "Change font in current frame.
Each time this is called, font cycles thru a predefined set of fonts.
If NUM is 1, cycle forward. If NUM is -1, cycle backward"
  (interactive "p")

  ;; this function sets a property “state”. It is a integer. Possible values are any index to the fontList.
  (let (fontList fontToUse currentState nextState )
    (setq fontList (list
                    "Monospace-10" "Monaco-10" "Inconsolata-12" "DejaVu Sans Mono-10"))
    (setq currentState (if (get 'cycle-font 'state) (get 'cycle-font 'state) 0))
    (setq nextState (% (+ currentState (length fontList) num) (length fontList)))

    (setq fontToUse (nth nextState fontList))
    (set-frame-parameter nil 'font fontToUse)
    (redraw-frame (selected-frame))
    (message "Current font is: %s" fontToUse )

    (put 'cycle-font 'state nextState)))

(defun cycle-font-foward ()
  "Switch to the next font, in the current frame.
See `cycle-font'."
  (interactive)
  (cycle-font 1))

;; ==============================
;; cycling thru interline spacing
;; ==============================
;;;###autoload
(defun toggle-line-spacing () 
  "Toggle between different line spacing"
  (interactive)
  (setq contador (1+ contador)) ; increment
  (if (< contador comprimento)
      (progn 
	(setq valor (nth contador lista-espacos))
	(setq line-spacing valor)
	(message "The line spacing is %d %% of normal line height" (* line-spacing 100)))
    (progn 
      (setq contador 0)                   ; reseting the counter 
      (setq line-spacing nil)		  ; 
      (message "Normal line spacing"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mk/run-str-in-eshell (str)
  "Inserts and run a string in an Eshell buffer"
  (interactive)
  (insert str)
  (eshell-send-input nil t t))

;;;###autoload 
(defun mk/update-git-dirs-eshell ()
  "Update git dirs using eshell."
  (interactive)
  (let ((lst-files (nthcdr 2 (directory-files "."))))
    (dolist (x lst-files)
      (mk/run-str-in-eshell (concat "cd " x "\n"))
      (mk/run-str-in-eshell "git pull \n")
      (read-char)
      (mk/run-str-in-eshell  "cd .. \n")
      (message "ALL DONE!!"))))

;;;###autoload
(defun mk/recursive-directories (dir)
  "Return all directories below DIR."
  (let (directs)
    (dolist (pdir (directory-files dir t))  ; return absolute paths
      (when (and (not (member (file-name-nondirectory pdir) '("." "..")))
		 (file-directory-p pdir))
	(push pdir directs)
	(setq directs (append (mk/recursive-directories pdir) directs))))
    directs))

;; The gnus-recursive-directory-files from gnu-util.el, which this
;; function is based on, provides an equivalent feature for files
;; (i.e., it returns all files below DIR.

;; ------------------------
;; better beginning-of-line
;; ------------------------
;;;###autoload
(defun mk/smarter-beginning-of-line ()
  "Move point to beginning-of-line or first non-whitespace
character or first non-whitespace after a comment sign."
  (interactive "^")
  (let ((oldpos (point))
	(indentpos (progn
		     (back-to-indentation)
		     (point)))
	(textpos (progn
		   (beginning-of-line-text)
		   (point))))
    (cond
     ((> oldpos textpos) (beginning-of-line-text))
     ((and (<= oldpos textpos) (> oldpos indentpos)) (back-to-indentation))
     ((and (<= oldpos indentpos) (> oldpos (line-beginning-position))) (beginning-of-line))
     (t (beginning-of-line-text)))))

;;;###autoload
(defun mk/unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; ------------------
;; emacs config files
;; ------------------
;;;###autoload
(defun mk/emacs-config-files ()
  (interactive)
  (save-excursion
    (cd "~/config-files/general/emacs-configs/")
    (call-interactively 'helm-find-files)))

(provide 'mk_misc-functions)
