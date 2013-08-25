;;; windows

;; ===========
;; winner-mode
;; ===========
(setq winner-boring-buffers '("*Completions*"
			      "*Compile-Log*"
			      "*inferior-lisp*"
			      "*Fuzzy Completions*"
			      "*Apropos*"
			      "*dvc-error*"
			      ;; "*Help*"
			      "*cvs*"
			      "*Buffer List*"))

(when (require 'winner)
  (defvar winner-boring-buffers-regexp
    "\*[hH]elm.*\\|\*xhg.*\\|\*xgit.*")
  (defun winner-set1 (conf)
    ;; For the format of `conf', see `winner-conf'.
    (let* ((buffers nil)
	   (alive
	    ;; Possibly update `winner-point-alist'
	    (loop for buf in (mapcar 'cdr (cdr conf))
		  for pos = (winner-get-point buf nil)
		  if (and pos (not (memq buf buffers)))
		  do (push buf buffers)
		  collect pos)))
      (winner-set-conf (car conf))
      (let (xwins)                      ; to be deleted

	;; Restore points
	(dolist (win (winner-sorted-window-list))
	  (unless (and (pop alive)
		       (setf (window-point win)
			     (winner-get-point (window-buffer win) win))
		       (not (or (member (buffer-name (window-buffer win))
					winner-boring-buffers)
				(string-match winner-boring-buffers-regexp
					      (buffer-name (window-buffer win))))))
	    (push win xwins)))          ; delete this window

	;; Restore marks
	(letf (((current-buffer)))
	  (loop for buf in buffers
		for entry = (cadr (assq buf winner-point-alist))
		do (progn (set-buffer buf)
			  (set-mark (car entry))
			  (setf (winner-active-region) (cdr entry)))))
	;; Delete windows, whose buffers are dead or boring.
	;; Return t if this is still a possible configuration.
	(or (null xwins)
	    (progn
	      (mapc 'delete-window (cdr xwins)) ; delete all but one
	      (unless (one-window-p t)
		(delete-window (car xwins))
		t))))))
  (defalias 'winner-set 'winner-set1))

(setq winner-dont-bind-my-keys t)

(winner-mode 1)

(global-set-key (kbd "\e\ep") 'winner-undo)
(global-set-key (kbd "\e\en") 'winner-redo)

;; --------------
;; Sticky windows
;; --------------
(global-set-key (kbd "C-x 9") 
		(lambda ()
		  (interactive)
		  (message "Sticky window")
		  (set-window-dedicated-p (selected-window) (not current-prefix-arg))))
;; Note: a prefix argument reverts the operation (not really). Does
;; the situation change if I define a separate function (like in the
;; emacswiki page)?


(defun sticky-window-delete-other-windows ()
  "Delete all other windows that are not marked to be visible
with `sticky-window-keep-window-visible'. This is intended as a
replacement for delete-other-windows"
  (interactive)
  (mapcar (lambda (window)
	    (if (not (window-dedicated-p window))
		(delete-window window)))
	  (cdr (window-list))))
;; TODO: rewrite it so that I use a prefix argument to overwrite `C-x
;; 9'. A simpler way of doing that is by calling the regular
;; delete-other-windown whet the prefix argument is used.

;; -----------------------
;; Transposing two buffers
;; -----------------------
(global-set-key (kbd "\e\es") 'transpose-buffers)

(defun transpose-buffers (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;; --------------------
;; toggle windows split
;; --------------------
(global-set-key (kbd "C-x |") 'toggle-window-split)

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))


;; ----------------------
;; moving between windows
;; ----------------------
;; (setq windmove-wrap-around t)

;; (global-set-key (kbd "C-S-b") 'windmove-left)
;; (global-set-key (kbd "C-S-f") 'windmove-right)
;; (global-set-key (kbd "C-S-n") 'windmove-down)
;; (global-set-key (kbd "C-S-t") 'windmove-up)

;; FIXME: I should choose different keybindings because that conflicts
;; with org-mode keybindings


(provide 'mk_windows-setup)
