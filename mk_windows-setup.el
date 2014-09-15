;;; windows and buffers

;; ===========
;; winner-mode
;; ===========
;; (setq winner-boring-buffers '("*Completions*"
;; 			      "*Compile-Log*"
;; 			      "*inferior-lisp*"
;; 			      "*Fuzzy Completions*"
;; 			      "*Apropos*"
;; 			      "*dvc-error*"
;; 			      ;; "*Help*"
;; 			      "*cvs*"
;; 			      "*Buffer List*"))

;; (when (require 'winner)
;;   (defvar winner-boring-buffers-regexp
;;     "\*[hH]elm.*\\|\*xhg.*\\|\*xgit.*")
;;   (defun winner-set1 (conf)
;;     ;; For the format of `conf', see `winner-conf'.
;;     (let* ((buffers nil)
;; 	   (alive
;; 	    ;; Possibly update `winner-point-alist'
;; 	    (loop for buf in (mapcar 'cdr (cdr conf))
;; 		  for pos = (winner-get-point buf nil)
;; 		  if (and pos (not (memq buf buffers)))
;; 		  do (push buf buffers)
;; 		  collect pos)))
;;       (winner-set-conf (car conf))
;;       (let (xwins)                      ; to be deleted

;; 	;; Restore points
;; 	(dolist (win (winner-sorted-window-list))
;; 	  (unless (and (pop alive)
;; 		       (setf (window-point win)
;; 			     (winner-get-point (window-buffer win) win))
;; 		       (not (or (member (buffer-name (window-buffer win))
;; 					winner-boring-buffers)
;; 				(string-match winner-boring-buffers-regexp
;; 					      (buffer-name (window-buffer win))))))
;; 	    (push win xwins)))          ; delete this window

;; 	;; Restore marks
;; 	(letf (((current-buffer)))
;; 	  (loop for buf in buffers
;; 		for entry = (cadr (assq buf winner-point-alist))
;; 		do (progn (set-buffer buf)
;; 			  (set-mark (car entry))
;; 			  (setf (winner-active-region) (cdr entry)))))
;; 	;; Delete windows, whose buffers are dead or boring.
;; 	;; Return t if this is still a possible configuration.
;; 	(or (null xwins)
;; 	    (progn
;; 	      (mapc 'delete-window (cdr xwins)) ; delete all but one
;; 	      (unless (one-window-p t)
;; 		(delete-window (car xwins))
;; 		t))))))
;;   (defalias 'winner-set 'winner-set1))

(setq winner-dont-bind-my-keys t)

(winner-mode 1)

(global-set-key (kbd "C-x p") 'winner-undo)
(global-set-key (kbd "\e\ep") 'winner-undo)
(global-set-key (kbd "\e\en") 'winner-redo)

;; --------------
;; Sticky windows
;; --------------

;;;###autoload
(defun sticky-window-keep-window-visible ()
  "Insure the buffer associated with the current window stays visible.
This is handy for ERC buffers where you would like to see the
conversation while you work in other windows within the frame. 
This is intended to be used with `sticky-window-delete-window'.
A prefix arg reverses this operation."
  (interactive)
  (set-window-dedicated-p (selected-window) (not current-prefix-arg)))

;;;###autoload
(defun sticky-window-delete-window ()
  "This is intended to be a replacement for `delete-window', but
that avoids deleting windows that have been marked as dedicated
with `sticky-window-keep-window-visible'."
  (interactive)
  (let ((window (selected-window)))
	(if (and (not current-prefix-arg) (window-dedicated-p window))
		(error "This is a dedicated window. Use C-u prefix on this keybinding to really delete it.")
	  (set-window-dedicated-p (selected-window) nil)
	  (delete-window window))))

;;;###autoload
(defun sticky-window-delete-other-windows ()
  "Delete all other windows that are not marked to be visible
with `sticky-window-keep-window-visible'."
  (interactive)
  (mapcar (lambda (window)
			(if (not (window-dedicated-p window))
				(delete-window window)))
		  (cdr (window-list))))

(global-set-key (kbd "C-x C-d") 'sticky-window-keep-window-visible)

(global-set-key (kbd "C-x C-l") 'nil)
(global-set-key (kbd "C-x C-S-l") 'downcase-region)
(global-set-key (kbd "C-x C-l") 'sticky-window-delete-other-windows)

(global-set-key (kbd "C-x <SPC>") 'delete-other-windows)

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
(global-set-key (kbd "\e\e |") 'toggle-window-split)

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


(global-set-key (kbd "C-x C-n") 'bury-buffer)
(global-set-key (kbd "C-x C-;") 'set-goal-column)

(global-set-key (kbd "C-x n") 'delete-window)

;; ----------------------
;; moving between windows
;; ----------------------
;; (setq windmove-wrap-around t)

;; (global-set-key (kbd "") 'windmove-right)
;; (global-set-key (kbd "") 'windmove-left)
;; (global-set-key (kbd "") 'windmove-down)
;; (global-set-key (kbd "") 'windmove-up)

(provide 'mk_windows-setup)
