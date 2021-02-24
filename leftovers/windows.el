
;; --------------
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

;; ----------------------
;; moving between windows
;; ----------------------
;; (setq windmove-wrap-around t)

;; (global-set-key (kbd "") 'windmove-right)
;; (global-set-key (kbd "") 'windmove-left)
;; (global-set-key (kbd "") 'windmove-down)
;; (global-set-key (kbd "") 'windmove-up)

;; (require 'ace-window)
;; (setq aw-keys '( ?d ?h ?t ?n ?l ?u ?e ?o))
;; (global-set-key (kbd "") 'ace-window)

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

;; ----------------------
;; moving between windows
;; ----------------------
;; (setq windmove-wrap-around t)

;; (global-set-key (kbd "") 'windmove-right)
;; (global-set-key (kbd "") 'windmove-left)
;; (global-set-key (kbd "") 'windmove-down)
;; (global-set-key (kbd "") 'windmove-up)

;; (require 'ace-window)
;; (setq aw-keys '( ?d ?h ?t ?n ?l ?u ?e ?o))
;; (global-set-key (kbd "") 'ace-window)
