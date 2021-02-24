
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


;; ------------------------
;; better beginning-of-line
;; ------------------------
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
