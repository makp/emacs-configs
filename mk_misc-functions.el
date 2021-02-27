(defun mk/unfill-paragraph ()
  "Transform a multi-line paragraph into a single line."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(provide 'mk_misc-functions)
