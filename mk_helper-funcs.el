;;; Code:

(defun mk/run-python-func-on-text (py_func)
  "Run Python function PY_FUNC on either the selected text or the clipboard content if no text is selected."
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (gui-get-selection 'CLIPBOARD)))
	 (temp-file (make-temp-file "emacs-content-" nil ".txt"))
	 (python-command (format "from content_creator import %s; %s(open('%s').read())" py_func py_func temp-file))
	 (python-output nil)
	 (temp-buffer-name "*Python Output*"))

    ;; Write `text` to a temporary file
    (with-temp-file temp-file
      (insert text))

    ;; Execute the Python command and capture the output
    (setq python-output
	  (with-temp-buffer
	    (let ((default-directory (concat default-directory "helper_funcs/")))
	      (call-process "python" nil t nil "-c" python-command))
	    (buffer-string)))

    (delete-file temp-file)

    ;; Display content of `python-output' in a temp buffer
    (with-output-to-temp-buffer temp-buffer-name
      (princ python-output))

    ;; Switch to temp buffer
    (pop-to-buffer temp-buffer-name)

    ;; Make temp buffer editable
    (with-current-buffer temp-buffer-name
      (setq buffer-read-only nil)
      (org-mode))))


(defun mk/create-content()
  (interactive)
  (mk/run-python-func-on-text "teste")) ;; create_content


(provide 'mk_helper_funcs)
;;; mk_helper_funcs.el ends here