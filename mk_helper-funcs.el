;;; package --- Summary
;;; Commentary:
;;; Code:


(defvar mk/helper-funcs-path
  (concat (file-name-directory load-file-name) "helper-funcs/")
  "Store path to helper functions.")


(defun mk/run-python-func-on-text (type py_func)
  "Run Python function PY_FUNC from module TYPE.

Either use the selected text or the clipboard content if no text is selected."
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (gui-get-selection 'CLIPBOARD)))
	 (temp-file (make-temp-file "emacs-content-" nil ".txt"))
	 (python-command (format "from %s import %s; %s(open('%s').read())" type py_func py_func temp-file))
	 (python-output nil)
	 (temp-buffer-name "*Python Output*"))

    ;; Write `text` to a temporary file
    (with-temp-file temp-file
      (insert text))

    ;; Execute the Python command and capture the output
    (setq python-output
	  (with-temp-buffer
	    (let ((default-directory mk/helper-funcs-path))
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


(defun mk/write ()
  "Prompt user to select helper function."
  (interactive)
  (let ((options '("Refine text" "Write short message"))
        (choice nil))
    (while (not (member choice options))
      (setq choice (completing-read "Choose an option: " options)))
    (cond ((string= choice "Write short message")
	   (mk/run-python-func-on-text "writing" "write_short_message"))
          ((string= choice "Refine text")
           (mk/run-python-func-on-text "writing" "refine_text")))))


(provide 'mk_helper-funcs)
;;; mk_helper-funcs.el ends here
