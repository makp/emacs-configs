;;; mk_python.el --- Custom config for python mode

;;; Commentary:
;; Run `elpy-config' for a summary of my config.

;;; Code:

;; Enable elpy-mode
(elpy-enable)

;; Dir for conda envs
;; NOTE: Use `pyenv-workon` to select env
(setenv "WORKON_HOME" (expand-file-name "~/.conda/envs/"))

;; ------------------
;; interactive python
;; ------------------

;; Use jupyter console
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")

;; Standard interpreter
;; (setq python-shell-interpreter "python"
;;       python-shell-interpreter-args "-i")


;; Set default dir for python shells (project-root is the default value)
(setq elpy-shell-starting-directory 'current-directory)


;; 
(add-hook 'inferior-python-mode-hook
	  (lambda ()
	    (company-mode 1)))


;; Enable font locking of inputs to python shell
;; From https://elpy.readthedocs.io/en/latest/customization_tips.html#enable-full-font-locking-of-inputs-in-the-python-shell
(advice-add 'elpy-shell--insert-and-font-lock
            :around (lambda (f string face &optional no-font-lock)
                      (if (not (eq face 'comint-highlight-input))
                          (funcall f string face no-font-lock)
                        (funcall f string face t)
                        (python-shell-font-lock-post-command-hook))))

(advice-add 'comint-send-input
            :around (lambda (f &rest args)
                      (if (eq major-mode 'inferior-python-mode)
                          (cl-letf ((g (symbol-function 'add-text-properties))
                                    ((symbol-function 'add-text-properties)
                                     (lambda (start end properties &optional object)
                                       (unless (eq (nth 3 properties) 'comint-highlight-input)
                                         (funcall g start end properties object)))))
                            (apply f args))
                        (apply f args))))

(provide 'mk_python)
;;; mk_python.el ends here
