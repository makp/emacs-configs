;;; mk_python.el --- Custom config for python mode

;;; Commentary:

;; TODO: look into a pkg for automatic code formatting -- possibly blacken; ein -- nb within Emacs.

;;; Code:


;; Enable elpy-mode
(elpy-enable)

;; Interpreter setup
;; NOTE: You will need to install jupyter-console with pip
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")

;; default dir with python shells (project-root is the default value)
(setq elpy-shell-starting-directory 'current-directory)


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
