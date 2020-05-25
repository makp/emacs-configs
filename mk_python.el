;;; mk_python.el --- Custom config for python mode

;;; Commentary:

;; TODO: look into a pkg for automatic code formatting -- possibly blacken; ein -- nb within Emacs.

;;; Code:


;; Enable elpy-mode
(elpy-enable)

;; when starting python shells, don't use project root as default dir
;; (setq elpy-shell-use-project-root nil)

(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")

(provide 'mk_python)
;;; mk_python.el ends here
