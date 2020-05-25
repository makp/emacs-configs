;;; mk_python.el --- Custom config for python mode

;;; Commentary:

;; TODO: add support for jupyter or ipython.

;;; Code:


;; Enable elpy-mode
(elpy-enable)

;; when starting python shells, don't use project root as default dir
(setq elpy-shell-use-project-root nil)

(provide 'mk_python)
;;; mk_python.el ends here
