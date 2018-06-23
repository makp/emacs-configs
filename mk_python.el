;;; mk_python.el --- Custom config for python mode

;;; Commentary:

;; TODO: add support for jupyter or ipython.

;;; Code:


;; Use elpy-mode
(elpy-enable)

;; elpy uses company instead of auto-complete
;; (setq ac-modes (delq 'python-mode ac-modes))

;; when starting python shells, don't use project root as default dir
(setq-default elpy-shell-use-project-root nil)

;; Use flycheck instead of flymake
;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

(provide 'mk_python)
;;; mk_python.el ends here
