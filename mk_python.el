
;; Use elpy-mode
(elpy-enable)

;; elpy uses company instead of auto-complete
(setq ac-modes (delq 'python-mode ac-modes))

;; Use flycheck instead of flymake
;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

(provide 'mk_python)
