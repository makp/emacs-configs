
;; Use elpy-mode
(elpy-enable)

;; Use flycheck instead of flymake
;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

(provide 'mk_python)
