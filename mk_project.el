;;; mk_project.el --- Custom config for Projectile -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(require 'projectile)

(projectile-mode +1)

;; Use helm for projectile
(setq projectile-completion-system 'helm)
(require 'helm-projectile)
(helm-projectile-on)


(provide 'mk_project)

;;; mk_project.el ends here
