;;; mk_web-devel.el --- Custom config for web template editing -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(define-key web-mode-map (kbd "C-c C-v") 'browse-url-of-file)

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq-default web-mode-markup-indent-offset 2) ;HTML
  (setq-default web-mode-css-indent-offset 3)	 ;CSS
  (setq-default web-mode-code-indent-offset 2))  ;scriptcode (e.g., JavaScript, PHP, etc)

(add-hook 'web-mode-hook  'my-web-mode-hook)

(provide 'mk_web-devel)

;;; mk_web-devel.el ends here