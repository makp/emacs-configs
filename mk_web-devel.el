(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

(define-key web-mode-map (kbd "C-c C-v") 'browse-url-of-file)

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2) ;HTML
  (setq web-mode-css-indent-offset 3)	 ;CSS
  (setq web-mode-code-indent-offset 2))  ;scriptcode (e.g., JavaScript, PHP, etc)

(add-hook 'web-mode-hook  'my-web-mode-hook)


(provide 'mk_web-devel)