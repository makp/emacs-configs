;;; mk_yasnippet-setup.el --- Custum setup for Yasnippets -*- lexical-binding: t -*-

;;; Commentary:

;; 

;;; Code:


(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))
(setq yas-snippet-dirs '("~/config-files/general/emacs-configs/helper_funcs/my-snippets"))

(require 'yasnippet)
(yas-global-mode 1)

;; supress warning when using backquote expansions
(require 'warnings)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))


;; yas-wrap-around-region
;; If non-nil, snippet expansion wraps around selected region for $0 field.

;; yas-trigger-key
;; The key bound to `yas-expand' when `yas-minor-mode' is active.

;; yas-indent-line 'fixed
;; This variable controls indenting. The default value, "auto",
;; causes your snippet to be indented according to the mode the
;; buffer it was inserted in.

;; mode-require-final-newline
;; Var controls whether to add a new line at the end of the file

;; yas-prompt-functions

;;; Inter-field navigation
;; (defun yas-goto-end-of-active-field ()
;;   (interactive)
;;   (let* ((snippet (car (yas--snippets-at-point)))
;; 	 (position (yas--field-end (yas--snippet-active-field snippet))))
;;     (if (= (point) position)
;;         (move-end-of-line-or-next-line)
;;       (goto-char position))))

;; (defun yas-goto-start-of-active-field ()
;;   (interactive)
;;   (let* ((snippet (car (yas--snippets-at-point)))
;; 	 (position (yas--field-start (yas--snippet-active-field snippet))))
;;     (if (= (point) position)
;;         (move-start-of-line-or-prev-line)
;;       (goto-char position))))

;; (define-key yas-keymap (kbd "C-e") 'yas-goto-end-of-active-field)
;; (define-key yas-keymap (kbd "C-a") 'yas-goto-start-of-active-field)

(provide 'mk_yasnippet-setup)

;;; mk_yasnippet-setup.el ends here
