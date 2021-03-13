;;; mk_yasnippet-setup.el --- Custum setup for Yasnippets -*- lexical-binding: t -*-

;;; Commentary:

;; 

;;; Code:


(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))
(setq-default yas-snippet-dirs '("~/config-files/general/emacs-configs/my-snippets"))

(require 'yasnippet)
(yas-global-mode 1)

;; supress warning when using backquote expansions
(require 'warnings)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))


;; yas-trigger-key
;; The key bound to `yas-expand' when `yas-minor-mode' is active.

;; yas-wrap-around-region
;; If non-nil, snippet expansion wraps around selected region for $0 field.

;; yas-indent-line 'fixed
;; This variable controls indenting. The default value, "auto",
;; causes your snippet to be indented according to the mode the
;; buffer it was inserted in.

;; mode-require-final-newline
;; Var controls whether to add a new line at the end of the file

;; yas-prompt-functions

;; --------------------
;; interaction with yas
;; --------------------
;; (add-hook 'org-mode-hook
;; 	  (lambda ()
;; 	    (org-set-local 'yas-trigger-key [tab])
;; 	    (define-key yas-keymap [tab] 'yas-next-field-group)))

;; (defun yas-org-very-safe-expand ()
;;   (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))

;; (add-hook 'org-mode-hook
;; 	  (lambda ()
;; 	    (make-variable-buffer-local 'yas-trigger-key)
;; 	    (setq yas-trigger-key [tab])
;; 	    (add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand)
;; 	    (define-key yas-keymap [tab] 'yas-next-field)))

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
