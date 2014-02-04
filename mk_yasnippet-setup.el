;; =========
;; Yasnippet
;; =========
(require 'yasnippet)
(require 'dropdown-list)

(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

(setq yas-snippet-dirs '("~/config-files/general/emacs-config/my-snippets"))

(yas-global-mode 1)

(defun shk-yas/helm-prompt (prompt choices &optional display-fn)
  "Use helm to select a snippet."
  (interactive)
  (setq display-fn (or display-fn 'identity))
  (if (require 'helm-config)
      (let (tmpsource cands result rmap)
	(setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
	(setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
	(setq tmpsource
	      (list
	       (cons 'name prompt)
	       (cons 'candidates cands)
	       '(action . (("Expand" . (lambda (selection) selection))))
	       ))
	(setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
	(if (null result)
	    (signal 'quit "user quit!")
	  (cdr (assoc result rmap))))
    nil))

(setq yas-prompt-functions '(shk-yas/helm-prompt yas-dropdown-prompt))

(setq yas-trigger-key "TAB")
;; The key bound to `yas-expand' when `yas-minor-mode' is active.

(setq yas-indent-line 'fixed)
;; This variable controls indenting. The default value, "auto", causes
;; your snippet to be indented according to the mode the buffer it was
;; inserted in.

(setq yas-wrap-around-region t)
;; If non-nil, snippet expansion wraps around selected region.

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


(setq-default mode-require-final-newline nil)
;; to avoid new lines to be inserted after a yasnippet


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
