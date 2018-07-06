;;; mk_company.el --- Custom config for Emacs company mode.

;;; Commentary:

;; 

;;; Code:

;; (add-hook 'after-init-hook 'global-company-mode)
(global-company-mode)

(setq-default company-idle-delay 0.25)
(setq-default company-minimum-prefix-length 3)

(global-set-key (kbd "<tab>") 'company-complete-common-or-cycle) ;TAB
(define-key company-active-map (kbd "TAB") 'company-complete) ;C-i
(define-key company-active-map (kbd "C-s") 'nil)
(define-key company-active-map (kbd "M-s") 'company-search-candidates)
(define-key company-active-map (kbd "C-m") nil)


;; ========
;; Backends
;; ========

;; -----------------------
;; Backend for LaTeX files
;; -----------------------

(defun mk/company-tex-backend ()
  "Add company backends provided by company-math for LaTeX math symbols."
  (add-to-list 'company-backends '(company-math-symbols-latex
				   company-math-symbols-unicode
				   company-latex-commands)))

(with-eval-after-load 'tex-mode
  (mk/company-tex-backend)
  (company-auctex-init)) ;company-auctex

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(provide 'mk_company)

;;; mk_company.el ends here
