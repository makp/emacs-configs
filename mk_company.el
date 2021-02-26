;;; mk_company.el --- Custom config for Emacs company mode.

;;; Commentary:

;; 

;;; Code:

(setq-default company-global-modes '(text-mode emacs-lisp-mode LaTeX-mode python-mode))
(global-company-mode)

;; For some reason company doesn't load with `company-global-modes'
(add-hook 'LaTeX-mode-hook
	  'company-mode)

;; (setq-default company-idle-delay 0.25)
;; (setq-default company-minimum-prefix-length 3)

;; (global-set-key (kbd "<tab>") 'company-complete-common-or-cycle) ;TAB
;; (define-key company-active-map (kbd "TAB") 'company-complete) ;C-i
;; (define-key company-active-map (kbd "C-s") 'nil)
;; (define-key company-active-map (kbd "M-s") 'company-search-candidates)
;; (define-key company-active-map (kbd "C-m") nil)


;; ========
;; Backends
;; ========
;; Var: `company-backends'.
;; Note that only one backend is used at a time, but a backend can be grouped into a list.
;; One neat feature about company is that you can interactively call separate backends.

;; Add backend for LaTeX files
(defvar mk/enable-tex-backend t
  "Enable backend in `tex-mode'.")

(defvar backends-for-auctex
  '(company-math-symbols-latex
    company-math-symbols-unicode
    company-latex-commands
    ;; company-auctex-labels
    ;; company-auctex-bibs
    ;; company-auctex-macros
    ;; company-auctex-symbols
    ;; company-auctex-environments
    )
  "Backends provided by `company-math' and `company-auctex'.")

(when mk/enable-tex-backend
  (setq company-backends (append (list backends-for-auctex) company-backends)))


;; Add yasnippet to every backend
(defvar mk/enable-yas-every-backend t
  "Enable yasnippet for all backends.")

(defun mk/add-yas-to-backend (backend)
  "Add `company-yasnippet' to BACKEND.
From https://github.com/syl20bnr/spacemacs/pull/179"
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))


(defun mk/add-yas-to-every-backend ()
  "Add `company-yasnippet' to every backend in `company-backends'."
  (when mk/enable-yas-every-backend
    (setq company-backends
	  (mapcar #'mk/add-yas-to-backend company-backends))))

(mk/add-yas-to-every-backend)

(provide 'mk_company)

;;; mk_company.el ends here
