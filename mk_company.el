;;; mk_company.el --- Custom config for Emacs company mode.

;;; Commentary:

;; 

;;; Code:

;; (setq-default company-global-modes '(emacs-lisp-mode python-mode))
;; (global-company-mode)


;; (setq-default company-idle-delay 0.25)
;; (setq-default company-minimum-prefix-length 3)

;; (global-set-key (kbd "<tab>") 'company-complete-common-or-cycle) ;TAB
;; (define-key company-active-map (kbd "TAB") 'company-complete) ;C-i
;; (define-key company-active-map (kbd "C-s") 'nil)
;; (define-key company-active-map (kbd "M-s") 'company-search-candidates)


;; ========
;; Backends
;; ========
;; Key var: `company-backends'.
;; Note that only one backend is used at a time, but a backend can be grouped into a list.
;; One neat feature about company is that you can interactively call separate backends.

;; Add backend for LaTeX files
(defvar mk/enable-tex-backend t
  "Enable backend in `tex-mode'.")

;; A couple notes about the engines below:
;; - `company-auctex-symbols' is a very neat engine in that it allows insertion of symbols outside of math mode.
;; Because of this, `company-auctex-symbols' seems to be more versatile than `company-math-symbols-latex'.
;; - `company-auctex-bibs' engine relies on `LaTeX-bibitem-list' but this var is nil in my tex files.

(defvar backends-for-auctex
  '((company-math-symbols-latex
     ;; company-math-symbols-unicode
     )
    (company-auctex-symbols
     company-auctex-macros
     company-latex-commands
     ;; company-auctex-environments
     )
    (company-auctex-labels)
    ;;(company-auctex-bibs)
    )
  "Backends provided by `company-math' and `company-auctex'.")

(when mk/enable-tex-backend
  (setq company-backends (append backends-for-auctex company-backends)))


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
