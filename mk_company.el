;;; mk_company.el --- Custom config for Emacs company mode.

;;; Commentary:

;; 

;;; Code:


;; Enable `company-mode' in all buffers
(add-hook 'after-init-hook 'global-company-mode)

;; (setq company-global-modes '(emacs-lisp-mode python-mode))

;; company-box
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)


;; ================
;; company settings
;; ================

;; Don't ignore case when completing candidates
;; (setq company-dabbrev-ignore-case nil)

;; Don't downcase completion candidates
(setq company-dabbrev-downcase nil)

;; (setq company-idle-delay 0.25)
;; (setq company-minimum-prefix-length 3)


;; ========
;; Backends
;; ========
;; Key var: `company-backends'.
;; Note that only one backend is used at a time, but a backend can be
;; grouped into a list.  One neat feature about company is that you
;; can interactively call separate backends.

;; Add backend for LaTeX files
(defvar mk/enable-tex-backend nil
  "Enable backend in `tex-mode'.")

;; A couple notes about the engines below:
;; - `company-auctex-symbols' is a very neat engine in that it allows
;; insertion of symbols outside of math mode.  Because of this,
;; `company-auctex-symbols' seems to be more versatile than
;; `company-math-symbols-latex'.
;; - `company-auctex-bibs' engine relies on `LaTeX-bibitem-list' but
;; this var is nil in my tex files.

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


(provide 'mk_company)

;;; mk_company.el ends here
