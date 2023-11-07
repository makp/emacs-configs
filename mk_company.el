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
(setq company-minimum-prefix-length 1
      company-tooltip-align-annotations t
      company-files-exclusions '(".git/")
      company-idle-delay 0.0)
;; (setq company-dabbrev-downcase nil) ;; Don't downcase completion candidates


;; ========
;; Backends
;; ========
;; Key var: `company-backends'.
;; Note that only one backend is used at a time, but a backend can be
;; grouped into a list. One neat feature about company is that you can
;; interactively call separate backends.

(require 'company-auctex)

;; Globally activate unicode symbol completion
;; NOTE: This backend is not active in latex math envs. For LaTeX, use
;; company-math-symbols-latex.
(add-to-list 'company-backends 'company-math-symbols-unicode)


(defvar backends-for-tex
  '((company-math-symbols-latex ; from `company-math'
     company-latex-commands)
    (company-auctex-macros
     company-auctex-symbols
     company-auctex-environments)
    (company-auctex-labels)
    (company-auctex-bibs))
  "Backends provided by `company-math' and `company-auctex'.")


(defun mk/enable-tex-backends()
  "Enable `company-math' and `company-auctex' backends."
  (setq-local company-backends
	      (append backends-for-tex company-backends)))


(provide 'mk_company)
;;; mk_company.el ends here
