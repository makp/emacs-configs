;;; init.el --- Initialization file

;;; Commentary:

;; 

;;; Code:

;; =========
;; Load path
;; =========
(add-to-list 'load-path "~/elisp/")
(add-to-list 'load-path "~/config-files/general/emacs-configs/")

;; ===========
;; core config
;; ===========
(require 'mk_packages)
(require 'mk_mode-line)
(require 'mk_better-defaults)
(require 'mk_helm-setup)
(require 'mk_registers)
(require 'mk_windows-setup)
(require 'mk_buffers-setup)
(require 'mk_session-management)
(require 'mk_ibuffer-setup)
(require 'mk_magit)
(require 'mk_misc-functions)
(require 'mk_mark-setup)
(require 'mk_keybindings-dvorak)


;; =========
;; Autoloads
;; =========
;; Terminals
(global-set-key (kbd "C-x a") 'async-shell-command)

(global-set-key (kbd "C-x C-a") 'mk/eshell-popup)
(autoload 'mk/eshell-popup "mk_eshell" t nil)

(global-set-key (kbd "C-x <RET>") 'mk/chama-ansi-term)
(autoload 'mk/chama-ansi-term "mk_ansi-term" t nil)

;; Dired
(global-set-key (kbd "C-x C-j") 'mk/dired-jump)
(autoload 'mk/dired-jump "mk_dired" t nil)

;; =================
;; eval-after-load's
;; =================
(with-eval-after-load 'cc-mode
  (require 'mk_cc))
(with-eval-after-load 'elisp-mode
  (require 'mk_elisp))
(with-eval-after-load 'tex-mode
  (require 'mk_latex))
(with-eval-after-load 'python-mode
  (require 'mk_python))
(with-eval-after-load 'dired
  (require 'mk_dired))


;; =======
;; modules
;; =======
;; -------------------------
;; mark + yas + autocomplete
;; -------------------------
(global-set-key (kbd "<f5>")
		'(lambda ()
		   (interactive)
		   (require 'mk_company)
		   (require 'mk_yasnippet-setup)
		   (require 'mk_web-devel)))

;; --------------------------------------------
;; org-mode, latex, and other programming modes
;; --------------------------------------------
(global-set-key (kbd "<f6>")
		'(lambda ()
		   (interactive)
		   ;; (require 'mk_chrome)
		   (require 'mk_orgmode-setup)
		   (require 'mk_emacsw3m)))

;; -----------
;; Email + ERC
;; -----------
(global-set-key (kbd "<f7>")
		'(lambda ()
		   (interactive)
		   ;; (require 'mk_autocomplete-setup)
		   (require 'mk_keyfreq)
		   ;; (require 'gnuplot)
		   ;; (require 'mk_ess)
		   ;; (require 'mk_eldoc)
		   ;; (setq user-full-name "Makmiller Pedroso")
		   ;; (setq user-mail-address "makmiller@gmail.com")
		   ;; (require 'mk_email) ; (autoload 'notmuch "notmuch" "notmuchm mail" t)
		   ;; (require 'mk_message-mode)
		   ;; (require 'mk_erc)
		   ;; (require 'mk_emms-setup)
		   ))

;;; init.el ends here
