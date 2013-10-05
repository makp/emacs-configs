;; "-*- emacs-lisp -*-"
;; Main Emacs config file

(require 'cl)

;; ====================
;; Separate custom file
;; ====================
;; TODO

;; =========
;; Load path
;; =========
(add-to-list 'load-path "~/elisp/")

(add-to-list 'load-path "~/config-files/general/emacs-config/")

;; (let ((default-directory "~/.emacs.d/elpa/"))
;;   (normal-top-level-add-to-load-path '("."))
;;   (normal-top-level-add-subdirs-to-load-path))

(let ((default-directory "~/elisp/bin/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; ========
;; Packages
;; ========
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)
;; check if the packages is installed; if not, install it.
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package)) 
           (package-install package))))
 '(magit helm helm-ls-git ace-jump-mode auto-complete autopair gnuplot-mode yasnippet undo-tree multiple-cursors keyfreq highlight-parentheses region-bindings-mode dropdown-list zenburn-theme ac-math paredit popup emms w3m ess))

;; ===========
;; color theme
;; ===========
(load-theme 'zenburn t)

;; ===========
;; core config
;; ===========
(require 'mk_mode-line)
(require 'mk_better-defaults)
(require 'mk_dvorak-mode)
(require 'mk_registers)
(require 'mk_helm-setup)
(require 'mk_windows-setup)
(require 'mk_dired)
(require 'mk_session-management)
(require 'mk_ibuffer-setup)
(require 'mk_magit)
(require 'mk_eshell)
(require 'mk_misc-functions)

;; =======
;; modules
;; =======
;; -------------------------
;; mark + yas + autocomplete
;; -------------------------
(global-set-key (kbd "<f6>")
		'(lambda ()
		   (interactive)
		   (require 'mk_mark-setup)
		   (require 'mk_yasnippet-setup)
		   (require 'mk_autocomplete-setup)
		   (require 'mk_elisp)
		   (require 'mk_keyfreq)
		   (require 'mk_emms-setup)))

;; -----------------------------------------------
;; org-mode, latex, and the some programming modes
;; -----------------------------------------------
(global-set-key (kbd "<f7>")
		'(lambda ()
		   (interactive)
		   (require 'mk_orgmode-setup) ; (eval-after-load "org" ')
		   (require 'mk_mobileorg)
		   (require 'mk_emacsw3m)
		   (require 'gnuplot)
		   (require 'mk_ess)
		   (require 'mk_latex-setup) ; (eval-after-load "tex-mode" ')
		   (require 'mk_eldoc)))		      
 
;; -----------
;; Email + ERC
;; -----------
(global-set-key (kbd "<f8>")
		'(lambda ()
		   (interactive)
		   (message ">>>>> Loading my email + erc config <<<<<<<<")
		   (setq user-full-name "Makmiller Pedroso")
		   (setq user-mail-address "makmiller@gmail.com")
		   (require 'mk_email) ; (autoload 'notmuch "notmuch" "notmuchm mail" t)
		   (require 'mk_message-mode)
		   (require 'mk_msmtp)
		   (require 'mk_erc)
		   (message ">>>>> End of my email + erc config <<<<<<<<")))

;; =================
;; eval-after-load's
;; =================
(eval-after-load 'cc-mode '(require 'mk_cc))

;;; end of init.el
