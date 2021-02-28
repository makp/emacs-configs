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
(require 'mk_better-defaults)
(require 'mk_helm-setup)
(require 'mk_evil)
(require 'mk_magit)
(require 'mk_orgmode-setup)
(require 'mk_leader-keybindings)
(require 'mk_keybindings)
(require 'mk_registers)
(require 'mk_buffers-and-windows)
(require 'mk_session-management)
(require 'mk_misc-functions)
(require 'mk_mode-line)

;; =====
;; extra
;; =====
(global-set-key (kbd "<f5>")
		'(lambda ()
		   (interactive)
		   (require 'mk_company)
		   (require 'mk_yasnippet-setup)
		   (require 'mk_eww)
		   (require 'mk_web-devel)
		   (require 'mk_keyfreq)
		   (pdf-tools-install)))

;; =================
;; eval-after-load's
;; =================
(with-eval-after-load 'cc-mode
  (require 'mk_cc))
(with-eval-after-load 'elisp-mode
  (require 'mk_elisp))
(with-eval-after-load 'tex-mode
  (require 'mk_latex))
(with-eval-after-load 'python
  (require 'mk_python))
(with-eval-after-load 'dired
  (require 'mk_dired))
(with-eval-after-load 'ibuffer
  (require 'mk_ibuffer-setup))


;; Custom set vars below this line
