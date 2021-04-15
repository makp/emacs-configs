;;; init.el --- Custom init -*- lexical-binding: t -*-

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
(require 'mk_magit)
(require 'mk_orgmode-setup)
(require 'mk_evil)
(require 'mk_mode-line)
(require 'mk_leader-keybindings)
(require 'mk_keybindings)
(require 'mk_buffers-and-windows)
(require 'mk_session-management)
(require 'mk_company)
(require 'mk_yasnippet-setup)


;; =================
;; eval-after-load's
;; =================
(with-eval-after-load 'dired
  (require 'mk_dired))
(with-eval-after-load 'cc-mode
  (require 'mk_cc))
(with-eval-after-load 'elisp-mode
  (require 'mk_elisp))
(with-eval-after-load 'tex-mode
  (require 'mk_latex))
(with-eval-after-load 'python
  (require 'mk_python))
(with-eval-after-load 'web-mode
  (require 'mk_web-devel))
(with-eval-after-load 'wolfram-mode
  (require 'mk_wolfram))

;; =========
;; autoloads
;; =========
(autoload 'mk/search-web "mk_eww-search-engine"
  "Select a search engine before running EWW." t)
(autoload 'ibuffer "mk_ibuffer-setup")
(autoload 'mk/call-ansi-term "mk_ansi-term")

;; =====
;; extra
;; =====
(global-set-key (kbd "<f5>")
		'(lambda ()
		   (interactive)
		   (require 'mk_keyfreq)
		   (pdf-tools-install)))


(provide 'init)
;;; init.el ends here
