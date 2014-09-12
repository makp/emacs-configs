;;; helm setup
(require 'helm-config)
(require 'helm-match-plugin)
(helm-mode t)
;; helm-completing-read-handlers-alist controls where we use helm.

(autoload 'helm-ls-git-ls "helm-ls-git" nil t)

;; ====
;; seqs
;; ====
(setq 
 helm-c-external-programs-associations
 '(("pdf" . "okular")
   ("djvu" . "okular")
   ("docx" . "libreoffice")
   ("doc" . "libreoffice")
   ("rtf" . "libreoffice")
   ("svg" . "inkscape"))
  helm-input-idle-delay 0.01
 ;; be idle for this many seconds, before updating. Safe value is
 ;; always >= `helm-idle-delay'.
 helm-idle-delay 0.01
 helm-delete-minibuffer-contents-from-point t
 helm-move-to-line-cycle-in-source nil
 helm-locate-command "locate %s -e -A --regex %s"
 helm-buffers-fuzzy-matching t
 helm-candidate-number-limit 200
 helm-always-two-windows t
 helm-reuse-last-window-split-state t
 helm-buffers-favorite-modes (append helm-buffers-favorite-modes
				     '(latex-mode org-mode)))

(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)
(define-key helm-map (kbd "C-S-r") 'helm-toggle-visible-mark)


;; ==============================
;; rebinding a few things to helm
;; ==============================
(define-key global-map [remap apropos-command] 'helm-apropos) 
(define-key global-map [remap bookmark-jump] 'helm-pp-bookmarks)
(define-key global-map [remap find-tag] 'helm-etags-select)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap execute-extended-command] 'helm-M-x)

;; ==============
;; helm functions
;; ==============

;;; find-files
(global-set-key (kbd "C-x h") 'helm-find-files)

;;; list buffers
(global-set-key (kbd "C-x DEL") 'helm-buffers-list)	; C-x C-h

;;; helm for git repos
(global-set-key (kbd "C-x f") 'helm-browse-project) ; it was set-fill-column
(global-set-key (kbd "C-x C-f") 'helm-recentf)

;;; find
(global-set-key (kbd "M-s f") 'helm-find)

;;; locate
(global-set-key (kbd "C-c h")
 		(lambda ()
		  "locate for papers/books."
 		  (interactive)
 		  (helm-locate-with-db '("~/elisp/locate.db"))))

(global-set-key (kbd "C-c DEL") ;C-c C-h
		(lambda ()
		  "locate for mydocs."
		  (interactive)
		  (helm-locate-with-db '("~/elisp/locate-mydocs.db"))))

(global-set-key (kbd "C-c C-H")
		(lambda ()
		  "locate for $HOME."
 		  (interactive)
 		  (helm-locate-with-db '("~/elisp/locate-home.db"))))

;; ----------------------
;; searching within files
;; ----------------------
;; occur
(global-set-key (kbd "C-S-s") 'helm-occur)

;;; grep
(global-set-key (kbd "C-S-g")
		(lambda ()
		  (interactive)
		  (let ((current-prefix-arg '(4)))
		    (call-interactively
		     'helm-do-grep))))

;; imenu
(global-set-key (kbd "M-s t") 'helm-imenu)

;; =========
;; registers
;; =========
(global-set-key (kbd "C-x r h") 'helm-register)

;; ===================
;; mark and kill rings
;; ===================
;; mark-ring
(global-set-key (kbd "C-c z") 'helm-all-mark-rings)

;; kill-ring
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; ======
;; eshell
;; ======
;;; enable helm pcomplete
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map 
                [remap eshell-pcomplete]
                'helm-esh-pcomplete)))


;;; enable helm eshell history
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map 
                (kbd "M-p")
                'helm-eshell-history)))

(provide 'mk_helm-setup)
;;; mk_helm-setup.el ends here
