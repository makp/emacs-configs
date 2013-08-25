;;; helm setup
(require 'helm-config)
(require 'helm-match-plugin)
(helm-mode t)
;; helm-completing-read-handlers-alist controls where we use helm.

;; ====
;; seqs
;; ====
(setq 
 helm-c-external-programs-associations
 '(("pdf" . "zathura")
   ("docx" . "libreoffice")
   ("rtf" . "libreoffice"))
  helm-input-idle-delay 0.1
 ;; be idle for this many seconds, before updating. Safe value is
 ;; always >= `helm-idle-delay'.
 helm-idle-delay 0.1
 helm-delete-minibuffer-contents-from-point t)

;; ==============
;; helm functions
;; ==============
;; -------
;; my-helm
;; -------
(defun my-helm ()
  (interactive)
  (helm
   :prompt "Switch to: "
   :candidate-number-limit 21                
   :sources
   '(helm-c-source-buffers-list			;; buffers
     helm-c-source-recentf			;; recent files
     helm-c-source-bookmarks			;; bookmarks
     helm-c-source-files-in-current-dir)))	;; current dir

(global-set-key (kbd "C-x h") 'my-helm)	; C-x C-h

;; ------------------------
;; searching for file names
;; ------------------------
;;; find-files
(global-set-key (kbd "C-x DEL") 'helm-find-files)

;;; find
(global-set-key (kbd "M-s f") 'helm-find)

;;; locate
(global-set-key (kbd "C-c h")
 		(lambda ()
		  "locate for papers/books."
 		  (interactive)
 		  (helm-locate-with-db '("/home/makmiller/elisp/locate.db"))))

(global-set-key (kbd "C-c DEL") 	;C-c C-h
		(lambda ()
		  "locate for mydocs."
		  (interactive)
		  (helm-locate-with-db '("/home/makmiller/elisp/locate-mydocs.db"))))

(global-set-key (kbd "C-c C-H")
		(lambda ()
		  "locate for papers/books."
 		  (interactive)
 		  (helm-locate-with-db '("/home/makmiller/elisp/locate-home.db"))))

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
(global-set-key (kbd "C-x c y") 'helm-show-kill-ring)

;; ==============================
;; rebinding a few things to helm
;; ==============================
(define-key global-map [remap apropos-command] 'helm-apropos) 
(define-key global-map [remap bookmark-jump] 'helm-pp-bookmarks)
(define-key global-map [remap find-tag] 'helm-etags-select)
(define-key global-map [remap occur] 'helm-occur)

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
