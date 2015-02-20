;;; helm setup

;; (require 'helm-config)
;; (require 'helm-match-plugin)
;;; These two lines seem unecessary since I'm using MELPA.

(helm-mode 1)
(helm-adaptive-mode 1)
(helm-autoresize-mode 1)

;; ==========
;; Extensions
;; ==========
(autoload 'helm-ls-git-ls "helm-ls-git" nil t)

;; =========
;; Helm vars
;; =========
(setq helm-c-external-programs-associations
      '(("pdf" . "okular")
	("djvu" . "okular")
	("docx" . "libreoffice")
	("doc" . "libreoffice")
	("rtf" . "libreoffice")
	("svg" . "inkscape")
	("mp4" . "mplayer"))
      helm-pdfgrep-default-read-command "okular --unique -p %p '%f'"
      helm-input-idle-delay 0.01
      ;; be idle for this many seconds, before updating. Safe value is
      ;; always >= `helm-idle-delay'.
      helm-idle-delay 0.01
      ;; helm-delete-minibuffer-contents-from-point t
      helm-move-to-line-cycle-in-source nil
      helm-locate-command "locate %s -e -A --regex %s"
      helm-buffers-fuzzy-matching t
      helm-locate-fuzzy-match t
      helm-M-x-fuzzy-match t
      helm-candidate-number-limit 100
      helm-always-two-windows t
      helm-reuse-last-window-split-state t
      helm-buffers-favorite-modes '(latex-mode org-mode emacs-lisp-mode)
      ;; (append helm-buffers-favorite-modes 
      ;; '(latex-mode org-mode)
      helm-ls-git-status-command 'magit-status  ;
      helm-boring-file-regexp-list '("\\.git$")
      helm-boring-buffer-regexp-list
      '("*Help*" "*Completions*" "*Ibuffer*" "*toc*"))

;; helm-completing-read-handlers-alist controls where helm completion
;; is used.

;; ===========
;; Global maps
;; ===========
;; rebinding a few things to helm
(define-key global-map [remap apropos-command] 'helm-apropos) 
(define-key global-map [remap bookmark-jump] 'helm-pp-bookmarks)
(define-key global-map [remap find-tag] 'helm-etags-select)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap execute-extended-command] 'helm-M-x)
(define-key global-map [remap jump-to-register] 'helm-register)

;;; helm-resume
(global-set-key (kbd "C-c h") 'helm-resume)

;;; find-files
(global-set-key (kbd "C-x h") 'helm-find-files)

;;; list buffers
(global-set-key (kbd "C-x DEL") 'helm-buffers-list)	; C-x C-h
;;; Examples:
;;; "*lisp,sh ^helm": buffers in lisp- or sh-mode that begin with "helm".
;;; "@crash": buffers that contain the string "crash"
;;; You can also select multiple buffers or select all buffers with M-a

;;; find
(global-set-key (kbd "C-x C-f") 'helm-find)

;;; grep
(global-set-key (kbd "C-x g")
		(lambda ()
		  (interactive)
		  (let ((current-prefix-arg '(4)))
		    (call-interactively
		     'helm-do-grep))))

;;; locate
(defun mk/locate-with-helm (&optional arg)
  "locate for mydocs, pdfs, and home."
  (interactive "p")
  (cond
   ((equal arg 1)  (helm-locate-with-db '("~/elisp/locate.db")))	; pdfs
   ((equal arg 4)  (helm-locate-with-db '("~/elisp/locate-mydocs.db"))) ; ~/Documents/mydocs
   ((equal arg 16) (helm-locate-with-db '("~/elisp/locate-home.db")))))	; $HOME

(global-set-key (kbd "C-c DEL") 'mk/locate-with-helm) ;C-c C-h

;;; recentf

;; ----------------------
;; searching within files
;; ----------------------
;; occur
(global-set-key (kbd "C-S-s") 'helm-occur)

;; imenu
(global-set-key (kbd "M-s t") 'helm-imenu)

;; registers
(global-set-key (kbd "C-x r h") 'helm-register)

;; ===================
;; mark and kill rings
;; ===================
;; mark-ring
(global-set-key (kbd "C-c z") 'helm-all-mark-rings)

;; kill-ring
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; ===============
;; helm local maps
;; ===============
;;; helm-maps
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

(define-key helm-map (kbd "C-S-r") 'helm-toggle-visible-mark)
(define-key helm-map (kbd "C-x h") 'helm-quit-and-find-file)

;;; helm-buffer-map
(define-key helm-buffer-map (kbd "C-S-s") 'helm-buffers-run-multi-occur)

;;; helm-find-files-map
(define-key helm-find-files-map (kbd "C-x C-a") 'helm-ff-run-switch-to-eshell)
(define-key helm-find-files-map (kbd "C-x g") (lambda ()
				     (interactive)
				     (let ((current-prefix-arg '(4)))
				       (call-interactively
					'helm-ff-run-grep))))

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

;; fix completion after sudo (from helm wiki)
(defun pcomplete/sudo ()
  (let ((prec (pcomplete-arg 'last -1)))
    (cond ((string= "sudo" prec)
           (while (pcomplete-here*
                    (funcall pcomplete-command-completion-function)
                    (pcomplete-arg 'last) t))))))


(provide 'mk_helm-setup)
;;; mk_helm-setup.el ends here
