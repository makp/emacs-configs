;;; mk_ibuffer-setup.el --- Custom config ibuffer

;;; Commentary:

;; 

;;; Code:

(require 'ibuffer)
(require 'ibuf-ext)

(dolist (ibfilter '("^\\*" "_region_" "magit-process:.*" "magit-diff.*" "magit:.*"))
  (add-to-list 'ibuffer-never-show-predicates ibfilter))

(add-hook 'ibuffer-hook
	  (lambda ()
	    (ibuffer-auto-mode 1)	;auto-update
	    ))


;;  ibuffer-show-empty-filter-groups nil ;; don't show empty filter groups
;;  ibuffer-expert t ;; don't ask for confirmation of "dangerous" operations.
;;  ibuffer-filter-group-name-face 'font-lock-variable-name-face ;;
;;  ibuffer-old-time 50)


;; ;; Use human readable size column instead of original one
;; (define-ibuffer-column size-h
;;   (:name "Size" :inline t)
;;   (cond
;;    ((> (buffer-size) 1000) (format "%7.3fk" (/ (buffer-size) 1000.0)))
;;    ((> (buffer-size) 1000000) (format "%7.3fM" (/ (buffer-size) 1000000.0)))
;;    (t (format "%8d" (buffer-size)))))


(provide 'mk_ibuffer-setup)

;;; mk_ibuffer-setup.el ends here
