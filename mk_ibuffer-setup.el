;;; mk_ibuffer-setup.el --- Custom config ibuffer

;;; Commentary:

;; 

;;; Code:

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-auto-mode 1)))	;auto-update

(add-hook 'ibuffer-hook
	  (lambda ()
	    (setq truncate-lines 1)))

(setq-default
 ibuffer-show-empty-filter-groups nil ;; don't show empty filter groups
 ibuffer-expert t ;; don't ask for confirmation of "dangerous" operations.
 ibuffer-filter-group-name-face 'font-lock-variable-name-face ;;
 ibuffer-old-time 50)

(require 'ibuf-ext)
(dolist (ibfilter '("^\\*" "_region_"))
  (add-to-list 'ibuffer-never-show-predicates ibfilter))

;; ibuffer mode maps
(define-key ibuffer-mode-map (kbd "C-i") 'ibuffer-toggle-filter-group)

(define-key ibuffer-mode-map "l" (lambda ()
				   (interactive)
				   (call-interactively 'ace-jump-line-mode)))

(define-key ibuffer-mode-map (kbd "U") 'ibuffer-unmark-all)
;;; to be consistent with dired-mode. It was ibuffer.*regexp

(defun mk/open-magit-status-from-ibuffer ()
  (interactive)
  (ibuffer-visit-buffer)
  (call-interactively 'magit-status))

(define-key ibuffer-mode-map (kbd "g") 'mk/open-magit-status-from-ibuffer)

;; Use human readable size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000) (format "%7.3fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000000) (format "%7.3fM" (/ (buffer-size) 1000000.0)))
   (t (format "%8d" (buffer-size)))))

;; ibuffer-vc
(add-hook 'ibuffer-hook
	  (lambda ()
	    (ibuffer-vc-set-filter-groups-by-vc-root)
	    (unless (eq ibuffer-sorting-mode 'alphabetic)
	      (ibuffer-do-sort-by-alphabetic))))

;; The following code uses `force-mode-line-update' to cause
;; `vc-state' to work properly. Source:
;; https://emacs.stackexchange.com/questions/35758/vc-status-behavior-in-ibuffer-vc/41024#41024
(defun vc-state-refresh-post-command-hook ()
  "Check if command in `this-command' was executed, then run `vc-refresh-state'"
  (when (memq this-command '(other-window kill-buffer ibuffer))
    (vc-refresh-state)))

(add-hook 'after-save-hook 'vc-refresh-state)
(add-hook 'after-revert-hook 'vc-refresh-state)
(add-hook 'post-command-hook #'vc-state-refresh-post-command-hook)


;; Type '`' to switch through different ibuffer-formats. Use "," to
;; change how files are sorted.
(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
	      (name 18 18 :left :elide)
	      " "
	      (size-h 9 -1 :right)
	      " "
	      (mode 6 6 :left :elide)
	      " "
	      (vc-status 12 12 :left)
	      " "
	      filename-and-process)
	(mark modified read-only vc-status-mini " "
	      (name 18 18 :left :elide)
	      " "
	      (size-h 9 -1 :right)
	      " "
	      (mode 6 6 :left :elide)
	      " "
	      (vc-status 12 12 :left))))


;; (defun ibuffer-ediff-marked-buffers ()
;;   (interactive)
;;   (let* ((marked-buffers (ibuffer-get-marked-buffers))
;;          (len (length marked-buffers)))
;;     (unless (= 2 len)
;;       (error (format "%s buffer%s been marked (needs to be 2)"
;;                      len (if (= len 1) " has" "s have"))))
;;     (ediff-buffers (car marked-buffers) (cadr marked-buffers))))

;; (define-key ibuffer-mode-map "e" 'ibuffer-ediff-marked-buffers)


(provide 'mk_ibuffer-setup)

;;; mk_ibuffer-setup.el ends here