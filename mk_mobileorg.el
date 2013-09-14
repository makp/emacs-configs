;; =========
;; mobileorg
;; =========
;;; Set your Dropbox root directory
(setq org-mobile-directory "~/Dropbox/android/MobileOrg/")

;;; Name of the file where new notes captured in the android device
;;; will be stored
(setq org-mobile-inbox-for-pull "~/elisp/agenda/ag-geral.org")


(setq org-mobile-files (append
			'("~/Documents/mydocs/notes/emacs-and-lisp/emacs.org")
			org-agenda-files))

;; -------------------------
;; automatic org-mobile-push
;; -------------------------
;; This will make it possible to do a chain of edits in several files,
;; resulting in Emacs running push only once after it has been idle
;; for the specified number of seconds.
(defvar org-mobile-push-timer nil
  "Timer that `org-mobile-push-timer' used to reschedule itself, or nil.")

(defun org-mobile-push-with-delay (secs)
  (when org-mobile-push-timer
    (cancel-timer org-mobile-push-timer))
  (setq org-mobile-push-timer
        (run-with-idle-timer
         (* 1 secs) nil 'org-mobile-push)))

(add-hook 'after-save-hook 
 (lambda () 
   (when (eq major-mode 'org-mode)
     (dolist (file (org-mobile-files-alist))
      (if (string= (file-truename (expand-file-name (car file)))
		   (file-truename (buffer-file-name)))
           (org-mobile-push-with-delay 1200))))))

;; refreshes agenda file each day
(run-at-time "00:05" 86400 '(lambda () (org-mobile-push-with-delay 1))) 

;; -------------------------
;; automatic org-mobile-pull
;; -------------------------
(org-mobile-pull) ;; run org-mobile-pull at startup

(defun install-monitor (file secs)
  (run-with-timer
   0 secs
   (lambda (f p)
     (unless (< p (second (time-since (elt (file-attributes f) 5))))
       (org-mobile-pull)))
   file secs))

(install-monitor (file-truename
                  (concat
                   (file-name-as-directory org-mobile-directory)
                          org-mobile-capture-file))
                 5)

;; Do a pull every 5 minutes to circumvent problems with timestamping
;; (i.e., dropbox bugs)
(run-with-timer 0 (* 5 60) 'org-mobile-pull)

(provide 'mk_mobileorg)