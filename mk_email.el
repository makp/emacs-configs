;; ============
;; General info
;; ============
;;; In home, there is the configuration file for notmuch that controls
;;; initial tagging, etc.

;; =============
;; sending email
;; =============

(defun envia-email (&optional arg)
  (interactive "P")
  (compose-mail)
  (when (consp arg)
    (message-goto-from)
    (message-beginning-of-line)
    (kill-line)
    (insert "\"Makmiller Pedroso\" <mpedroso@towson.edu>")
    (message-goto-fcc)
    (insert "~/Mail/TU/Sent")
    (message-goto-subject)))

(global-set-key (kbd "C-x m") 'envia-email)


;; =======
;; notmuch
;; =======
(require 'notmuch)

;; ----------------
;; search-mode maps
;; ----------------

;;; send email with `m' and reply with `r'

(global-set-key (kbd "M-s e n") 'notmuch)

;; (autoload 'notmuch-search "notmuch" "Notmuch search. " t)
(global-set-key (kbd "M-s e s") 'notmuch-search)

(global-set-key (kbd "M-s e u") (lambda ()
                                  (interactive)
                                  (notmuch-search "tag:unread")))

(global-set-key (kbd "M-s e i") (lambda ()
                                  (interactive)
                                  (notmuch-search "tag:tudo")))

(global-set-key (kbd "M-s e f") (lambda ()
                                  (interactive)
                                  (notmuch-search "tag:flagged")))

(global-set-key (kbd "M-s e d") (lambda ()
                                  (interactive)
                                  (notmuch-search "tag:draft")))

(global-set-key (kbd "M-s e t") (lambda ()
                                  (interactive)
                                  (notmuch-search "tag:sent")))

;; ----------
;; operations
;; ----------

(define-key notmuch-search-mode-map "D"
  (lambda ()
    "Toggle deleted tag for thread"
    (interactive)
    (if (member "delete" (notmuch-search-get-tags))
	(notmuch-search-tag '("-delete"))
      (notmuch-search-tag '("+delete" "-unread")))
    (next-line)))

(define-key notmuch-show-mode-map "D"
  (lambda ()
    "toggle deleted tag for message"
    (interactive)
    (if (member "delete" (notmuch-show-get-tags))
	(notmuch-show-tag '("-delete"))
      (notmuch-show-tag '("+delete" "-unread")))))

(define-key notmuch-search-mode-map "u"
  (lambda ()
    "toggle the flagged tag for thread"
    (interactive)
    (if (member "flagged" (notmuch-search-get-tags))
	(notmuch-search-tag '("-flagged"))
      (notmuch-search-tag '("+flagged")))))

(define-key notmuch-show-mode-map "u"
  (lambda ()
    "toggle the flagged tag for message"
    (interactive)
    (if (member "flagged" (notmuch-show-get-tags))
	(notmuch-show-tag '("-flagged"))
      (notmuch-show-tag '("+flagged")))))

(define-key notmuch-search-mode-map "d"
  (lambda ()
    "toggle the unread tag for thread"
    (interactive)
    (if (member "unread" (notmuch-search-get-tags))
	(notmuch-search-tag '("-unread"))
      (notmuch-search-tag '("+unread")))
      (next-line)))

(define-key notmuch-search-mode-map (kbd "g") 'notmuch-poll-and-refresh-this-buffer)
;;; g was self-insert-command

(setq message-kill-buffer-on-exit t)
;;; close the mail buffer after sending it. 

(setq notmuch-fcc-dirs '(("mpedroso@towson.edu" . "TU/Sent")))

(setq mm-text-html-renderer 'w3m)
;;; this variable controls of the display of an HTML message. Here I'm
;;; saying to use emacs-w3m. The default value was gnus-w3m

(define-key notmuch-show-mode-map (kbd "o") 'w3m-external-view-this-url)
(define-key notmuch-show-mode-map (kbd "T") 'w3m-toggle-inline-images)


;;; Search in notmuch supports wildcards (*) and operators (+, -, AND,
;;; NOT, XOR, OR, brackets, NEAR, ADJACENT, from: )

(require 'notmuch-address)
(setq notmuch-address-command "~/scripts/third-party-scripts/nottoomuch/nottoomuch-addresses.sh")
(notmuch-address-message-insinuate)

(provide 'mk_email)