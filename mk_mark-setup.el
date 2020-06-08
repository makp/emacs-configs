;;; my mark setup 

(setq set-mark-command-repeat-pop t)
;; If `set-mark-command-repeat-pop' is non-nil, repeating the C-SPC
;; command with no prefix argument pops the next position off the
;; local (or global) mark ring and jumps there.

;; ===================================
;; push-mark without activating region
;; ===================================
(defun mk/mark-command (&optional arg)
  (interactive "P")
  (if (consp arg)
      (set-mark-command nil)
    (push-mark-no-activate)))

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(global-set-key (kbd "C-r") 'mk/mark-command)

;; -----------------
;; pop mark commands
;; -----------------
(global-set-key (kbd "C-z") 'pop-to-mark-command)
(global-set-key (kbd "C-S-z") 'mc/mark-pop)

;; =============
;; mark commands
;; =============
(global-set-key (kbd "M-r") 'mark-sexp)
(global-set-key (kbd "C-x C-r") 'set-mark-command)   ; it was find-file-read-only
(global-set-key (kbd "C-S-r") 'mark-paragraph)
;; (global-set-key (kbd "M-R")

;; ====================
;; region-bindings-mode
;; ====================
(require 'region-bindings-mode)
(region-bindings-mode-enable)

;; ================
;; multiple cursors
;; ================
(require 'multiple-cursors)

;;; If you have an active region, you can add a cursor to each line
;;; with this command:
(define-key region-bindings-mode-map "m" 'mc/edit-lines)

;;; If you don't have an active region, you can set a rectangular
;;; region with:
(global-set-key (kbd "C-x r t") 'set-rectangular-region-anchor)

(global-set-key (kbd "C-x r n") 'mc/insert-numbers)

(global-set-key (kbd "M-o") 'mc/mark-next-like-this)
;;; TIP: if no region is selected, it will just add a cursor on the
;;; next line.

(global-set-key (kbd "C-<") 'mc/reverse-regions)
;;; TIP: with nothing selected and just one cursor, it will flip the
;;; sexp at point and the one below it.

;;; When you have cursors out of your view, press "C-'"

;;; If you want to add multiple cursors not based on continous lines
;;; but on keywords, use:
(define-key region-bindings-mode-map "g" 'keyboard-quit)
(define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
(define-key region-bindings-mode-map "t" 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map "N" 'mc/unmark-next-like-this)
(define-key region-bindings-mode-map "T" 'mc/unmark-previous-like-this)
(define-key region-bindings-mode-map ">" 'mc/skip-to-next-like-this)
(define-key region-bindings-mode-map "<" 'mc/skip-to-previous-like-this)

(define-key region-bindings-mode-map "z" 'exchange-point-and-mark)

(define-key region-bindings-mode-map "*" 'mc/mark-all-like-this)
(define-key region-bindings-mode-map "%" 'query-replace-regexp)
(define-key region-bindings-mode-map "." 'mc/mark-all-in-region)

(define-key region-bindings-mode-map "r" 'mark-sexp)
(define-key region-bindings-mode-map "-" '(lambda ()
					    (interactive)
					    (exchange-point-and-mark)
					    (backward-sexp)
					    (exchange-point-and-mark)))

(define-key region-bindings-mode-map "C" 'duplicate-current-line-or-region)
(define-key region-bindings-mode-map "S" 'mc/sort-regions)

(define-key region-bindings-mode-map "w" 'kill-ring-save)

(define-key region-bindings-mode-map ";" 'comment-dwim)

;;; 
(provide 'mk_mark-setup)
;;; my-mark-setup.el ends here