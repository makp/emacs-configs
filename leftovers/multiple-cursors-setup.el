

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