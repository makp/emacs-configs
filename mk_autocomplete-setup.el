;;; my auto-complete setup
(require 'auto-complete-config)
(ac-config-default)

;; ================
;; Dictionary files
;; ================
(add-to-list 'ac-user-dictionary-files "~/elisp/.my-ispell-personal-dictionary")

;; ===============
;; General options
;; ===============
(ac-flyspell-workaround)  ;; to fix incompatibilities with flyspell
(setq ac-auto-show-menu 3) ;; time taken to show the menu
(setq ac-auto-show-menu t)
(setq ac-auto-start 2) ; number inserted characters before autocompletion is triggered


;; ===========
;; Keybindings
;; ===========
(define-key ac-mode-map (kbd "M-/") 'ac-quick-help)

(define-key ac-mode-map (kbd "<tab>") 'auto-complete)
(define-key ac-completing-map "\t" nil) 

(define-key ac-completing-map (kbd "M-s") 'ac-isearch)

;; select candidates with C-n/C-p -- instead of M-n/M-p.
;; (setq ac-use-menu-map t)
;; (define-key ac-menu-map (kbd "C-n") 'ac-next)
;; (define-key ac-menu-map (kbd "C-p") 'ac-previous)

;; =======
;; Sources
;; =======

;; ;; --------
;; ;; ac-slime
;; ;; --------
;; (add-to-list 'load-path "~/.emacs.d/ac-slime-0.2/")
;; (require 'ac-slime)
;; (add-hook 'slime-mode-hook 'set-up-slime-ac)
;; (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'slime-repl-mode))


;; -------
;; ac-math
;; -------
(require 'ac-math)
(add-to-list 'ac-modes 'latex-mode)

(defun ac-latex-mode-setup ()
  (setq ac-sources
	(append '(ac-source-math-latex ac-source-latex-commands
	ac-source-math-unicode ac-source-yasnippet) ac-sources)))

(add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)
(setq ac-math-unicode-in-math-p t)

;; ------
;; ac-ess
;; ------
(setq ess-use-auto-complete t)

;; ;; ---------
;; ;; ac-eshell
;; ;; ---------
;; (defun ac-pcomplete ()
;;   ;; eshell uses `insert-and-inherit' to insert a \t if no completion
;;   ;; can be found, but this must not happen as auto-complete source
;;   (flet ((insert-and-inherit (&rest args)))
;;     ;; this code is stolen from `pcomplete' in pcomplete.el
;;     (let* (tramp-mode ;; do not automatically complete remote stuff
;;            (pcomplete-stub)
;;            (pcomplete-show-list t) ;; inhibit patterns like * being deleted
;;            pcomplete-seen pcomplete-norm-func
;;            pcomplete-args pcomplete-last pcomplete-index
;;            (pcomplete-autolist pcomplete-autolist)
;;            (pcomplete-suffix-list pcomplete-suffix-list)
;;            (candidates (pcomplete-completions))
;;            (beg (pcomplete-begin))
;;            ;; note, buffer text and completion argument may be
;;            ;; different because the buffer text may bet transformed
;;            ;; before being completed (e.g. variables like $HOME may be
;;            ;; expanded)
;;            (buftext (buffer-substring beg (point)))
;;            (arg (nth pcomplete-index pcomplete-args)))
;;       ;; we auto-complete only if the stub is non-empty and matches
;;       ;; the end of the buffer text
;;       (when (and (not (zerop (length pcomplete-stub)))
;;                  (or (string= pcomplete-stub ; Emacs 23
;;                               (substring buftext
;;                                          (max 0
;;                                               (- (length buftext)
;;                                                  (length pcomplete-stub)))))
;;                      (string= pcomplete-stub ; Emacs 24
;;                               (substring arg
;;                                          (max 0
;;                                               (- (length arg)
;;                                                  (length pcomplete-stub)))))))
;;         ;; Collect all possible completions for the stub. Note that
;;         ;; `candidates` may be a function, that's why we use
;;         ;; `all-completions`.
;;         (let* ((cnds (all-completions pcomplete-stub candidates))
;;                (bnds (completion-boundaries pcomplete-stub
;;                                             candidates
;;                                             nil
;;                                             ""))
;;                (skip (- (length pcomplete-stub) (car bnds))))
;;           ;; We replace the stub at the beginning of each candidate by
;;           ;; the real buffer content.
;;           (mapcar #'(lambda (cand) (concat buftext (substring cand skip)))
;;                   cnds))))))

;; (defvar ac-source-pcomplete
;;   '((candidates . ac-pcomplete)))

;; (add-hook 'eshell-mode-hook #'(lambda () 
;; 				(setq ac-sources '(ac-source-pcomplete))))

;; (add-to-list 'ac-modes 'eshell-mode)

;; --------
;; org-mode
;; --------
(add-to-list 'ac-modes 'org-mode)

(provide 'mk_autocomplete-setup)