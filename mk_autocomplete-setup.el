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
;; (define-key ac-mode-map (kbd "M-/") 'ac-quick-help)

(define-key ac-mode-map (kbd "<tab>") 'auto-complete)
;; for when auto-complete-mode does not start automatically (e.g., you
;; want completion without inserting any character).
;; "(kbd "<tab>")" is not supposed to bind C-i but only the TAB key

;; Autocomplete has two ways of completing after completion is
;; started: ac-expand and ac-complete. ac-complete is set to C-m or
;; RET.

(define-key ac-completing-map (kbd "TAB") 'ac-expand)

(define-key ac-completing-map "\t" nil) 
(define-key ac-completing-map (kbd "M-s") 'ac-isearch)



;; select candidates with C-n/C-p -- instead of M-n/M-p.
;; (setq ac-use-menu-map t)
;; (define-key ac-menu-map (kbd "C-n") 'ac-next)
;; (define-key ac-menu-map (kbd "C-p") 'ac-previous)

;; =======
;; Sources
;; =======

;; --------
;; ac-slime
;; --------

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
;; (setq ess-use-auto-complete t)

;; ---------
;; ac-eshell
;; ---------

;; --------
;; org-mode
;; --------
(add-to-list 'ac-modes 'org-mode)

(provide 'mk_autocomplete-setup)