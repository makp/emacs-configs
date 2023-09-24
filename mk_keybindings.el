;;; mk_keybindings.el --- Custom non-leader keybindings -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:


;; Set C-h as backspace instead of help
(define-key key-translation-map [?\C-h] [?\C-?])

;; (global-set-key (kbd "M-o") 'open-line)


;; ----
;; Evil
;; ----
;; Enable evil collection keybindings
(evil-collection-init)
;; (evil-collection-init '(dired ibuffer))

;; Leader key
(defvar mk/leader-states '(normal visual motion emacs)
  "Variable storing the states affected by the keybindings with the leader key.")

(defvar mk/leader-key "\\" "The leader key.")

(evil-set-leader mk/leader-states mk/leader-key)

;; Prevent evil-collection from binding my leader key
(setq evil-collection-key-blacklist `(,mk/leader-key))

;; Leader keybindings
(require 'mk_leader-keybindings)


;; ;; C-w behavior
;; (define-key helm-map (kbd "C-w") 'backward-kill-word) ;it was `helm-yank-text-at-point'
;; ;; (setq evil-want-C-w-in-emacs-state t)
;; (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
;; ;; it was `company-show-location'
;; (define-key company-active-map (kbd "M-w") 'company-show-location)


;; ;; Make C-a/C-x behave like vim
;; (evil-define-key 'normal 'global (kbd "C-a") 'evil-numbers/inc-at-pt)
;; (evil-define-key 'normal 'global (kbd "C-x") 'evil-numbers/dec-at-pt)
;; (evil-define-key 'visual 'global (kbd "C-a") 'evil-numbers/inc-at-pt-incremental)
;; (evil-define-key 'visual 'global (kbd "C-x") 'evil-numbers/dec-at-pt-incremental)

;; ;; Poor man's version of gp and gP
;; (evil-define-key 'normal 'global
;;   "gp" "p`]"
;;   "gP" "P`]")

;; ------------
;; Line numbers
;; ------------
(global-set-key (kbd "M-l") 'display-line-numbers-mode)


;; ------
;; winner
;; ------
;; (require 'evil-vars)
(define-key evil-window-map "u" 'winner-undo)
(define-key evil-window-map "U" 'winner-redo)



;; company
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

;; company-search-candidates
;; (global-set-key (kbd "<tab>") 'company-complete-common-or-cycle) ;TAB
;; (define-key company-active-map (kbd "TAB") 'company-complete) ;C-i

;; org-mode
;; (evil-define-key 'normal org-mode-map
;;   (kbd ">") 'org-shiftmetaright
;;   (kbd "<") 'org-shiftmetaleft)
(define-key org-mode-map (kbd "C-c r") 'helm-bibtex)

;; (define-key org-mode-map (kbd "") 'org-metaright)
;; (define-key org-mode-map (kbd "") 'org-metaleft)
;; (define-key org-mode-map (kbd "") 'org-metadown)
;; (define-key org-mode-map (kbd "") 'org-metaup)
;; (define-key org-mode-map (kbd "") 'org-shiftup)
;; (define-key org-mode-map (kbd "") 'org-shifttab)
;; (define-key org-mode-map (kbd "") 'org-insert-todo-heading-respect-content))

(global-set-key (kbd "C-c l") 'org-store-link)
;; NOTE: This key needs to be global bc you can store links from non-org buffers
;; (global-set-key (kbd "C-c C-S-l") 'org-insert-link-global)
;; (global-set-key (kbd "C-c C-S-o") 'org-open-at-point-global)

;; latex
(with-eval-after-load 'reftex
  (define-key reftex-mode-map (kbd "C-c r") 'helm-bibtex) ; reftex-citation
  (define-key reftex-mode-map (kbd "C-c v") 'reftex-view-crossref)
  (define-key reftex-mode-map (kbd "C-c t") 'my-reftex-toc))


;; (evil-define-key 'normal LaTeX-mode-map (kbd "zj") 'outline-next-visible-heading)
;; (evil-define-key 'normal LaTeX-mode-map (kbd "zk") 'outline-previous-visible-heading)
;; (evil-define-key 'normal LaTeX-mode-map (kbd "zm") 'outline-hide-body)

;; EWW
;; (global-set-key (kbd "C-c w") 'mk/search-web)
(define-key eww-mode-map (kbd "f") 'ace-link-eww) ; "f" was undefined

;; dired
(define-key dired-mode-map (kbd "C-x C-q") 'mk/dired-toggle-edit-from-evil)
;; (define-key dired-mode-map "E" 'dired-ediff-marked-files)
;; (define-key dired-mode-map "l" 'dired-up-directory)

(provide 'mk_keybindings)
;;; mk_keybindings.el ends here
