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

(setq evil-collection-setup-minibuffer t) ; nil is the default

;; Leader keys
(defvar mk/leader-key "\\" "Main leader key.")
(defvar mk/alt-leader-key "C-\\"
  "Alternative leader key to be used with other states, including insert state.")

;; Leader key states
(defvar mk/leader-states '(normal motion)
  "Variable storing states affected by the leader key.")

(defvar mk/alt-leader-states '(insert visual emacs)
  "Variable storing states affected by the alternative leader key.")


;; Prevent conflict with the leader keys
(setq evil-collection-key-blacklist `(,mk/leader-key))
(keymap-global-unset mk/alt-leader-key)


;; Bind leader keys
(evil-set-leader mk/leader-states (kbd mk/leader-key))
(evil-set-leader mk/alt-leader-states (kbd mk/alt-leader-key))


;; Leader keybindings
(require 'mk_leader-keybindings)

(setq evil-want-C-w-in-emacs-state t)

;; Make C-a/C-x behave like vim (from evil-numbers)
(evil-define-key '(normal visual) 'global (kbd "C-a") 'evil-numbers/inc-at-pt)
(evil-define-key '(normal visual) 'global (kbd "C-x") 'evil-numbers/dec-at-pt)
;; evil-numbers/inc-at-pt-incremental
;; evil-numbers/dec-at-pt-incremental


;; ------------
;; Line numbers
;; ------------
(global-set-key (kbd "M-l") 'display-line-numbers-mode)


;; ----------
;; Projectile
;; ----------
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; ------
;; winner
;; ------
;; (require 'evil-vars)
(define-key evil-window-map "u" 'winner-undo)
(define-key evil-window-map "U" 'winner-redo)


;; company
(define-key company-active-map (kbd "TAB") nil) ; it was `company-complete-common-or-cycle'
(define-key company-active-map (kbd "<tab>") nil)
(define-key company-active-map (kbd "C-w") nil) ; it was `company-show-location'
(define-key company-active-map (kbd "C-h") nil) ; it was `company-show-doc-buffer'


;; helm
(define-key helm-map (kbd "C-w") 'backward-kill-word) ;it was `helm-yank-text-at-point'
(define-key helm-map (kbd "M-w") 'helm-yank-text-at-point)

;; org-mode
(evil-define-key 'normal org-mode-map (kbd "M-l") nil) ; it was `org-demote-subtree'
(setq evil-collection-calendar-want-org-bindings t
      evil-collection-outline-bind-tab-p t)

;; (define-key org-mode-map (kbd "C-c r") 'helm-bibtex)

;; (define-key org-mode-map (kbd "") 'org-metaright)
;; (define-key org-mode-map (kbd "") 'org-metaleft)
;; (define-key org-mode-map (kbd "") 'org-metadown)
;; (define-key org-mode-map (kbd "") 'org-metaup)
;; (define-key org-mode-map (kbd "") 'org-shiftup)
;; (define-key org-mode-map (kbd "") 'org-shifttab)
;; (define-key org-mode-map (kbd "") 'org-insert-todo-heading-respect-content))

;; (evil-define-key 'normal org-mode-map
;;   (kbd ">") 'org-shiftmetaright
;;   (kbd "<") 'org-shiftmetaleft)

;; (global-set-key (kbd "C-c l") 'org-store-link)

;; latex
(with-eval-after-load 'reftex
  (define-key reftex-mode-map (kbd "C-c r") 'helm-bibtex) ; reftex-citation
  (define-key reftex-mode-map (kbd "C-c v") 'reftex-view-crossref)
  (define-key reftex-mode-map (kbd "C-c t") 'my-reftex-toc))

;; (evil-define-key 'normal LaTeX-mode-map (kbd "zj") 'outline-next-visible-heading)
;; (evil-define-key 'normal LaTeX-mode-map (kbd "zk") 'outline-previous-visible-heading)
;; (evil-define-key 'normal LaTeX-mode-map (kbd "zm") 'outline-hide-body)

;; -------
;; chatgpt
;; -------
(add-hook 'chatgpt-mode-hook
	  (lambda ()
	    (evil-define-key 'normal chatgpt-mode-map (kbd "RET") 'mk/chatgpt-write-message)))


;; EWW
;; (global-set-key (kbd "C-c w") 'mk/search-web)
(define-key eww-mode-map (kbd "f") 'ace-link-eww) ; "f" was undefined

;; dired
;; (define-key dired-mode-map "E" 'dired-ediff-marked-files)

(provide 'mk_keybindings)
;;; mk_keybindings.el ends here
