;;; mk_keybindings.el --- Custom non-leader keybindings -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(defvar mk/states '(normal motion))

;; Add extra key for universal argument bc of Evil mode
;; (global-set-key (kbd "M-u") 'universal-argument)
;; http://stackoverflow.com/questions/4808756/how-do-you-move-the-prefix-argument-to-a-different-key-in-emacs/4809193#4809193

;; (global-set-key (kbd "C-\\") 'eval-region)

;; C-h as backspace
(define-key key-translation-map [?\C-h] [?\C-?])

;; ------
;; winner
;; ------
(defvar evil-window-map)
(define-key evil-window-map "u" 'winner-undo)
(define-key evil-window-map "U" 'winner-redo)


;; ---
;; avy
;; ---
(evil-define-key mk/states 'global (kbd "M-c") 'avy-goto-word-1)
(define-key global-map (kbd "M-l") 'avy-goto-line)

;; ----
;; helm
;; ----
;; helm-find-files-map
;; (define-key helm-map (kbd "C-x h") 'helm-quit-and-find-file)
;; (define-key helm-map (kbd "C-x C-a") 'helm-ff-run-switch-to-eshell)
;; helm-browse-project-map
;; helm-ls-git-buffer-map
;; helm-ls-git-map

;; org-mode
(with-eval-after-load 'org
  (evil-define-key 'normal org-mode-map (kbd "zj") 'org-next-visible-heading)
  (evil-define-key 'normal org-mode-map (kbd "zk") 'org-previous-visible-heading)
  )

;; (define-key org-mode-map (kbd "") 'org-metaright)
;; (define-key org-mode-map (kbd "") 'org-metaleft)
;; (define-key org-mode-map (kbd "") 'org-metadown)
;; (define-key org-mode-map (kbd "") 'org-metaup)
;; (define-key org-mode-map (kbd "") 'org-shiftmetaleft)
;; (define-key org-mode-map (kbd "") 'org-shiftmetaright)
;; (define-key org-mode-map (kbd "") 'org-shiftup)
;; (define-key org-mode-map (kbd "") 'org-shifttab)
;; (define-key org-mode-map (kbd "") 'org-insert-todo-heading-respect-content))

(global-set-key (kbd "C-c l") 'org-store-link)
;; NOTE: This link needs to be global bc you can store links from non-org buffers
;; (global-set-key (kbd "C-c C-S-l") 'org-insert-link-global)
;; (global-set-key (kbd "C-c C-S-o") 'org-open-at-point-global)

;; latex
(with-eval-after-load 'reftex
  (define-key LaTeX-mode-map (kbd "C-c r") 'helm-bibtex) ; reftex-citation
  (define-key reftex-mode-map (kbd "C-c v") 'reftex-view-crossref)
  (define-key reftex-mode-map (kbd "C-c t") 'my-reftex-toc))

(with-eval-after-load 'latex
  (evil-define-key 'normal LaTeX-mode-map (kbd "zj") 'outline-next-visible-heading)
  (evil-define-key 'normal LaTeX-mode-map (kbd "zk") 'outline-previous-visible-heading)
  (evil-define-key 'normal LaTeX-mode-map (kbd "zm") 'outline-hide-body)
  )
;;

;; EWW
(global-set-key (kbd "C-c w") 'mk/search-web)

(with-eval-after-load 'eww
  (define-key eww-mode-map (kbd "f") 'ace-link-eww) ; "f" was undefined
  )


(provide 'mk_keybindings)
;;; mk_keybindings.el ends here