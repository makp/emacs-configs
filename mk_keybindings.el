;;; mk_keybindings.el --- Custom non-leader keybindings -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(defvar mk/states '(normal motion))

;; Add extra key for universal argument bc of Evil mode
(global-set-key (kbd "M-u") 'universal-argument)
;; http://stackoverflow.com/questions/4808756/how-do-you-move-the-prefix-argument-to-a-different-key-in-emacs/4809193#4809193

;; (global-set-key (kbd "C-x r q") 'save-buffers-kill-emacs)
;; (global-set-key (kbd "C-\\") 'eval-region)

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
;; (with-eval-after-load 'org
;;   (define-key org-mode-map (kbd "C-,") nil) ;was org-cycle-agenda-files

;;   ;; for promoting and demoting headings
;;   ;; (define-key org-mode-map (kbd "C-S-n") 'org-metaright)
;;   ;; (define-key org-mode-map (kbd "C-S-t") 'org-metaleft)

;;   ;; (define-key org-mode-map (kbd "M-T") 'org-metadown)
;;   ;; (define-key org-mode-map (kbd "M-N") 'org-metaup)

;;   ;; demoting and demoting a heading and its subtrees
;;   ;; (define-key org-mode-map (kbd "M-B") 'org-shiftmetaleft)
;;   ;; (define-key org-mode-map (kbd "M-F") 'org-shiftmetaright)
;;   ;; (define-key org-mode-map (kbd "C-S-t") 'org-shiftup)

;;   (define-key org-mode-map (kbd "C-c p") nil)  ; it was orgtbl-ascii-plot

;;   (define-key org-mode-map (kbd "C-c t") 'org-shifttab)
;;   (define-key org-mode-map (kbd "C-c SPC") nil)
;;   (define-key org-mode-map (kbd "M-h") nil)

;;   (define-key org-mode-map (kbd "C-x p") nil)

;;   (define-key org-mode-map (kbd "C-c C-j") 'org-insert-todo-heading-respect-content))


(provide 'mk_keybindings)
;;; mk_keybindings.el ends here