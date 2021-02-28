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


(provide 'mk_keybindings)
;;; mk_keybindings.el ends here