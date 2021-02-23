(require 'evil)
(evil-mode 1)

;; ----------
;; Leader key
;; ----------
(defvar mk/leader-states '(normal motion emacs))

(evil-set-leader '(normal motion) (kbd "SPC"))
;; SPC is assigned to the function `evil-forward-char' in normal and motion states.

(evil-set-leader 'emacs (kbd "C-SPC"))

;; ---
;; M-x
;; ---
(evil-define-key '(normal motion) 'global (kbd "<leader> SPC") 'helm-M-x)

;; ----
;; file
;; ----
(evil-define-key mk/leader-states 'global (kbd "<leader>ff") 'helm-find-files)

;; ------
;; buffer
;; ------
(evil-define-key mk/leader-states 'global (kbd "<leader>bs") 'save-buffer)
(evil-define-key mk/leader-states 'global (kbd "<leader>bl") 'ibuffer)
(evil-define-key mk/leader-states 'global (kbd "<leader>bb") 'helm-mini)
(evil-define-key mk/leader-states 'global (kbd "<leader>bn") 'bury-buffer)

;; ----
;; jump
;; ----
(evil-define-key mk/leader-states 'global (kbd "<leader>jl") 'avy-goto-line)
(evil-define-key mk/leader-states 'global (kbd "<leader>jc") 'avy-goto-word-1)


;; -----
;; dired
;; -----
(evil-define-key mk/leader-states 'global (kbd "<leader>dj") 'mk/dired-jump)


;; ----
;; ring
;; ----
(evil-define-key mk/leader-states 'global (kbd "<leader>rk") 'helm-show-kill-ring)

;; ------
;; window
;; ------
(evil-define-key mk/leader-states 'global (kbd "<leader>wo") 'other-window)
(evil-define-key mk/leader-states 'global (kbd "<leader>wdo") 'delete-other-windows)
(evil-define-key mk/leader-states 'global (kbd "<leader>wdd") 'delete-window)

(evil-define-key mk/leader-states 'global (kbd "<leader>wu") 'winner-undo)
(evil-define-key mk/leader-states 'global (kbd "<leader>wr") 'winner-redo)


;; ------
;; search
;; ------
(evil-define-key mk/leader-states 'global (kbd "<leader>so") 'helm-occur)

;; ---
;; Git
;; ---
(evil-define-key mk/leader-states 'global (kbd "<leader>gs") 'magit-status)
(evil-define-key mk/leader-states 'global (kbd "<leader>gg") 'mk/grep-project)
(evil-define-key mk/leader-states 'global (kbd "<leader>gf") 'mk/browse-project)


;; -------
;; comment
;; -------
(evil-define-key mk/leader-states 'global (kbd "<leader>cl") 'comment-line)


(provide 'mk_evil)
;;; mk_evil.el ends here
