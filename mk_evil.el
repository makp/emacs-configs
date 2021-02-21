(require 'evil)
(evil-mode 1)

;; =======
;; keymaps
;; =======
(evil-set-leader 'normal (kbd "SPC"))

;; ---
;; M-x
;; ---
(evil-define-key 'normal 'global (kbd "<leader> SPC") 'helm-M-x)

;; ----
;; file
;; ----
(evil-define-key 'normal 'global (kbd "<leader>ff") 'helm-find-files)

;; ------
;; buffer
;; ------
(evil-define-key 'normal 'global (kbd "<leader>bs") 'save-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bl") 'ibuffer)
(evil-define-key 'normal 'global (kbd "<leader>bb") 'helm-mini)
(evil-define-key 'normal 'global (kbd "<leader>bn") 'bury-buffer)

;; ----
;; jump
;; ----
(evil-define-key 'normal 'global (kbd "<leader>jl") 'avy-goto-line)
(evil-define-key 'normal 'global (kbd "<leader>jc") 'avy-goto-word-1)


;; -----
;; dired
;; -----
(evil-define-key 'normal 'global (kbd "<leader>dj") 'mk/dired-jump)


;; ----
;; ring
;; ----
(evil-define-key 'normal 'global (kbd "<leader>rk") 'helm-show-kill-ring)


;; ------
;; window
;; ------
(evil-define-key 'normal 'global (kbd "<leader>wo") 'other-window)
(evil-define-key 'normal 'global (kbd "<leader>wdo") 'delete-other-windows)
(evil-define-key 'normal 'global (kbd "<leader>wdd") 'delete-window)


;; ------
;; search
;; ------
(evil-define-key 'normal 'global (kbd "<leader>so") 'helm-occur)

;; ---
;; Git
;; ---
(evil-define-key 'normal 'global (kbd "<leader>gs") 'magit-status)
(evil-define-key 'normal 'global (kbd "<leader>gg") 'mk/grep-project)


;; -------
;; comment
;; -------
(evil-define-key 'normal 'global (kbd "<leader>cl") 'comment-line)


(provide 'mk_evil)
;;; mk_evil.el ends here
