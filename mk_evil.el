(require 'evil)
(evil-mode 1)

(setq evil-search-module 'evil-search)

;; ---
;; C-u
;; ---
;; http://stackoverflow.com/questions/4808756/how-do-you-move-the-prefix-argument-to-a-different-key-in-emacs/4809193#4809193
(global-set-key (kbd "M-u") 'universal-argument)
(evil-define-key '(normal visual) 'global (kbd "C-u") 'evil-scroll-up)
(evil-define-key 'insert 'global (kbd "C-u") 'evil-delete-back-to-indentation)

;; ----------
;; Leader key
;; ----------
;; These vars are supposed to simplify keybinding assignments
(defvar mk/states-narrow '(normal motion))
(defvar mk/states-wide '(normal motion emacs))

(evil-set-leader '(normal motion) (kbd "SPC"))
;; SPC is assigned to the function `evil-forward-char' in normal and motion states.
(evil-set-leader 'emacs (kbd "C-SPC"))
(evil-define-key 'emacs 'global (kbd "C-w") 'evil-window-map)

;; ----
;; helm
;; ----
(evil-define-key mk/states-wide 'global (kbd "<leader>l") 'helm-resume)

;; ----
;; help
;; ----
(evil-define-key mk/states-wide 'global (kbd "<leader>h") 'help)

;; ---
;; M-x
;; ---
(evil-define-key mk/states-wide 'global (kbd "<leader> SPC") 'helm-M-x)

;; ----
;; file
;; ----
(evil-define-key mk/states-wide 'global (kbd "<leader>ff") 'helm-find-files)

;; ------
;; buffer
;; ------
(evil-define-key mk/states-wide 'global (kbd "<leader>bs") 'save-buffer)
(evil-define-key mk/states-wide 'global (kbd "<leader>bl") 'ibuffer)
(evil-define-key mk/states-wide 'global (kbd "<leader>bb") 'helm-mini)
(evil-define-key mk/states-wide 'global (kbd "<leader>bn") 'bury-buffer)

;; ----
;; jump
;; ----
(evil-define-key mk/states-narrow 'global (kbd "M-c") 'avy-goto-word-1)


;; -----
;; dired
;; -----
(evil-define-key mk/states-wide 'global (kbd "<leader>dj") 'mk/dired-jump)


;; ----
;; ring
;; ----
(evil-define-key mk/states-wide 'global (kbd "<leader>rk") 'helm-show-kill-ring)

;; ------
;; window
;; ------
(evil-define-key mk/states-wide 'global (kbd "<leader>ww") 'evil-window-next)
(evil-define-key mk/states-wide 'global (kbd "<leader>wu") 'winner-undo)
(evil-define-key mk/states-wide 'global (kbd "<leader>wr") 'winner-redo)


;; ------
;; search
;; ------
(evil-define-key mk/states-wide 'global (kbd "<leader>so") 'helm-occur)

;; ---
;; Git
;; ---
(evil-define-key mk/states-wide 'global (kbd "<leader>gs") 'magit-status)
(evil-define-key mk/states-wide 'global (kbd "<leader>gg") 'mk/grep-project)
(evil-define-key mk/states-wide 'global (kbd "<leader>gf") 'mk/fetch)
(evil-define-key mk/states-wide 'global (kbd "<leader>gb") 'mk/browse-project)


;; -------
;; comment
;; -------
(evil-define-key mk/states-wide 'global (kbd "<leader>cl") 'comment-line)
(evil-define-key 'visual 'global (kbd "<leader>cr") 'comment-or-uncomment-region)

;; -----
;; elisp
;; -----
(evil-define-key mk/states-wide 'global (kbd "<leader>el") 'eval-last-sexp)


(provide 'mk_evil)
;;; mk_evil.el ends here