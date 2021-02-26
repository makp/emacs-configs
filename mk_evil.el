(setq-default evil-respect-visual-line-mode t)
(setq-default evil-undo-system 'undo-redo)
(setq-default evil-search-module 'evil-search)
(require 'evil)
(evil-mode 1)


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

(evil-set-leader mk/states-wide (kbd "SPC"))
;; SPC is assigned to the function `evil-forward-char' in normal and motion states.
;; (evil-set-leader 'emacs (kbd "\\"))

;; magit
(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "SPC") 'evil-send-leader))

;; ----
;; helm
;; ----
(evil-define-key mk/states-wide 'global (kbd "<leader>l") 'helm-resume)
(define-key helm-map (kbd "C-w") 'backward-kill-word) ;it was `helm-yank-text-at-point'

;; ----
;; help
;; ----
(evil-define-key mk/states-wide 'global (kbd "<leader>hh") 'help)
(evil-define-key mk/states-wide 'global (kbd "<leader>ha") 'helm-apropos)

;; ---
;; M-x
;; ---
(evil-define-key mk/states-wide 'global (kbd "<leader>SPC") 'helm-M-x)

;; ----
;; file
;; ----
(evil-define-key mk/states-wide 'global (kbd "<leader>ff") 'helm-find-files)
(evil-define-key mk/states-wide 'global (kbd "<leader>fl") 'mk/locate-with-helm)
;; helm-find-files-map
;; (define-key helm-map (kbd "C-x h") 'helm-quit-and-find-file)
;; (define-key helm-map (kbd "C-x C-a") 'helm-ff-run-switch-to-eshell)


;; ------
;; buffer
;; ------
(evil-define-key mk/states-wide 'global (kbd "<leader>bs") 'save-buffer)
(evil-define-key mk/states-wide 'global (kbd "<leader>bl") 'ibuffer)
(evil-define-key mk/states-wide 'global (kbd "<leader>bb") 'helm-mini)
(evil-define-key mk/states-wide 'global (kbd "<leader>bn") 'bury-buffer)
(evil-define-key mk/states-wide 'global (kbd "<leader>bx") 'kill-current-buffer)
(evil-define-key mk/states-wide 'global (kbd "<leader>bX") 'kill-buffer-and-window)

;; ------
;; search
;; ------
(evil-define-key mk/states-wide 'global (kbd "<leader>so") 'helm-occur)
(evil-define-key mk/states-wide 'global (kbd "<leader>sO") 'helm-occur-visible-buffers)
(evil-define-key mk/states-wide 'global (kbd "<leader>si") 'helm-imenu)
(evil-define-key mk/states-wide 'global (kbd "<leader>sa") 'helm-do-grep-ag)
(evil-define-key mk/states-wide 'global (kbd "<leader>st") 'mk/find-tags)
(evil-define-key mk/states-wide 'global (kbd "<leader>sf") 'helm-find)

;; ---
;; avy
;; ---
(evil-define-key mk/states-narrow 'global (kbd "M-c") 'avy-goto-word-1)
(define-key global-map (kbd "M-l") 'avy-goto-line)

;; -----
;; dired
;; -----
(evil-define-key mk/states-wide 'global (kbd "<leader>dj") 'mk/dired-jump)


;; ------------------------
;; register, ring, and mark
;; ------------------------
(evil-define-key mk/states-wide 'global (kbd "<leader>rk") 'helm-show-kill-ring)
(evil-define-key mk/states-wide 'global (kbd "<leader>rr") 'helm-register)
(evil-define-key mk/states-wide 'global (kbd "<leader>rm") 'helm-all-mark-rings)
(evil-define-key mk/states-wide 'global (kbd "<leader>rj") 'mk/jump-to-register)
;; (evil-define-key mk/states-wide 'global (kbd "<leader> ms") 'mk/push-mark-no-activate)
;; (evil-define-key mk/states-wide 'global (kbd "<leader> md") 'kill-region)

;; ------
;; window
;; ------
(evil-define-key mk/states-wide 'global (kbd "<leader>w") 'evil-window-map)
(define-key evil-window-map "u" 'winner-undo)
(define-key evil-window-map "U" 'winner-redo)



;; ---
;; Git
;; ---
(evil-define-key mk/states-wide 'global (kbd "<leader>gs") 'magit-status)
(evil-define-key mk/states-wide 'global (kbd "<leader>gf") 'mk/fetch)
(evil-define-key mk/states-wide 'global (kbd "<leader>gb") 'magit-diff-buffer-file)

;; -------
;; project
;; -------
(evil-define-key mk/states-wide 'global (kbd "<leader>pg") 'mk/grep-project)
(evil-define-key mk/states-wide 'global (kbd "<leader>pb") 'mk/browse-project-buffers)
(evil-define-key mk/states-wide 'global (kbd "<leader>pf") 'mk/select-project-and-find-files)
(evil-define-key mk/states-wide 'global (kbd "<leader>ph") 'helm-browse-project-history)
;; helm-browse-project-map
;; helm-ls-git-buffer-map
;; helm-ls-git-map

;; -------
;; comment
;; -------
(evil-define-key mk/states-wide 'global (kbd "<leader>cl") 'comment-line)
(evil-define-key 'visual 'global (kbd "<leader>cr") 'comment-or-uncomment-region)

;; ----
;; edit
;; ----

;; ----
;; sexp
;; ----

(provide 'mk_evil)
;;; mk_evil.el ends here