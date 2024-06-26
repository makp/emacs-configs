;;; mk_leader-keybindings.el --- Custom keybindngs -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

;; (require 'evil)

(evil-define-key nil 'global
  (kbd "<leader>h") 'help

  ;; file
  (kbd "<leader>ff") 'helm-find
  (kbd "<leader>fl") 'mk/locate-with-helm
  (kbd "<leader>fy") 'mk/copy-absolute-filename

  ;; buffer
  (kbd "<leader>bb") 'helm-mini
  (kbd "<leader>bp") 'previous-buffer
  (kbd "<leader>bn") 'bury-buffer
  (kbd "<leader>bx") 'kill-current-buffer
  (kbd "<leader>bX") 'kill-buffer-and-window
  (kbd "<leader>bq") 'save-buffers-kill-terminal
  
  ;; search
  (kbd "<leader>so") 'helm-occur
  (kbd "<leader>sO") 'helm-occur-visible-buffers
  (kbd "<leader>si") 'helm-imenu
  (kbd "<leader>sg") 'helm-do-grep-ag
  (kbd "<leader>st") 'mk/find-tags

  ;; register, ring, and mark
  (kbd "<leader>rr") 'helm-register
  (kbd "<leader>rk") 'helm-show-kill-ring
  (kbd "<leader>rm") 'helm-all-mark-rings

  ;; git
  (kbd "<leader>gs") 'magit-status 	; use prefix arg to choose repo
  (kbd "<leader>gd") 'magit-diff-buffer-file
  (kbd "<leader>gf") 'mk/magit-fetch
  (kbd "<leader>gF") 'mk/select-git-repo-and-fetch-from-all-remotes
  (kbd "<leader>gg") 'helm-grep-do-git-grep ; accepts prefix arg
  (kbd "<leader>gb") 'helm-browse-project ; accepts prefix arg
  (kbd "<leader>gl") 'mk/list-git-status-of-open-buffers


  ;; LSP
  (kbd "<leader>ld") 'lsp-find-definition
  (kbd "<leader>lr") 'lsp-rename
  (kbd "<leader>lx") 'lsp-find-references


  ;; project
  (kbd "<leader>pp") 'helm-projectile-switch-project
  (kbd "<leader>po") 'projectile-switch-open-project
  (kbd "<leader>pf") 'helm-projectile-find-file
  (kbd "<leader>pF") 'helm-projectile-find-file-in-known-projects
  (kbd "<leader>pb") 'helm-projectile-switch-to-buffer
  (kbd "<leader>ps") 'projectile-vc
  (kbd "<leader>pd") 'helm-projectile-find-dir
  (kbd "<leader>pD") 'projectile-dired
  (kbd "<leader>pt") 'projectile-find-tag
  (kbd "<leader>pr") 'projectile-replace
  (kbd "<leader>px") 'projectile-find-references
  (kbd "<leader>pg") 'helm-projectile-rg
  (kbd "<leader>pa") 'projectile-run-term


  ;; orgmode - agendas
  (kbd "<leader>aa") 'org-agenda
  (kbd "<leader>ao") 'mk/select-agenda
  (kbd "<leader>ac") 'org-capture

  (kbd "<leader>att") 'org-timer-set-timer
  (kbd "<leader>atx") 'org-timer-stop

  (kbd "<leader>ati") 'mk/clock-in
  (kbd "<leader>atI") 'mk/quick-clockin
  (kbd "<leader>ato") 'org-clock-out
  (kbd "<leader>atw") 'org-clock-goto

  ;; completion
  (kbd "<leader>cc") 'company-other-backend
  (kbd "<leader>cy") 'company-yasnippet
  (kbd "<leader>cf") 'company-files

  ;; text operations
  (kbd "<leader>tw") 'mk/write
  ;; (kbd "<leader>tw") 'mk/code

  ;; open X
  (kbd "<leader>of") 'helm-find-files
  (kbd "<leader>os") 'mk/open-org-scratch
  (kbd "<leader>oc") 'mk/chatgpt-select-model
  (kbd "<leader>oa") 'mk/call-ansi-term
  (kbd "<leader>od") 'dired-jump)



(provide 'mk_leader-keybindings)
;;; mk_leader-keybindings.el ends here
