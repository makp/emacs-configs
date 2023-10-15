;;; mk_leader-keybindings.el --- Custom keybindngs -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

;; (require 'evil)

(evil-define-key nil 'global
  ;; (kbd "<leader><leader>") 'helm-M-x
  (kbd "<leader>l") 'helm-resume
  (kbd "<leader>h") 'help

  ;; file
  (kbd "<leader>ff") 'helm-find-files
  (kbd "<leader>fl") 'mk/locate-with-helm

  ;; dired
  (kbd "<leader>dp") 'mk/copy-absolute-filename

  ;; buffer
  (kbd "<leader>bw") 'save-buffer
  (kbd "<leader>bb") 'helm-mini
  (kbd "<leader>bp") 'previous-buffer
  (kbd "<leader>bn") 'bury-buffer
  (kbd "<leader>bx") 'kill-current-buffer
  (kbd "<leader>bX") 'kill-buffer-and-window
  (kbd "<leader>bq") 'save-buffers-kill-emacs
  
  ;; search
  (kbd "<leader>so") 'helm-occur
  (kbd "<leader>sO") 'helm-occur-visible-buffers
  (kbd "<leader>si") 'helm-imenu
  (kbd "<leader>sa") 'helm-do-grep-ag
  (kbd "<leader>st") 'mk/find-tags
  (kbd "<leader>sf") 'helm-find

  ;; register, ring, and mark
  (kbd "<leader>rr") 'helm-register
  (kbd "<leader>rk") 'helm-show-kill-ring
  (kbd "<leader>rm") 'helm-all-mark-rings

  ;; git
  (kbd "<leader>gs") 'magit-status
  (kbd "<leader>gf") 'mk/magit-fetch
  (kbd "<leader>gg") 'helm-grep-do-git-grep ; accepts prefix arg
  (kbd "<leader>gd") 'magit-diff-buffer-file
  (kbd "<leader>gb") 'helm-browse-project ; accepts prefix arg

  ;; project
  (kbd "<leader>pp") 'magit-list-repositories
  (kbd "<leader>pl") 'mk/list-git-status-of-open-buffers
  (kbd "<leader>ps") 'mk/select-project-and-display-status
  (kbd "<leader>pg") 'mk/select-project-and-run-git-grep
  (kbd "<leader>pa") 'mk/selection-project-and-run-ag
  (kbd "<leader>pb") 'mk/select-project-and-browse-buffers
  (kbd "<leader>pF") 'mk/select-project-and-fetch-from-all-remotes
  (kbd "<leader>pf") 'mk/select-project-and-run-find

  ;; orgmode - agendas
  (kbd "<leader>aa") 'org-agenda
  (kbd "<leader>ao") 'mk/select-agenda
  (kbd "<leader>ac") 'org-capture

  (kbd "<leader>at") 'org-timer-set-timer
  (kbd "<leader>ax") 'org-timer-stop
  (kbd "<leader>ai") 'mk/clock-in
  (kbd "<leader>aI") 'mk/quick-clockin	;mk/clock-in
  (kbd "<leader>ao") 'org-clock-out
  (kbd "<leader>aw") 'org-clock-goto

  ;; completion
  (kbd "<leader>cy") 'company-yasnippet
  (kbd "<leader>cf") 'company-files

  ;; text operations
  (kbd "<leader>tw") 'mk/write
  ;; (kbd "<leader>tw") 'mk/code

  ;; open ...
  (kbd "<leader>os") 'mk/open-org-scratch
  (kbd "<leader>oc") 'mk/chatgpt-select-model
  (kbd "<leader>oa") 'mk/call-ansi-term)

(provide 'mk_leader-keybindings)
;;; mk_leader-keybindings.el ends here
