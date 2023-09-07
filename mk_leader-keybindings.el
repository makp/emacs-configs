;;; mk_leader-keybindings.el --- Custom keybindngs -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(defvar mk/leader-states '(normal motion emacs)
  "Variable storing the states affected by the keybindings with the leader key.")

(evil-set-leader mk/leader-states (kbd "SPC"))
;; SPC is assigned to the function `evil-forward-char' in normal and motion states.


(evil-define-key mk/leader-states 'global
  (kbd "<leader>SPC") 'helm-M-x
  (kbd "<leader>l") 'helm-resume
  (kbd "<leader>h") 'help

  ;; file
  (kbd "<leader>ff") 'helm-find-files
  (kbd "<leader>fl") 'mk/locate-with-helm

  ;; buffer
  (kbd "<leader>bs") 'save-buffer
  (kbd "<leader>bl") 'ibuffer
  (kbd "<leader>bb") 'helm-mini
  (kbd "<leader>bB") 'previous-buffer
  (kbd "<leader>bn") 'bury-buffer
  (kbd "<leader>bc") 'kill-current-buffer
  (kbd "<leader>bC") 'kill-buffer-and-window
  (kbd "<leader>bq") 'save-buffers-kill-emacs
  
  ;; search
  (kbd "<leader>so") 'helm-occur
  (kbd "<leader>sO") 'helm-occur-visible-buffers
  (kbd "<leader>si") 'helm-imenu
  (kbd "<leader>sa") 'helm-do-grep-ag
  (kbd "<leader>st") 'mk/find-tags
  (kbd "<leader>sf") 'helm-find

  ;; ring, mark, and register
  (kbd "<leader>rk") 'helm-show-kill-ring
  (kbd "<leader>rr") 'helm-register
  (kbd "<leader>rm") 'helm-all-mark-rings
  (kbd "<leader>rj") 'mk/jump-to-register

  ;; window
  (kbd "<leader>w") 'evil-window-map

  ;; git
  (kbd "<leader>gs") 'magit-status
  (kbd "<leader>gf") 'mk/magit-fetch
  (kbd "<leader>gg") 'helm-grep-do-git-grep ; accepts prefix arg
  (kbd "<leader>gd") 'magit-diff-buffer-file
  (kbd "<leader>gb") 'helm-browse-project ; accepts prefix arg

  ;; project
  (kbd "<leader>ph") 'helm-projects-history
  (kbd "<leader>pg") 'mk/select-project-and-run-git-grep
  (kbd "<leader>pb") 'mk/select-project-and-browse-buffers
  (kbd "<leader>pF") 'mk/select-project-and-fetch-from-all-remotes
  (kbd "<leader>pf") 'mk/select-project-and-find-files

  ;; orgmode - agendas
  (kbd "<leader>aa") 'org-agenda
  (kbd "<leader>ao") 'mk/select-agenda
  (kbd "<leader>ac") 'org-capture

  (kbd "<leader>ct") 'org-timer-set-timer
  (kbd "<leader>cx") 'org-timer-stop
  (kbd "<leader>ci") 'mk/clock-in
  (kbd "<leader>cI") 'mk/quick-clockin	;mk/clock-in
  (kbd "<leader>co") 'org-clock-out
  (kbd "<leader>cw") 'org-clock-goto

  ;; helpful buffers/files
  (kbd "<leader>os") 'mk/open-org-scratch-buffer
  (kbd "<leader>oc") 'mk/chatgpt

  ;; terminal
  (kbd "<leader>ta") 'mk/call-ansi-term)

(provide 'mk_leader-keybindings)
;;; mk_leader-keybindings.el ends here
