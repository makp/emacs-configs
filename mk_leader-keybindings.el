;;; mk_leader-keybindings.el --- Custom keybindngs -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(defvar mk/leader-states '(normal motion emacs)
  "Variable storing the states affected by the keybindings with the leader key.")

(evil-set-leader mk/leader-states (kbd "SPC"))
;; SPC is assigned to the function `evil-forward-char' in normal and motion states.

;; integration with magit
(with-eval-after-load 'magit
  (defvar magit-mode-map)
  (define-key magit-mode-map (kbd "SPC") 'evil-send-leader))

(evil-define-key mk/leader-states 'global
  (kbd "<leader>SPC") 'helm-M-x
  (kbd "<leader>l") 'helm-resume
  (kbd "<leader>h") 'help

  ;; file
  (kbd "<leader>ff") 'helm-find-files
  (kbd "<leader>fl") 'mk/locate-with-helm
  (kbd "<leader>bs") 'save-buffer

  ;; buffer
  (kbd "<leader>bl") 'ibuffer
  (kbd "<leader>bb") 'helm-mini
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
  (kbd "<leader>gf") 'mk/fetch
  (kbd "<leader>gb") 'magit-diff-buffer-file

  ;; project
  (kbd "<leader>pg") 'mk/grep-project
  (kbd "<leader>pb") 'mk/browse-project-buffers
  (kbd "<leader>pf") 'mk/select-project-and-find-files
  (kbd "<leader>ph") 'helm-browse-project-history

  ;; dic
  (kbd "<leader>dw") 'define-word-at-point
  (kbd "<leader>dt") 'synosaurus-lookup
  (kbd "<leader>da") 'flyspell-auto-correct-word
  (kbd "<leader>dc") 'ispell-word

  ;; orgmode
  (kbd "<leader>oa") 'org-agenda
  (kbd "<leader>oc") 'org-capture
  )

(provide 'mk_leader-keybindings)
;;; mk_leader-keybindings.el ends here