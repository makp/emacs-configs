;;; mk_leader-keybindings.el --- Custom keybindngs -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(defvar mk/leader-states '(normal motion emacs)
  "Variable storing the states affected by the keybindings with the leader key.")

;; ----------
;; Leader key
;; ----------
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
  (kbd "<leader>bx") 'kill-current-buffer
  (kbd "<leader>bX") 'kill-buffer-and-window
  
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
  (kbd "<leader>pg") 'mk/grep-project

  ;; project
  (kbd "<leader>pb") 'mk/browse-project-buffers
  (kbd "<leader>pf") 'mk/select-project-and-find-files
  (kbd "<leader>ph") 'helm-browse-project-history

  ;; dic
  (kbd "<leader>dw") 'define-word-at-point
  (kbd "<leader>dt") 'synosaurus-lookup
  (kbd "<leader>dc") 'flyspell-auto-correct-word)


;; ----
;; edit
;; ----
;; (global-set-key (kbd "C-x f") 'mk/unfill-paragraph)

;; ----
;; sexp
;; ----



(provide 'mk_leader-keybindings)
;;; mk_leader-keybindings.el ends here