(require 'erc)

;; -----------
;; autojoining
;; -----------

;; The first element is the IRC-server, the other elements are
;; channels. This will make ERC automatically join the channels when
;; it notices you're connected to the IRC-server.
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
      '((".*\\.freenode.net" "#emacs" "#org-mode" "#conkeror" "#archlinux")
 	(".*\\.irc.twice-irc.de" "#i3")))

(defun mk-erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?
      (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: start ERC
      (erc :server "irc.freenode.net" :port 6667 :nick "mmped")
      (erc :server "irc.twice-irc.de" :port 6667 :nick "mmped"))))

(setq erc-track-enable-keybindings nil)
;;; whether to enable the ERC track keybindings, namely: `C-c C-SPC'
;;; and `C-c C-@', which both do the same thing.

;; check channels
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				"324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(require 'erc-imenu)

(provide 'mk_erc)
