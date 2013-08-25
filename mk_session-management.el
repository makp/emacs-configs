;; ========
;; savehist
;; ========
(setq savehist-additional-variables
      '(search-ring regexp-search-ring kill-ring register-alist)
      savehist-file "~/.emacs.d/savehist")

(savehist-mode 1)

;; =======
;; recentf
;; =======
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 300)
(setq recent-save-file "~/.emacs.d/recentf")

;; =========
;; saveplace
;; =========
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")

(provide 'mk_session-management)
