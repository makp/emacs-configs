;;; mk_session-management.el --- Custom config for session management

;;; Commentary:

;; 

;;; Code:


;; ========
;; savehist
;; ========
(setq savehist-additional-variables
      '(search-ring regexp-search-ring kill-ring register-alist)
      savehist-file "~/.emacs.d/savehist")

(savehist-mode 1)

(setq history-delete-duplicates t)

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
(save-place-mode 1)
(setq save-place-file "~/.emacs.d/saved-places")
(setq save-place-forget-unreadable-files nil)

(provide 'mk_session-management)

;;; mk_session-management.el ends here