;;; mk_keybindings-dvorak.el --- Custom global keybindings for Dvorak keyboard

;;; Commentary:

;; This custom keybindings are suppose to facilitate the use of the
;; Dvorak keyboard with Emacs.

;;; Code:


;; Swap some key bindings

;; 

;; (define-key key-translation-map (kbd "M-p") (kbd "M-t"))
;; (define-key key-translation-map (kbd "M-t") (kbd "M-p"))

;; (define-key key-translation-map (kbd "C-x") (kbd "C-u"))
;; (define-key key-translation-map (kbd "C-u") (kbd "C-x"))

;; (define-key key-translation-map (kbd "C-t") (kbd "C-p"))
;; (define-key key-translation-map (kbd "C-p") (kbd "C-t"))


;; ========================
;; Change a few keybindings
;; ========================



;; (defvar my-keys-minor-mode-map (make-keymap) "Define a couple of
;; global keybindings to be used with a Dvorak keyboard.")


;; (define-key my-keys-minor-mode-map (kbd "C-x ,") 'split-window-below)
;; (define-key my-keys-minor-mode-map (kbd "C-x C-,") 'split-window-right)


;; (define-key my-keys-minor-mode-map (kbd "C-x y") 'delete-next-window)
;; (define-key my-keys-minor-mode-map (kbd "C-x C-y") 'delete-previous-window)

;; (define-key my-keys-minor-mode-map (kbd "C-.") 'execute-extended-command)
;; (define-key my-keys-minor-mode-map (kbd "C-M-t") 'backward-list)

;; (defun delete-next-window ()
;;   (interactive)
;;   (message "Deleting the next window...")
;;   (other-window 1)
;;   (delete-window))

;; (defun delete-previous-window ()
;;   (interactive)
;;   (message "Deleting the previous window...")
;;   (other-window -1)
;;   (delete-window))

;; (define-minor-mode my-keys-minor-mode
;;   "A minor mode so that my key settings override annoying major modes."
;;   t " D" 'my-keys-minor-mode-map)

;; (my-keys-minor-mode 1)

;; (defadvice load (after give-my-keybindings-priority)
;;   "Try to ensure that my keybindings always have priority."
;;   (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-minor-mode))
;;       (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
;;         (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
;;         (add-to-list 'minor-mode-map-alist mykeys))))
;; (ad-activate 'load)


(provide 'mk_keybindings-dvorak)

;;; mk_keybindings-dvorak.el ends here
