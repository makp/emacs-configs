;; FIXME: this file is a mess!!! 

;; The regular keyboard-translate doesn't work with Emacs daemon. You
;; should instead use (define-key key-translation-map ...). However
;; that breaks the prefix arg (bug #9417)

;;; SWAP SOME KEYS

(defun bug-do-emacs ()
  "Emacs' bug 9417. 'key-translation-map' does not work properly
with emacs --daemon "
  (interactive)

  (keyboard-translate ?\C-h ?\C-?)
  
  (keyboard-translate ?\C-t ?\C-p)
  (keyboard-translate ?\C-p ?\C-t)

  (keyboard-translate ?\C-u ?\C-x)
  (keyboard-translate ?\C-x ?\C-u))

(bug-do-emacs) 				; this won't work with emacs --daemon

(global-set-key (kbd "<f5>") 'bug-do-emacs)

;; (define-key key-translation-map (kbd "C-t") (kbd "C-p"))
;; (define-key key-translation-map (kbd "C-p") (kbd "C-t"))

;; (define-key key-translation-map [?\C-x] [?\C-u])
;; (define-key key-translation-map [?\C-u] [?\C-x])

(define-key key-translation-map (kbd "M-p") (kbd "M-t"))
(define-key key-translation-map (kbd "M-t") (kbd "M-p"))

;;; MY KEYS-MINOR-MODE-MAP
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(define-key my-keys-minor-mode-map (kbd "C-x p") ' sticky-window-delete-other-windows)

(define-key my-keys-minor-mode-map (kbd "C-x ,") 'split-window-below)
(define-key my-keys-minor-mode-map (kbd "C-x C-,") 'split-window-right)

(define-key my-keys-minor-mode-map (kbd "C-x C-t") 'delete-window)
(define-key my-keys-minor-mode-map (kbd "C-x y") 'delete-next-window)
(define-key my-keys-minor-mode-map (kbd "C-x C-y") 'delete-previous-window)

(define-key my-keys-minor-mode-map (kbd "C-.") 'execute-extended-command)
(define-key my-keys-minor-mode-map (kbd "C-M-t") 'backward-list)

(defun delete-next-window ()
  (interactive)
  (message "Deleting the next window...")
  (other-window 1)
  (delete-window))

(defun delete-previous-window ()
  (interactive)
  (message "Deleting the previous window...")
  (other-window -1)
  (delete-window))

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " D" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-minor-mode))
      (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

(provide 'mk_dvorak-mode)