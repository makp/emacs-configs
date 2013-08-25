;; FIXME: this file is a mess!!!

(defun bug-do-emacs ()
  "Emacs' bug. 'key-translation-map' does not work with emacs
--daemon"
  (interactive)

  (keyboard-translate ?\C-h ?\C-?)
  
  (keyboard-translate ?\C-t ?\C-p)
  (keyboard-translate ?\C-p ?\C-t)

  (keyboard-translate ?\C-u ?\C-x)
  (keyboard-translate ?\C-x ?\C-u))

(bug-do-emacs)

(global-set-key (kbd "<f5>") 'bug-do-emacs)

;;; Emacs bug
;; (define-key key-translation-map (kbd "C-t") (kbd "C-p"))
;; (define-key key-translation-map (kbd "C-p") (kbd "C-t"))

(define-key key-translation-map (kbd "M-p") (kbd "M-t"))
(define-key key-translation-map (kbd "M-t") (kbd "M-p"))

;; Swap “C-t” and “C-x”, so it's easier to type on Dvorak layout
;; (keyboard-translate ?\C-. ?\C-x)
;; (keyboard-translate ?\C-x ?\C-.)

;; (my-dvorak-translations)
;; (add-hook 'after-make-frame-functions 'my-dvorak-translations)

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(define-key my-keys-minor-mode-map (kbd "C-x p") ' sticky-window-delete-other-windows)

(define-key my-keys-minor-mode-map (kbd "C-x ,") 'split-window-below)
(define-key my-keys-minor-mode-map (kbd "C-x C-,") 'split-window-right)

(define-key my-keys-minor-mode-map (kbd "C-x C-t") 'delete-window)
(define-key my-keys-minor-mode-map (kbd "C-x y") 'delete-next-window)
(define-key my-keys-minor-mode-map (kbd "C-x C-y") 'delete-previous-window)

(define-key my-keys-minor-mode-map (kbd "C-.") 'helm-M-x)
(define-key my-keys-minor-mode-map (kbd "C-M-t") 'backward-list)

(define-key my-keys-minor-mode-map (kbd "C-,") 'universal-argument)

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