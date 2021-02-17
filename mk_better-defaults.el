;;; mk_better-defaults.el --- Better defaults for Emacs

;;; Commentary:

;; 

;;; Code:


;; =====================
;; Common Lisp emulation
;; =====================
;; Resource:
;; https://www.gnu.org/software/emacs/manual/html_mono/cl.html

;; (require 'cl-lib)
;; cl-macs

;; =======================
;; Enable some keybindings
;; =======================
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)	;C-x C-l
(put 'upcase-region 'disabled nil)	;C-x C-u
(put 'set-goal-column 'disabled nil) 	;C-x C-n
(put 'scroll-left 'disabled nil) 	;C-x <
(put 'suspend-frame 'disabled t)

;; ==========
;; Appearance
;; ==========

;; ----
;; Font
;; ----
(defun mk/default-font ()
  "Select font based on hostname."
  (interactive)
  (if (equal (system-name) "leibniz")
      (add-to-list 'default-frame-alist
		   '(font . "DejaVu Sans Mono-13"))
    (add-to-list 'default-frame-alist
		 '(font . "DejaVu Sans Mono-10"))))

(mk/default-font)

;; -----------
;; Color theme
;; -----------
;; (load-theme 'gruvbox-dark-soft t)
(load-theme 'zenburn t)

;; ----------
;; Frame look
;; ----------
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; =====
;; setqs
;; =====
;; Garbage collection
(setq gc-cons-threshold 100000000) 	; in bytes
;; The default amount was 800KB. If you specify a larger value,
;; garbage collection will happen less often. This reduces the amount
;; of time spent garbage collecting, but increases total memory use.
;; References: ;;
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Garbage-Collection.html

(setq message-log-max t) 		;max # lines message log
(setq case-fold-search nil)		;case sensitive search
(setq shell-file-name "/bin/zsh")	;default shell

(setenv "PATH" (concat "/home/makmiller/scripts/myscripts:/usr/bin/vendor_perl:/home/makmiller/.local/bin" ":" (getenv "PATH")))
(setenv "EDITOR" (concat "~/scripts/myscripts/edit.sh" (getenv "EDITOR")))
(setenv "VISUAL" (concat "~/scripts/myscripts/edit.sh" (getenv "VISUAL")))
(setenv "ALTERNATE_EDITOR" (concat "emacs" (getenv "ALTERNATE_EDITOR")))

(setq shift-select-mode nil) 		;don't use shift to mark

(set-default 'indicate-empty-lines t) ;show empty lines after buffer ends

(fset 'yes-or-no-p 'y-or-n-p)	   ;don't ask me to type "yes" or "no"

(setq
 initial-scratch-message nil
 inhibit-splash-screen 0
 column-number-mode t
 echo-keystrokes 0.1) 			;see unfinished commands

(setq visible-bell t)

(setq sentence-end-double-space nil) ;Relevant for using M-k/e/a

(setq enable-recursive-minibuffers t)
;; If this variable is nil, you cannot invoke minibuffer commands when
;; the minibuffer window is active, not even if you switch to another
;; window to do it.

;; ===========
;; Keybindings
;; ===========
(global-unset-key (kbd "C-x C-z")) 	;get rid of suspend-frame
(global-unset-key (kbd "C-x C-p")) 	;it was mark-page

;; Bind save-buffer to an easier keystroke
(global-set-key (kbd "M-s s") 'isearch-forward)
(global-set-key (kbd "C-s") 'save-buffer)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

(global-set-key (kbd "C-x f") 'mk/unfill-paragraph)

(global-set-key (kbd "C-c C") 'duplicate-current-line-or-region)

(global-set-key (kbd "M-C") 'subword-capitalize)

(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region) ; like in latex-mode

(global-set-key (kbd "C-x r q") 'save-buffers-kill-emacs)

(global-set-key (kbd "C-\\") 'eval-region)

;; (global-set-key (kbd "C-x C-;") ')
;; (global-set-key (kbd "C-x C-z") ')

;; ------
;; Macros
;; ------
(define-key ctl-x-map "." nil) ;;; I never use fill-prefix
(define-key ctl-x-map "." 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-x C-.") 'kmacro-end-or-call-macro)

;; (defun mk/repeat ()
;;   (interactive)
;;   (repeat 1)
;;   (set-temporary-overlay-map
;;    (let ((map (make-sparse-keymap)))
;;      (define-key map (kbd ".") 'mk/repeat)
;;      map)))


;; ====================================
;; Global minor modes shiped with Emacs
;; ====================================
(global-subword-mode 1)
(blink-cursor-mode 1)
(global-font-lock-mode 1)
(global-hl-line-mode t)	      ;toggle line highlighting
(pending-delete-mode -1)
;; (mouse-avoidance-mode 'cat-and-mouse)

(global-auto-revert-mode 1)		;reload file when it changes
(setq-default
 auto-revert-verbose nil
 global-auto-revert-non-file-buffers t)

(show-paren-mode 1)
(electric-pair-mode 1) 			;pair parens automatically
(setq-default
 show-paren-delay 0	  		;disactivate delay when matching parentheses
 show-paren-style 'mixed)
;; The var 'show-paren-style' controls what gets highlighted. Possible
;; values: parenthesis, expression, and mixed


;; ============================
;; Minor modes for text buffers
;; ============================
(add-hook 'text-mode-hook 'turn-on-visual-line-mode) ;visual-line-mode (word wrap)
;; (global-visual-line-mode 1)


;; =====================
;; Highlight parentheses
;; =====================
;; The code below enables the minor mode highlight-parentheses on all
;; buffers (from EmacsWiki):

;; (define-globalized-minor-mode global-highlight-parentheses-mode
;;   highlight-parentheses-mode
;;   (lambda ()
;;     (highlight-parentheses-mode t)))
;; (global-highlight-parentheses-mode t)


;; =========
;; undo-tree
;; =========
(global-undo-tree-mode)

;; ;; keep region when undoing in region
;; (defadvice undo-tree-undo (around keep-region activate)
;;   (if (use-region-p)
;;       (let ((m (set-marker (make-marker) (mark)))
;;             (p (set-marker (make-marker) (point))))
;;         ad-do-it
;;         (goto-char p)
;;         (set-mark m)
;;         (set-marker p nil)
;;         (set-marker m nil))
;;     ad-do-it))


;; ========
;; keychord
;; ========
(key-chord-mode 1)

(key-chord-define-global "hj" 'kill-whole-line)
(key-chord-define-global "kh" 'kill-paragraph)
(key-chord-define-global "cg" 'hippie-expand)

;; Available keychords: "uu", "UU", "<<", ">>", "DD", "GG"


;; ========
;; avy-mode
;; ========
(define-key global-map (kbd "M-l") 'avy-goto-line)
(global-set-key (kbd "M-L") 'downcase-dwim)

(setq-default avy-keys (nconc (number-sequence ?a ?z)
			      (number-sequence ?A ?Z)
			      (number-sequence ?1 ?9)
			      '(?0)))

(key-chord-define-global "hh" 'avy-goto-word-1)
(key-chord-define-global "HH" 'avy-goto-char-timer)


;; ========
;; doc-view
;; ========
;; (setq doc-view-continuous t)
;; (setq doc-view-resolution 250)
;; (setq doc-view-image-width 1250)
;;; (setq doc-view-cache-directory "/tmp/docview1000")

;; Multiple async processes
;; (defadvice shell-command (after shell-in-new-buffer (command &optional output-buffer error-buffer))
;;   (when (get-buffer "*Async Shell Command*")
;;     (with-current-buffer "*Async Shell Command*"
;;       (rename-uniquely))))
;; (ad-activate 'shell-command)

;; What is exactly the difference between set-face-attribute and
;; modify-face?

(setq emerge-diff-options "--ignore-all-space")
;; Emerge doesn't care about differences in whitespace

;; (defun mk/open-line-below ()
;;   (interactive)
;;   (end-of-line)
;;   (newline)
;;   (indent-for-tab-command))

;; (global-set-key (kbd "C-c SPC") 'mk/open-line-below)

;; (defun deleta-os-outros ()
;;   (interactive)
;;   (mapcar #'delete-frame (cdr (frame-list))))
;; Description: to delete all other frames when you're not on X -- for
;; C-x 5 1 only works if you are in X.

;; ===========
;; Indentation
;; ===========

;; -----------------
;; aggressive-indent
;; -----------------
(global-aggressive-indent-mode 1)
;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)


;; =====
;; ediff
;; =====

;;; list-colors-display sorted by hue
(setq list-colors-sort 'hsv)

;;; improved version of delete-blank-lines
(defun better-delete-lines (&optional arg)
  "Better `delete-blank-lines'."
  (interactive "P")
  (if (not (consp arg))
      (delete-blank-lines)
    (delete-blank-lines)
    (kill-visual-line)
    (when (not (bolp))
      (just-one-space)
      (fill-paragraph))))

(global-set-key (kbd "C-x C-o") 'better-delete-lines)

;; ====
;; Tags
;; ====
;; (setq tags-table-list
;;       '("/home/makmiller/elisp/agendas"))
;; Look at the TAGS files in these directories.
;; I don't need to set this up if I'm using helm for selecting tags

;; ===
;; gpg
;; ===
;; (require 'epa-file)
;; (epa-file-enable)

;; ;; ===========
;; ;; lock screen
;; ;; ===========
;; (defun mk-lock-screen ()
;;   (interactive)
;;   (async-shell-command "sudo openvt -sw -- vlock -a"))

;; (global-set-key (kbd "\e\el") 
;; 		(lambda ()
;; 		  (interactive)
;; 		  (mk-lock-screen)
;; 		  (winner-undo)))

;; =========
;; join-line
;; =========
;; (defun top-join-line ()
;;   "Join the current line with the line beneath it."
;;   (interactive)
;;   (delete-indentation 1))

;; (global-set-key (kbd "M-^") 'top-join-line)
;; (global-set-key (kbd "C-^") 'delete-indentation)

;; ===========
;;  split-line
;; ===========
;; (global-set-key  'split-line)
;; (global-set-key  'open-line)

;; epub viewer (nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(setq-default nov-text-width 80)


;; ============
;; dictionaries
;; ============

(global-set-key (kbd "<f9> d") 'define-word-at-point)

;; ---------
;; thesaurus
;; ---------
(setq-default synosaurus-choose-method 'popup) 	;instead of ido
(global-set-key (kbd "<f9> l") 'synosaurus-lookup)


;; --------
;; flyspell
;; --------
(setq-default flyspell-auto-correct-binding (kbd "C-'")
	      ispell-personal-dictionary "~/elisp/.my-ispell-personal-dictionary")

(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "C-,") nil))



;; ========
;; flycheck
;; ========
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(global-flycheck-mode)


;; ------
;; Abbrev
;; ------
;; (setq abbrev-file-name
;;       "~/elisp/cache/abbrev-defs")

;; Description: abbrev file

;; (setq save-abbrevs t)
;; Description: save abbrevs when files are saved you will be asked
;; before the abbreviations are saved

;; (quietly-read-abbrev-file)
;; Description: reads the abbreviations file on startup

;; (global-set-key (kbd "C-c SPC") ')


(provide 'mk_better-defaults)
;;; mk_better-defaults.el ends here
