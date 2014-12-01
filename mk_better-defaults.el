;; =====
;; setqs
;; =====
(setq message-log-max t) 		; max # lines message log
(setq case-fold-search nil)		; case sensitive search
(setq shell-file-name "/bin/zsh")

(setq shift-select-mode nil) 		; don't use shift to mark
(set-default 'indicate-empty-lines t) 	; show empty lines after
					; buffer end
(global-subword-mode 1)

;; getting rid of suspend-frame
(global-unset-key (kbd "C-x C-z"))
(put 'suspend-frame 'disabled t)
(global-set-key (kbd "C-x C-z") 'repeat-complex-command)

;;; C-, as universal argument
(global-set-key (kbd "C-,") 'universal-argument)
(define-key universal-argument-map (kbd "C-,") ' universal-argument-more)
;;; http://stackoverflow.com/questions/4808756/how-do-you-move-the-prefix-argument-to-a-different-key-in-emacs/4809193#4809193

;;; solving some conflicts
(add-hook 'flyspell-mode-hook
	  (lambda ()
	    (define-key flyspell-mode-map (kbd "C-,") nil)))

(add-hook 'org-mode-hook
	  (lambda ()
	    (define-key org-mode-map (kbd "C-,") nil)))  ; it was org-cycle-agenda-files


;; =======================
;; General display options
;; =======================

;; ----------
;; Frame look
;; ----------
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(display-time)
(setq-default visible-bell)
(setq
 initial-scratch-message nil 
 inhibit-splash-screen 0 
 column-number-mode t
 echo-keystrokes 0.1) 			;see what you're typing


;; Note: you can choose your inial buffer with (initial-buffer-choice
;; ...)

(global-unset-key (kbd "C-x C-p"))

;; --------------
;; font-lock-mode
;; --------------
(global-font-lock-mode 1)

;; --------------
;; mouse behavior
;; --------------
(setq mouse-avoidance-mode 'banish)
;; Description: move the mouse to the upper-right corner on any key
;; press

;; ------------
;; line display
;; ------------
;; (setq-default global-visual-line-mode t)
;; Description: enables "word wrapping". This mode redefines C-a C-n
;; and C-k to operate on screen lines rather than logical lines

(setq-default truncate-lines t)

;; hl-line-mode
(global-hl-line-mode t)

(defadvice hi-lock-set-pattern (around use-overlays activate)
  (let ((font-lock-fontified nil))
    ad-do-it))
;;; to make hl-line-mode compatible with hi-lock-mode

;; ==========
;; navigation
;; ==========

;; -------------------
;; search with regexps
;; -------------------
(global-set-key [(control s)] 'isearch-forward-regexp)
(global-set-key [(control r)] 'isearch-backward-regexp)
(global-set-key [(meta %)] 'query-replace-regexp) 

;; ===========
;; Minibuffers
;; ===========
(setq enable-recursive-minibuffers t)
;; Turn recursive editing in the mini buffer.

(fset 'yes-or-no-p 'y-or-n-p) ; so I don't have to type "yes" or "no"

(if (require 'miniedit nil t)
    (miniedit-install))
;; Description: binds C-M-e in a minibuffer so that you can edit the
;; contents of the minibuffer before submitting it.

;; =========
;; undo-tree
;; =========
(require 'undo-tree)

(global-undo-tree-mode)

;; keep region when undoing in region
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))

;; =======
;; Buffers
;; =======
(global-set-key (kbd "C-x n")   'other-window)
(global-set-key (kbd "C-x C-n") 'bury-buffer)
(global-set-key (kbd "C-x C-;") 'set-goal-column)

;; -----------
;; auto-revert
;; -----------
(global-auto-revert-mode t) ;; Description: enable reloading when the
			    ;; file changes.
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; --------
;; uniquify
;; --------
(require 'uniquify) 
(setq 
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":")

;; ---------
;; redisplay
;; ---------
(setq redisplay-dont-pause t)

;; ========
;; keychord
;; ========
(require 'key-chord)
(key-chord-mode 1)

(key-chord-define-global "uu" 'fill-paragraph)
(key-chord-define-global "UU" 'mk/unfill-paragraph)

(key-chord-define-global "hj" 'kill-whole-line)
(key-chord-define-global "kh" 'kill-paragraph)

(key-chord-define-global "<<" 'beginning-of-buffer)
(key-chord-define-global ">>" 'end-of-buffer)

;; (key-chord-define-global "DD" '(lambda ()
;; 				 (interactive)
;; 				 (kill-buffer nil)))

;; (key-chord-define-global "GG" '(lambda ()
;; 				 (interactive)
;; 				 (let ((current-prefix-arg '(4)))
;; 				   (call-interactively 'magit-status))))

(key-chord-define-global "cg" 'hippie-expand)


;; ========
;; ace-jump
;; ========
(require 'ace-jump-mode)
(setq ace-jump-mode-scope 'window)

;;; don't ignore case
(setq ace-jump-mode-case-fold nil)

(define-key global-map (kbd "M-g") 'ace-jump-line-mode)
(global-set-key (kbd "M-s g") 'goto-line)

(key-chord-define-global "hh" 'ace-jump-mode)
(key-chord-define-global "HH" 'ace-jump-char-mode)


;; ========================
;; duplicate line or region
;; ========================
(global-set-key (kbd "C-c C") 'duplicate-current-line-or-region)

;; ========
;; Macros!!
;; ========
(define-key ctl-x-map "." nil) ;;; I never use fill-prefix
(define-key ctl-x-map "." 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-x C-.") 'kmacro-end-or-call-macro)

(global-set-key (kbd "C-x C-r") 'repeat)   ; it was find-file-read-only

;; (defun mk/repeat ()
;;   (interactive)
;;   (repeat 1)
;;   (set-temporary-overlay-map
;;    (let ((map (make-sparse-keymap)))
;;      (define-key map (kbd ".") 'mk/repeat)
;;      map)))

(global-set-key (kbd "M-C") 'subword-capitalize)

(global-set-key (kbd "C-x k") '(lambda ()
			       (interactive)
			       (kill-buffer nil)))

;;; Remember that Emacs natively support these commands:
;;; |-----------+-------------------------|
;;; | C-x C-k n | kmacro-name-last-macro  |
;;; | C-x C-k b | kmacro-bind-to-key      |
;;; | C-x q     | query                   |
;;; |-------------+-----------------------|
;;; | C-x C-k C-i | kmacro-insert-counter |
;;; | C-x C-k C-c | kmacro-set-counter    |
;;; | C-x C-k C-a | kmacro-add-counter    |
;;; | C-x C-k C-f | kmacro-set-format     |
;;; |-------------+-----------------------|

;; ---------
;; sentences
;; ---------
(setq sentence-end-double-space nil)
;;; this is relevant for using M-k/e/a

;; --------------
;; zap-up-to-char
;; --------------
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.
  
  \(fn arg char)"
  'interactive)

(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; ------------------
;; custom keybindings
;; ------------------
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; ===========
;; Parentheses
;; ===========
(show-paren-mode 1)

(setq
 show-paren-delay 0	     ; disactivates delay matching parentheses
 show-paren-style 'parenthesis) ; values: parenthesis, expression, and
					; mixed 
(global-set-key (kbd "C-c p") "(")
(global-set-key (kbd "C-c y") "[")   
(global-set-key (kbd "C-c f") "{")

;; --------
;; autopair
;; --------
(require 'autopair)
(autopair-global-mode)

;; ---------------
;; some keybidings
;; ---------------
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region) ; like in latex-mode
(global-set-key (kbd "C-x r q") 'save-buffers-kill-emacs)

;; =====
;; evals
;; =====
(global-set-key (kbd "C-\\") 'eval-region)

(define-key ctl-x-map "e" nil)
(define-key ctl-x-map "e" 'replace-last-sexp)

(defun replace-last-sexp ()
    (interactive)
    (let ((value (eval (preceding-sexp))))
      (kill-sexp -1)
      (insert (format "%S" value))))


;; ====
;; Fill
;; ====

;; ---------
;; auto-fill
;; ---------
(add-hook 'text-mode-hook 'turn-on-auto-fill) ; enable auto-fill-mode
;; Description: activate Auto-Fill mode for all text mode buffers.
;; "Auto Fill" mode is a minor mode in which lines are broken
;; automatically when they become too wide. Breaking happens only when
;; you type a <SPC> or <RET>.

;; (global-set-key (kbd "C-c SPC") 'fill-region-as-paragraph)

;; ==============================
;; Spell checker and dictionaries
;; ==============================
;
;; --------
;; Flyspell
;; --------
(setq flyspell-sort-corrections nil)
;; Description: use likeness rather than alphabetical ordering with

(setq flyspell-auto-correct-binding (kbd "C-'"))

;; ------------------------
;; Personal dictionary path
;; ------------------------
(setq ispell-personal-dictionary "~/elisp/.my-ispell-personal-dictionary")

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

;; -------------
;; Doc-view mode
;; -------------
(setq doc-view-continuous t)
(setq doc-view-resolution 250)
(setq doc-view-image-width 1250)
;;; (setq doc-view-cache-directory "/tmp/docview1000")

;; Multiple async processes 
(defadvice shell-command (after shell-in-new-buffer (command &optional output-buffer error-buffer))
  (when (get-buffer "*Async Shell Command*")
    (with-current-buffer "*Async Shell Command*"
      (rename-uniquely))))
(ad-activate 'shell-command)

;; What is exactly the difference between set-face-attribute and
;; modify-face?

(setq emerge-diff-options "--ignore-all-space")
;; Description: emerge doesn't care about differences in whitespace

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

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

;; =========================
;; Enabling some keybindings
;; =========================
(put 'narrow-to-region 'disabled nil)	; 
(put 'downcase-region 'disabled nil)	; `C-x C-l'
(put 'upcase-region 'disabled nil)	; `C-x C-u'
(put 'set-goal-column 'disabled nil) 	; `C-x C-n'
(put 'scroll-left 'disabled nil) 	; `C-x <'

;; =====
;; ediff
;; =====

;;; list-colors-display sorted by hue
(setq list-colors-sort 'hsv )

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
(setq tags-table-list
      '(;; 	"~/Documents/mydocs/"
	"~/config-files/general/emacs-config/"))
;; Look at the TAGS files in these directories.

(pending-delete-mode t)

;; (global-set-key (kbd "C-j") nil)
;; (local-set-key (kbd "C-j") 'universal-argument)

;; ===
;; gpg
;; ===
;; (require 'epa-file)
;; (epa-file-enable)

;; ===========
;; lock screen
;; ===========
(defun mk-lock-screen ()
  (interactive)
  (async-shell-command "sudo openvt -sw -- vlock -a"))

(global-set-key (kbd "\e\el") 
		(lambda ()
		  (interactive)
		  (mk-lock-screen)
		  (winner-undo)))

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

;; ============
;; Line spacing
;; ============
(setq lista-espacos (list 0 0.35 0.5 0.75)	;; list of different values
      ;; for line-spacing
      contador 1				;; the default value
      comprimento (length lista-espacos))

(setq-default line-spacing (nth contador lista-espacos))   ;; declaring the default value for line spacing

(provide 'mk_better-defaults)
;;; mk-better-defaults.el ends here