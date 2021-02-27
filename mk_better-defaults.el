;;; mk_better-defaults.el --- Better defaults for Emacs

;;; Commentary:

;; 

;;; Code:


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
(load-theme 'zenburn t)

;; ----------
;; Frame look
;; ----------
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; ====
;; path
;; ====
(setenv "PATH" (concat "/home/makmiller/scripts/myscripts:/usr/bin/vendor_perl:/home/makmiller/.local/bin" ":" (getenv "PATH")))
(setenv "EDITOR" (concat "~/scripts/myscripts/edit.sh" (getenv "EDITOR")))
(setenv "VISUAL" (concat "~/scripts/myscripts/edit.sh" (getenv "VISUAL")))
(setenv "ALTERNATE_EDITOR" (concat "vim" (getenv "ALTERNATE_EDITOR")))

;; =====
;; shell
;; =====
(setq-default shell-file-name "/bin/zsh")	;default shell

(autoload 'mk/chama-ansi-term "mk_ansi-term" t nil)

;; TODO: Fix the func below for calling terminal
;; (autoload 'mk/eshell-popup "mk_eshell" t nil)

;; ========
;; avy-mode
;; ========
(ace-link-setup-default)		;; use avy to access links

(setq-default avy-keys (nconc (number-sequence ?a ?z)
			      (number-sequence ?A ?Z)
			      (number-sequence ?1 ?9)
			      '(?0)))

;; ===========
;; parenthesis
;; ===========
(show-paren-mode 1)
(electric-pair-mode 1) 			;pair parens automatically
(setq-default
 show-paren-delay 0	  		;disactivate delay when matching parentheses
 show-paren-style 'parenthesis)
;; The var 'show-paren-style' controls what gets highlighted. Possible
;; values: parenthesis, expression, and mixed


;; Enable the minor mode highlight-parentheses on all
;; buffers (from EmacsWiki):

;; (define-globalized-minor-mode global-highlight-parentheses-mode
;;   highlight-parentheses-mode
;;   (lambda ()
;;     (highlight-parentheses-mode t)))
;; (global-highlight-parentheses-mode t)

;; ============
;; dictionaries
;; ============

;; ---------
;; thesaurus
;; ---------
(setq-default synosaurus-choose-method 'popup) 	;instead of ido

;; --------
;; flyspell
;; --------
(setq-default ispell-personal-dictionary "~/elisp/.my-ispell-personal-dictionary")

;; --------
;; flycheck
;; --------
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(global-flycheck-mode)

;; ===========
;; auto-revert
;; ===========
(global-auto-revert-mode 1)		;reload file when it changes
(setq-default
 auto-revert-verbose nil
 global-auto-revert-non-file-buffers t)

;; ===========
;; epub viewer
;; ===========
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(setq-default nov-text-width 80)

;; ===========
;; Indentation
;; ===========
(global-aggressive-indent-mode 1)
;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)


;; ============
;; misc configs
;; ============
(add-hook 'text-mode-hook 'turn-on-visual-line-mode) ;visual-line-mode (word wrap)
;; (global-visual-line-mode 1)

;; Garbage collection
(setq gc-cons-threshold 100000000) 	; in bytes
;; The default amount was 800KB. If you specify a larger value,
;; garbage collection will happen less often. This reduces the amount
;; of time spent garbage collecting, but increases total memory use.
;; References:
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Garbage-Collection.html

(setq-default message-log-max t 		;max # lines message log
	      shift-select-mode nil 		;don't use shift to mark
	      indicate-empty-lines t ;show empty lines after buffer ends
	      initial-scratch-message nil
	      inhibit-splash-screen 0
	      column-number-mode t
	      echo-keystrokes 0.1 			;see unfinished commands
	      visible-bell t
	      sentence-end-double-space nil ;relevant for using M-k/e/a
	      enable-recursive-minibuffers t
	      case-fold-search nil)		;case sensitive search

(fset 'yes-or-no-p 'y-or-n-p)	   ;don't ask me to type "yes" or "no"

(global-subword-mode 1)
(blink-cursor-mode 1)
(global-font-lock-mode 1)
(global-hl-line-mode t)	      ;toggle line highlighting
(pending-delete-mode -1)
;; (mouse-avoidance-mode 'cat-and-mouse)


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

;; (setq emerge-diff-options "--ignore-all-space")
;; Emerge doesn't care about differences in whitespace

;; (defun mk/open-line-below ()
;;   (interactive)
;;   (end-of-line)
;;   (newline)
;;   (indent-for-tab-command))

;; (defun deleta-os-outros ()
;;   (interactive)
;;   (mapcar #'delete-frame (cdr (frame-list))))
;; Description: to delete all other frames when you're not on X -- for
;; C-x 5 1 only works if you are in X.

;; =====
;; ediff
;; =====

;;; list-colors-display sorted by hue
;; (setq list-colors-sort 'hsv)

;;; improved version of delete-blank-lines
;; (defun better-delete-lines (&optional arg)
;;   "Better `delete-blank-lines'."
;;   (interactive "P")
;;   (if (not (consp arg))
;;       (delete-blank-lines)
;;     (delete-blank-lines)
;;     (kill-visual-line)
;;     (when (not (bolp))
;;       (just-one-space)
;;       (fill-paragraph))))

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

;; C-h as backspace
(define-key key-translation-map [?\C-h] [?\C-?])

;; =====================
;; Common Lisp emulation
;; =====================
;; Resource:
;; https://www.gnu.org/software/emacs/manual/html_mono/cl.html

(provide 'mk_better-defaults)
;;; mk_better-defaults.el ends here