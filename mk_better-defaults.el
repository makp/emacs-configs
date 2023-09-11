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

;; ------------
;; line numbers
;; ------------
(setq display-line-numbers-type 'relative)


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

;; ==========
;; completion
;; ==========
;; (setq completion-styles '(flex))
;; NOTE: flex completion didn't work well with company last time I tried (it gave me too many options).


;; ==============================
;; highlight keywords in comments
;; ==============================
(require 'hl-todo)
(global-hl-todo-mode 1)
(setcdr (assoc "TODO" hl-todo-keyword-faces) "green")

;; ===========
;; parenthesis
;; ===========
(show-paren-mode 1)
(setq-default
 show-paren-delay 0	  		;disactivate delay when matching parentheses
 show-paren-style 'parenthesis)
;; The var 'show-paren-style' controls what gets highlighted. Possible
;; values: parenthesis, expression, and mixed
(electric-pair-mode 1)

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

;; ===
;; web
;; ===
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

;; ===========
;; mathematica
;; ===========
(add-to-list 'auto-mode-alist '("\.wl$" . wolfram-mode))

;; ===========
;; Indentation
;; ===========
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'wolfram-mode)

;; ============
;; prog buffers
;; ============
(add-hook 'prog-mode-hook
	  (lambda ()
	    (highlight-parentheses-mode 1)
	    (setq truncate-lines 1)
	    (company-mode 1)))

;; ============
;; text buffers
;; ============
(add-hook 'text-mode-hook
	  (lambda ()
	    (turn-on-visual-line-mode)
	    (setq line-spacing 1)
	    (company-mode 1)))

;; (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)

;; ==================
;; Garbage collection
;; ==================
;; garbage collection
;; set garbage collection threshold to 100MB
(setq gc-cons-threshold 100000000)

;; make sure GC is more frequent once startup is done
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 800000)))

;; improve startup time by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))


;; ====
;; misc
;; ====

;; Misc defaults
(setq-default message-log-max t		; max # lines message log
	      shift-select-mode nil	; don't use shift to mark
	      indicate-empty-lines t ;show empty lines after buffer ends
	      initial-scratch-message nil
	      inhibit-splash-screen 0
	      column-number-mode t
	      echo-keystrokes 0.1	; see unfinished commands
	      visible-bell t
	      sentence-end-double-space nil ; single space ends a sentence
	      enable-recursive-minibuffers t
	      case-fold-search nil)		;case sensitive search

;; Misc settings
(fset 'yes-or-no-p 'y-or-n-p)	   ;don't ask me to type "yes" or "no"
(global-subword-mode 1)
(blink-cursor-mode 1)
(global-font-lock-mode 1)
(global-hl-line-mode t)	      ;toggle line highlighting
(pending-delete-mode -1)

(put 'narrow-to-region 'disabled nil)

(setq browse-url-browser-function 'browse-url-default-browser)

(setq warning-minimum-level ':error)
;; (mouse-avoidance-mode 'cat-and-mouse)

;; ========
;; doc-view
;; ========
;; (setq doc-view-continuous t)
;; (setq doc-view-resolution 250)
;; (setq doc-view-image-width 1250)
;;; (setq doc-view-cache-directory "/tmp/docview1000")

;; =====
;; ediff
;; =====

;;; list-colors-display sorted by hue
;; (setq list-colors-sort 'hsv)

;; (setq emerge-diff-options "--ignore-all-space")
;; Emerge doesn't care about differences in whitespace


;; ====
;; Tags
;; ====
;; tags-table-list
;; Look at the TAGS files in these directories


;; ===========
;; lock screen
;; ===========
;; (defun mk-lock-screen ()
;;   (interactive)
;;   (async-shell-command "sudo openvt -sw -- vlock -a"))


;; ======
;; abbrev
;; ======
;; (setq abbrev-file-name
;;       "~/elisp/cache/abbrev-defs")

;; (setq save-abbrevs t)
;; Save abbrevs when files are saved you will be asked
;; before the abbreviations are saved

;; (quietly-read-abbrev-file) ;; Reads the abbreviations file on startup


;; =====================
;; Common Lisp emulation
;; =====================
;; Resource:
;; https://www.gnu.org/software/emacs/manual/html_mono/cl.html

;; =========
;; bookmarks
;; =========
;; (setq
;;  bookmark-default-file "~/Dropbox/shared-files/emacs-bookmarks"
;;  bookmark-save-flag 1)			; autosave each change


(provide 'mk_better-defaults)
;;; mk_better-defaults.el ends here
