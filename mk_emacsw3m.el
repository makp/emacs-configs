;; =========
;; emacs-w3m
;; =========
(add-hook 'w3m-mode-hook
	  (lambda ()
	    (scroll-lock-mode 1)))

(setq w3m-quick-start nil)
;;; input a target URL every time you start emacs-w3m

(setq w3m-session-deleted-save nil)

(setq w3m-default-display-inline-images t)

;;; making emacs-w3m behavior more like conkeror.
(add-hook 'w3m-mode-hook
	  (lambda ()
	    (define-key w3m-mode-map (kbd "M-k") nil)
	    (define-key w3m-mode-map (kbd "M-s") nil)
	    (define-key w3m-mode-map (kbd "C-c s") 'w3m-session-select)
	    (define-key w3m-mode-map (kbd "M-n") nil)
	    (define-key w3m-mode-map (kbd "N") 'w3m-scroll-up-or-next-url)
	    (define-key w3m-mode-map (kbd "F") 'w3m-view-next-page)
	    (define-key w3m-mode-map (kbd "P") 'w3m-view-previous-page)
	    (define-key w3m-mode-map (kbd "b") 'w3m-bookmark-add-current-url)))  ;it was nothing before 

(setq browse-url-browser-function 'choose-browser)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/usr/bin/conkeror")

(defun choose-browser (url &rest args)
  (interactive "sURL: ")
  (if (y-or-n-p "Use external browser? ")
      (browse-url-generic url)
    (w3m-browse-url url)))

(setq browse-url-browser-function 'choose-browser)

(global-set-key "\C-cw" 'browse-url-at-point)
;; `w3m-goto-url' can do that among other things.

;; Enable cookies
(setq w3m-use-cookies t)

;; Consider all urls safe
(setq mm-w3m-safe-url-regexp nil)

;; --------------
;; Numbered links
;; --------------
(add-hook 'w3m-mode-hook 
	  (lambda ()
	    (w3m-lnum-mode 1)))

;; ---------------------------------------
;; Download files asynchronously with wget
;; ---------------------------------------
;;;###autoload
(defun w3m-download-with-wget (loc)
  (interactive "DSave to: ")
  (let ((url (or (w3m-anchor) (w3m-image))))
    (if url
	(let ((proc (start-process "wget" (format "*wget %s*" url)
				   "wget" "--passive-ftp" "-nv"
				   "-P" (expand-file-name loc) url)))
	  (with-current-buffer (process-buffer proc)
	    (erase-buffer))
	  (set-process-sentinel proc (lambda (proc str)
				       (message "wget download done"))))
      (message "Nothing to get"))))

;; -------------------
;; view youtube videos
;; -------------------
;;;###autoload
(defun w3m-yt-view ()
  "View a YouTube link with youtube-dl and mplayer."
  (interactive)
  (let ((url (or (w3m-anchor) (w3m-image))))
    (string-match "[^v]*v.\\([^&]*\\)" url)
    (let* ((vid (match-string 1 url))
           (out (format "%s/%s.mp4" w3m-default-save-directory vid)))
      (call-process "youtube-dl" nil nil nil "-U" "-q" "-c" "-o" out url)
      (start-process "mplayer" nil "mplayer" "-quiet" out))))


;; --------------
;; Search related
;; --------------
(require 'w3m-search)

;; (defun ache-w3m (&optional n)
;;   "Calls w3m-search. With a numeric argument, it opens
;; w3m-search in another window"
;;   (interactive "P")
;;   (if (null n)
;;       (call-interactively 'w3m-search)
;;     (save-excursion
;;       (switch-to-buffer-other-window (current-buffer))
;;       (call-interactively 'w3m-search))))

;; Make previous search engine default
(defadvice w3m-search (after change-default activate)
  (let ((engine (nth 1 minibuffer-history)))
     (when (assoc engine w3m-search-engine-alist)
      (setq w3m-search-default-engine engine))))

(defun chamando-w3m ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'w3m-search)))

(global-set-key (kbd "C-x C-'") 'chamando-w3m)

(eval-after-load "w3m-search"
  '(progn
     (add-to-list 'w3m-search-engine-alist
		  '("sep"
		    "http://plato.stanford.edu/search/searcher.py?query=%s"
		    nil))
     (add-to-list 'w3m-uri-replace-alist
		  '("\\`sep:" w3m-search-uri-replace "sep"))))

(eval-after-load "w3m-search"
  '(progn
     (add-to-list 'w3m-search-engine-alist
		  '("free-dic"
		    "http://www.thefreedictionary.com/%s"
		    nil))
     (add-to-list 'w3m-uri-replace-alist
		  '("\\`d:" w3m-search-uri-replace "free-dic"))))

(add-to-list 'w3m-search-engine-alist
	     '("youtube" "http://www.youtube.com/results?search_query=%s&search=Search" nil))

(add-to-list 'w3m-search-engine-alist
	     '("emacs-wiki" "http://www.emacswiki.org/cgi-bin/wiki.pl?search=%s" nil))

;; ----------------------
;; toggle to a w3m buffer
;; ----------------------
(defun wicked/toggle-w3m ()
  "Switch to a w3m buffer or return to the previous buffer."
  (interactive)
  (if (derived-mode-p 'w3m-mode)
      ;; Currently in a w3m buffer
      ;; Bury buffers until you reach a non-w3m one
      (while (derived-mode-p 'w3m-mode)
	(bury-buffer))
    ;; Not in w3m
    ;; Find the first w3m buffer
    (let ((list (buffer-list)))
      (while list
	(if (with-current-buffer (car list)
	      (derived-mode-p 'w3m-mode))
	    (progn
	      (switch-to-buffer (car list))
	      (setq list nil))
	  (setq list (cdr list))))
      (unless (derived-mode-p 'w3m-mode)
	(call-interactively 'w3m)))))

(global-set-key (kbd "C-x '") 'wicked/toggle-w3m)

(provide 'mk_emacsw3m)
