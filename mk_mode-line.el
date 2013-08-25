(setq-default mode-line-format
	      (list
	       "%@"
	       ;; the buffer name; the file name as a tool tip
	       '(:eval (propertize "%b " 'face 'font-lock-keyword-face
				   'help-echo (buffer-file-name)))

	       ;;'(vc-mode vc-mode)

	       ;; line and column
	       "(" ;; '%02' to set to 2 chars at least; prevents flickering
	       (propertize "%02l" 'face 'font-lock-type-face) ","
	       (propertize "%02c" 'face 'font-lock-type-face) 
	       ") "

	       ;; relative position, size of file
	       "["
	       (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
	       "/"
	       (propertize "%I" 'face 'font-lock-constant-face) ;; size
	       "] "

	       ;; the current major mode for the buffer.
	       "["

	       '(:eval (propertize "%m" 'face 'font-lock-string-face
				   'help-echo buffer-file-coding-system))
	       ;; was this buffer modified since the last save?
	       '(:eval (when (buffer-modified-p)
			 (concat ","  (propertize "Mod"
						  'face 'font-lock-warning-face
						  'help-echo "Buffer has been modified"))))

	       ;; is this buffer read-only?
	       '(:eval (when buffer-read-only
			 (concat ","  (propertize "RO"
						  'face 'font-lock-type-face
						  'help-echo "Buffer is read-only"))))  

	       "]"

	       (propertize " || " 'face 'font-lock-constant-face)

	       ;;global-mode-string, org-timer-set-timer in org-mode need this
	       ;;       '(:eval (propertize "%M" 'face 'font-lock-warning-face))

	       '(global-mode-string global-mode-string)

	       " --"
	       ;; i don't want to see minor-modes; but if you want, uncomment this:
	       ;; minor-mode-alist  ;; list of minor modes
	       "%-" ;; fill with '-'
	       ))

(provide 'mk_mode-line)
;;; mk_mode-line.el ends here