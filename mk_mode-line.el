;;; mk_mode-line.el --- Config for the mode line

;;; Commentary:

;; 

;;; Code:

(display-time)

(setq-default mode-line-format
	      (list
	       "%e"	    ;print error mess on full memory
	       mode-line-front-space

	       '(:eval (propertize "%b " 'face 'font-lock-keyword-face
				   'help-echo (buffer-file-name))) ;tool tip

	       '(vc-mode vc-mode)

	       '(:eval evil-mode-line-tag) ; without the eval the state doesn't get updated

	       "(" ;; '%02' to set to 2 chars at least; prevents flickering
	       (propertize "%02l" 'face 'font-lock-type-face) "," ; line number
	       (propertize "%02c" 'face 'font-lock-type-face)	  ; column number
	       ") "

	       ;; (propertize "%p" 'face 'font-lock-constant-face) ; relative position
	       (propertize "%I" 'face 'font-lock-constant-face) ;; file size
	       " "

	       ;; Major mode
	       "["

	       '(:eval (propertize "%m" 'face 'font-lock-string-face))

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

	       mode-line-misc-info ; displays info from `global-mode-string'

	       ;; minor-mode-alist  ;; list of minor modes

	       "%-" ;; fill with '-'
	       ))

(provide 'mk_mode-line)

;;; mk_mode-line.el ends here