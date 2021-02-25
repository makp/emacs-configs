;;; mk_mode-line.el --- Config for the mode line

;;; Commentary:

;; Config examples:
;; https://emacs.stackexchange.com/questions/13652/how-to-customize-mode-line-format

;; 

;;; Code:

(display-time) ;; display time and load level every minute

(defun mk/state-appearance-on-modeline ()
  "Select face for the different Evil states."
  (if (equal 'normal evil-state)
      'font-lock-constant-face
    'font-lock-builtin-face))


(setq-default mode-line-format
	      (list
	       "%e"	    ;print error mess on full memory
	       mode-line-front-space

	       '(:eval (propertize "%b " 'face 'font-lock-keyword-face
				   'help-echo (buffer-file-name))) ;tool tip

	       '(vc-mode vc-mode)

	       ;; note that the eval is necessary to update state
	       '(:eval (propertize evil-mode-line-tag 'face
				   (mk/state-appearance-on-modeline)))

	       "(" ;; '%02' to set to 2 chars at least; prevents flickering
	       (propertize "%02l" 'face 'font-lock-type-face) "," ; line number
	       (propertize "%02c" 'face 'font-lock-type-face)	  ; column number
	       ") "

	       "%I" ;; file size
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

	       "%-" ;; fill with '-'
	       ))

(provide 'mk_mode-line)

;;; mk_mode-line.el ends here