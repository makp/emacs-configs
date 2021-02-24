;;; EMMS

;; ===========
;; basic setup
;; ===========
(require 'emms-setup)
(emms-devel)
(emms-default-players)
;;; NOTE: emms-devel "adds all of the features which come with the
;;; Emms distribution regardless of if they are considered stable or
;;; not." If EMMS becomes unstable, use (emms-all) instead.

(setq emms-source-file-default-directory "~/Music/")

;; =========================
;; additional customizations
;; =========================
(emms-mode-line nil)

(setq emms-stream-repeat-p t
      emms-repeat-playlist t)
;;; Repeat playlists and streams

;; (require 'emms-mark)
;; (setq emms-playlist-default-major-mode 'emms-mark-mode)
;;; playlists have a similar behaviour to dired
;; FIXME: As of Aug 24th 2013, this was breaking my setup

;; (require 'emms-browser)
;;; interactive mode to browse the metadata cache

;; ===========
;; keybindings
;; ===========
(global-set-key (kbd "C-c u") nil)
(global-set-key (kbd "C-c u s") 'emms-show)

;;; playlists
(global-set-key (kbd "C-c u g") 'emms-playlist-mode-go)
(global-set-key (kbd "C-c u p") 'emms-play-playlist)

;;; finding  files
(global-set-key (kbd "C-c u l") 'helm-emms)
(global-set-key (kbd "C-c u d") 'emms-play-directory-tree)

;; playing
(global-set-key (kbd "C-c u SPC") 'emms-pause)
(global-set-key (kbd "C-c u k") 'emms-stop)
(global-set-key (kbd "C-c u n") 'emms-next)
(global-set-key (kbd "C-c u N") 'emms-seek-forward)
(global-set-key (kbd "C-c u t") 'emms-previous)
(global-set-key (kbd "C-c u T") 'emms-seek-backward)

;; volume
(global-set-key (kbd "C-c u +") 'emms-volume-mode-plus)
(global-set-key (kbd "C-c u -") 'emms-volume-mode-minus)

;;; browser
;; (global-set-key (kbd "C-c u b") 'emms-smart-browse)

(provide 'mk_emms-setup)
