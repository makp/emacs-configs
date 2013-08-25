;;; configuration to use msmtp
(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/bin/msmtp"
      user-full-name "Makmiller Pedroso")

(setq mail-envelope-from 'header
      message-sendmail-envelope-from 'header
      mail-specify-envelope-from t)

(provide 'mk_msmtp)