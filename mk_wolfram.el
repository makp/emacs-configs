;;; mk_wolfram.el --- Custom config for Mathematica -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:
(autoload 'wolfram-mode "wolfram-mode" nil t)
(autoload 'run-wolfram "wolfram-mode" nil t)
(setq-default wolfram-program "WolframKernel")
(setq-default wolfram-path "~/.Mathematica/Applications")

(provide 'mk_wolfram)

;;; mk_wolfram.el ends here