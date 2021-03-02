;;; mk_eww-search-engine.el --- Custom config for EWW -*- lexical-binding: t -*-

;;; Commentary:

;; Add costom search engines to EWW.
;; This code is adapted from https://gist.github.com/brenns10/69d39f6c46170093f73d

;;; Code:

(require 'cl-lib)

(defvar search-engines
  '((("google" "g") "https://google.com/search?q=%s")
    (("duckduckgo" "d" "ddg") "https://duckduckgo.com/lite/?q=%s") ;"https://duckduckgo.com/?q=%s"
    (("sep" "s") "http://plato.stanford.edu/search/searcher.py?query=%s")
    (("free-dic" "f") "http://www.thefreedictionary.com/%s")))

(defvar search-engine-default "duckduckgo")

(defun search-get-engine (engine-name engine-list)
  (cond
   ((null engine-list) nil)
   ((member engine-name (caar engine-list)) (cadar engine-list))
   (t (search-get-engine engine-name (cdr engine-list)))))

(defun search-engine (engine-name term)
  "Search for a term using an engine."
  (interactive "MEngine: \nMTerm: ")
  (let* ((url (search-get-engine engine-name search-engines)))
    (if (equal url nil)
        (message "Error: search engine \"%s\" unknown." engine-name)
      (eww (format url (url-hexify-string term))))))

(defun mk/search-web (term)
  "Search the web using `search-engine-default' or a specified engine from `search-engine'."
  (interactive "MQuery: ")
  (let ((idx (cl-position ?: term)))
    (if (equal idx nil)
        (search-engine search-engine-default term)
      (search-engine (cl-subseq term 0 idx)
                     (cl-subseq term (+ 1 idx))))))

(provide 'mk_eww-search-engine)
;;; mk_eww-search-engine.el ends here