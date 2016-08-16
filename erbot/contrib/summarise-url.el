(require 'url)

(defun summarise-emacswiki (url)
  "Fetch an emacswiki URL and summarise it, returning the summary as a string"
  (set-buffer (url-retrieve-synchronously url))
  (summarise-emacswiki-callback nil))

(defun summarise-emacswiki-callback (status &rest args)
  "Summarisation handled here: This can be used as an async url.el style 
callback (see `url-retrieve' for details)"
  (let ((xhtml nil) (case-fold-search t) (enc nil))
    (mail-narrow-to-head)
    (setq enc (or (when (search-forward-regexp 
                         "^content-type:.*?;\\s-*charset=\\(\\S-+\\)" nil t)
                    (intern-soft (downcase (match-string 1)))) 'utf-8))
    (goto-char (point-max))
    (widen)
    (set-buffer-multibyte t)
    (forward-char)
    (decode-coding-region (point) (point-max) enc)
    (setq xhtml (xml-parse-region (point-min) (point-max)))
    (kill-buffer (current-buffer))
    (summarise-string
     (summarise-xml-to-string 
      (summarise-emacswiki-content xhtml))) ))

(defun summarise-string (text)
  "Given a string, return a summary consisting of the first sentence or so
followed by the results of running ots on the string."
  (with-temp-buffer 
    (set-buffer-multibyte t)
    (insert text) 
    (goto-char (point-min))
    (let ((preamble "") 
          (coding-system-for-read  'utf-8)
          (coding-system-for-write 'utf-8)) 
      (when (re-search-forward "\\S-[^.\n]+" nil t)
        (setq preamble (match-string 0))
        (when (< 160 (length preamble)) 
          (setq preamble (substring preamble 0 160))))
      (shell-command-on-region (point-min) (point-max) 
                               "ots" nil t (current-buffer))
      (beginning-of-buffer)
      (if (re-search-forward (regexp-quote preamble) nil t) 
	  (buffer-string)
	(concat preamble " " (buffer-string))) )))

(defun summarise-xml-to-string (xml-node) 
  "Strip all tags from this xml.el style node structure and its descendents,
in other words, return the bare text contained within this node."
  (if (stringp xml-node) xml-node
    (mapconcat 
     'identity 
     (cons " " (mapcar 'summarise-xml-to-string 
                       (xml-node-children xml-node))) "") ))

(defun summarise-emacswiki-content (xhtml)
  (let ((body    (cdr (assq 'body (assq 'html xhtml)))) 
        (content nil))
    (mapc
     (lambda (div) 
       (when (equal (xml-get-attribute div 'class) "wrapper") 
         (setq content div)
         ;;(mapc 
         ;; (lambda (div) 
         ;;   (when (string-match "\\<content\\>" (xml-get-attribute div 'class)) 
         ;;     (setq content div)))
         ;; (xml-get-children div 'div)) 
         )) 
     (xml-get-children body 'div)) 
    content))

(provide 'summarise-url)
