;;; erbsudoku.el --- 
;; Time-stamp: <2014-06-11 23:11:02 fledermaus>
;; Copyright © 2014 Vivek Das Mohapatra
;; Emacs Lisp Archive entry
;; Filename: erbsudoku.el
;; Package: erbsudoku
;; Author: Vivek Das Mohapatra <vivek@etla.org>
;; Keywords:
;; Version:
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot

(require 'sudoku)

(defun fs-sudoku-solve (&rest args)
  (let ((raw fs-internal-message-sans-bot-name) grid)
    (setq raw (replace-regexp-in-string "^\\S-+\\s-+" "" raw))
    (when (setq grid (catch :sd-invalid (sd-parse-string raw)))
      (let ((sd-search-limit 1000))
	(setq grid (sd-search grid)))
      (format "%s\n%d search ops; %d squares inspected; %0.2f seconds"
	      (sd-format grid t)
	      sd-search-opcount
	      (length sd-search-squares)
	      (sd-elapsed)))))

(provide 'erbsudoku)
