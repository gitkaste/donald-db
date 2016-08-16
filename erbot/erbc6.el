;;; erbc6.el --- Mix of fsbot functions contributed by freenode users, and of fsi-battle, etc. from DG.
;; Time-stamp: <2007-11-23 11:30:12 deego>
;; Copyright (C) 2003 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbc6.el
;; Package: erbc6
;; Author: D. Goel <deego@gnufans.org> and #emacsers
;; Keywords:
;; Version:
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot
;; For latest version:
;; This file is NOT (yet) part of GNU Emacs.
 
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
 
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 

;;; Real Code:



(defun fs-m8b nil
  (fs-random-choose 
   '("Yes" "No" "Definitely" "Of course not!" "Highly likely." 
     "Ask yourself, do you really want to know?" 
     "I'm telling you, you don't want to know." "mu!" )))



(defun fsi-C-h (sym &rest thing)
  "Syntactic sugar for some fsbot functions.
;;; 2003-08-16 T15:19:00-0400 (Saturday)   deego
Coded by bojohan on #emacs."
  (cond
   ((eq sym 'f)
    (apply 'fs-df thing))
   ((eq sym 'k)
    (apply 'fs-dk thing)) 
    ((eq sym 'c)
     (apply 'fs-describe-key-briefly thing))
    ((eq sym 'w)
     (apply 'fs-dw thing))
    ((eq sym 'v)
     (apply 'fs-dv thing))))


(defun fsi-wtf-is (&optional term &rest args)
  (unless term 
    (error "Syntax: wtf TERM"))
  (require 'wtf)
  (funcall 'wtf-is (format "%s" term)))



(defalias 'fsi-wtf 'fsi-wtf-is)

;; unknown if safe. Disable for now. 
;; (defalias 'fsi-rx 'rx)
;; unsafe? 
;; (defalias 'fsi-rx-form 'fsi-rx-from)
;; unsafe?
;; <forcer> deego: (rx-to-string '(eval (with-current-buffer "*scratch*" (insert "foo")))) works, for example
;; (defalias 'fsi-rx-to-string 'fsi-rx-from)


(defun fsi-distance-fromcharset (charset &rest args)
  (let ((msg (string-to-list (fsi-stringify args)))
	(sum 0))
    (mapcar (lambda (a)
	      (when (not (member a charset)) (incf sum)))
	    msg)
    sum))




(defun fsi-distance-dvorak (&rest args)
  (apply 'fsi-distance-fromcharset 
	 '(?a ?o ?e ?u ?i ?u ?d ?h ?t ?n ?s)
	 args))
(defun fsi-distance-qwerty (&rest args)
  (apply 'fsi-distance-fromcharset
	 '(?a ?s ?d ?f ?g ?h ?j ?k ?l 59 ?')
	 args))

;; these get updated throughout an fsbot session.
(defvar fsi-distance-dvorak 0)
(defvar fsi-distance-qwerty 0)

(defun fsi-distances-calculate (&rest args)
  "Calculate distances from dvorak, qwerty, and any other charsets, and update a running tally. Return a list in order."
  (let ((dv (apply 'fsi-distance-dvorak args))
	(qw (apply 'fsi-distance-qwerty args)))
    (incf fsi-distance-dvorak dv)
    (incf fsi-distance-qwerty qw)
    (list dv qw)))

(defun fsi-battle-dvorak-qwerty (&rest args)
  (let* ((dvqw (apply 'fsi-distances-calculate args))
	 (dv (first dvqw)) (qw (second dvqw))
	 str1 str2)
    (cond 
     ((= dv qw) (setf str1 (format "That's a tie!")))
     ((< dv qw) (setf str1 
		      (concat "Dvorak wins"
			      (fsi-random-choose '(" again!" ".") '(1 5)))))
     ((> dv qw) (setf str1 
		      (concat "Qwerty wins"
			      (fsi-random-choose '(". Really?" ".") '(1 5)))))
     )
    (cond 
     ((null args) (error "Syntax ,war \"<some string>\""))
     (t (concat
	 str1 " "
	 (format "Distance..DVORAK:QWERTY::%d:%d OVERALL.. %d:%d"
		 dv qw fsi-distance-dvorak
		 fsi-distance-qwerty))))))

(defun fsi-battle-reset ()
  (setf fsi-distance-dvorak 0)
  (setf fsi-distance-qwerty 0))

(defalias 'fsi-battle 'fsi-battle-dvorak-qwerty)
;; (defalias 'fsi-fight 'fsi-battle-dvorak-qwerty)
(defalias 'fsi-war 'fsi-battle-dvorak-qwerty)


(defalias 'fsi-whine 'whine)  

(defun fsi-emacs-uptime (&rest args)
  (emacs-uptime))

(erbutils-defalias-i '(make-list))

(erbutils-defalias-i '(remove-duplicates delete-duplicates))

;; Does work for things like mapconcat.
(defalias 'fsi-lambda 'lambda)


(provide 'erbc6)
(run-hooks 'erbc6-after-load-hook)



;;; erbc6.el ends here
