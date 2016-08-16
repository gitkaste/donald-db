;;; erbcspecial.el --- Special/dangerous implementation functions.
;; Many fs-functions can simply be defined in terms of other
;; fs-functions (and always should be!, for security.)
;; This file is for the remaining few, that can't be.
;; Thus, CODE IN THIS FILE SHOULD BE CONSTRUCTED VERY CAREFULLY.
;; Time-stamp: <2007-11-23 11:30:12 deego>
;; Copyright (C) 2004 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbcspecial.el
;; Package: erbcspecial
;; Author: D. Goel <deego@glue.umd.edu>
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
 

;; See also:

(defconst erbcspecial-version "0.0dev")

;;==========================================
;;; Requires:
(eval-when-compile (require 'cl))

;;; Real Code:

(defun erbn-special-quote-function-old-broken (fcn)
  ;; A little broken. Does not handle variables correctly.. 
  (error "Being fixed atm.")
  (when erbot-paranoid-p
    (error "this function  is disabled atm: paranoid-p is true."))
  (cond
   ((symbolp fcn)
    (erblisp-sandbox-quoted fcn))
   ;; If it's a lamba, it needs to be sandboxed carefully! It could have been a quoted lambda.. which then means verbatim evaluations!
   ;; Otherwise, the user can supply us '(lamda (x) (shell-command... )) and we will happily do it!
   ((and (listp fcn)
	 (equal (first fcn) 'lambda)
	 (erblisp-sandbox fcn)))
   ;; notice the recursion below:
   ((listp fcn) (erbn-special-quote-function-old-broken (fs-eval fcn)))
   (t (error "Cannot apply this as a function!"))))


;; (defun fs-mapcar-old (sym seq)
;;   "only symbols allowed at this time. "
;;   (unless (symbolp sym)
;;     (error "Function argument to mapcar for this bot can only be a symbol."))
;;   (setq sym (erblisp-sandbox-quoted sym))
;;   ;; everything should already be boxquoted.. cool
;;   (mapcar sym seq))

(defun OLDfsi-mapcar (fcn ls)
  (apply 'mapcar 
	 (erbn-special-quote-function-old-broken fcn)
	 ls nil))







(defmacro fsi-apply (sym &rest args)
  (when erbot-paranoid-p 
    (error "This function is disabled atm: erbot-paranoid-p"))
  (unless sym (error "No function to fs-apply!"))
  (let (erbn-tmpargs
	(erbn-tmplen (length args))
	erbn-tmpfirstargs
	erbn-lastargs
	erbn-tmpspecialp ;; denotes: NIL: no arguments at all.
	erbn-tmpnoinitialp ;; denotes the case when the len args =1..
	) 
    (cond
     ((= (length args) 0)
      (setq erbn-tmpspecialp t))
     ((= (length args) 1)
      (setq erbn-tmpnoinitialp t)))
    (cond
     ((null args)
      (setq erbn-tmpargs nil)
      (setq erbn-tmplastargs nil)
      (setq erbn-tmpspecialp nil))
     (t
      (setq erbn-tmpargs
	    (append (subseq args 0 (- erbn-tmplen 1))))
      (setq erbn-tmplastargs
	    (first (last args)))))
    (setq erbn-tmpargs (erbn-apply-sandbox-args erbn-tmpargs))
    (setq erbn-tmplastargs 
	  (if (and (listp erbn-tmplastargs)
	       (equal (car erbn-tmplastargs) 'quote))
	      erbn-tmplastargs
	    (erbn-apply-sandbox-args erbn-tmplastargs)))
    ;; if quoted lambda, just make it a lambda, please.
    (setf sym (fsi-standardize-function sym))

    (cond
     (erbn-tmpspecialp
      `(apply ,sym nil))
     (erbn-tmpnoinitialp
      `(apply ,sym ,erbn-tmplastargs))
     (t
      `(apply ,sym ,@erbn-tmpargs ,erbn-tmplastargs)))))




(defmacro erbn-mapcar-etc (mapfunction sym ls)
  "Utility macro meant to be called by fsi-mapcar, fsi-mapc, etc.


Caution: This macro should NOT be called except from fsi- or fs- functions. 
The reason is that it converts all variables, etc. supplied to it to fs-vars.
The calling function may have done something like (let ((v 'foo)) (erbn-mapcar-etc ... v ... )).
erbn-mapcar-etc will then try to eval fs-v. If, for some reason, the calling function has fs-v bound to unsavory stuff, we could end up evaling that.

Such a problem will never happen from user functions, of course,
because by the time they reach us, even the let binding should
have been applied to fs-v. You may suspect that another reason
userfunctions are safe is that userfunctions can never directly
call erbn-mapcar-etc in the first place. But, that reasoning doesn't
really hold because they can still call the macro fsi-mapcar,
which in turn calls us."


  (assert (atom mapfunction))
  ;; NO MAPCONCAT HERE, please. That needs to be created separately.
  (assert (member mapfunction '(mapcar mapl mapcon maplist mapc)))
  (when erbot-paranoid-p 
    (error "This function is disabled atm: erbot-paranoid-p"))
  (unless sym (error "No function to fs-apply!"))
  (let (
	;;(erbn-tmplen (length ls))
	(erbn-tmplastls ls)
	) 

    (setq erbn-tmplastls 
	  (if (and (listp erbn-tmplastls)
	       (equal (car erbn-tmplastls) 'quote))
	      erbn-tmplastls
	    (erbn-apply-sandbox-args erbn-tmplastls)))
    ;; if quoted lambda, just make it a lambda, please.

    (setf sym (fsi-standardize-function sym))

    
    `(,mapfunction ,sym ,erbn-tmplastls)))






(defun fsi-mapcar* (&rest args)
  (error "mapcar*? I don't do no mapcar*! Try apt-get install emacs and (require 'cl)"))

(defmacro fsi-mapc (sym args &rest ig)
  `(erbn-mapcar-etc mapc ,sym ,args))

(defmacro fsi-mapcar (sym args &rest ig)
  `(erbn-mapcar-etc mapcar ,sym ,args))

(defmacro fsi-mapcon (sym args &rest ig)
  `(erbn-mapcar-etc mapcon ,sym ,args))


(defmacro fsi-mapl (sym args &rest ig)
  `(erbn-mapcar-etc mapl ,sym ,args))
(defmacro fsi-maplist (sym args &rest ig)
  `(erbn-mapcar-etc maplist ,sym ,args))



(defun fsi-mapconcat (fs-f fs-ls &optional sep)
  ;; For reasons unknown, this does NOT work if I use f instead of fs-f above. 
  (when (null sep) (setf sep " "))
  (let 
      ((ls2 (fsi-mapcar fs-f fs-ls sep)))
    (mapconcat 'identity ls2 sep)))





(provide 'erbcspecial)
(run-hooks 'erbcspecial-after-load-hook)



;;; erbcspecial.el ends here
