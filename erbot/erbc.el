;;; erbc.el --- Erbot user-interface commands -- see also erbc5.el
;; Time-stamp: <2006-09-28 12:17:42 deego>
;; Copyright (C) 2002 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbc.el
;; Package: erbc
;; Author: D. Goel <deego@gnufans.org>
;; Version: 0.0DEV
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot
;; Other files:
;;; erball.el --- Functions on all files.
;;; erbbdb.el ---
;;; erbc.el --- Erbot user-interface commands.
;;; erbc2.el --- mostly: special functions for erbc.el
;;; erbc3.el ---erbot lisp stuff which should be PERSISTENT ACROSS SESSIONS.
;;; erbc4.el --- Russian Roulette
;;; erbc5.el --- continuation of erbc.el
;;; erbc6.el --- fsbot functions contributed by freenode users,
;;; esp. #emacsers.
;;; erbcompat.el --- Erbot GNU Emacs/XEmacs compatibility issues
;;; erbcountry.el
;;; erbc-special.el --- Special/dangerous implementation functions.
;;; erbdata.el ---
;;; erbedit.el --- quicker operator editing of bots' bbdb
;;; erbeng.el ---  english
;;; erbforget.el --- Help make the bots forget some TERMS.
;;; erbkarma.el ---
;;; erblisp.el ---
;;; erblog.el ---
;;; erbmsg.el --- memoserv-esque functions for Erbot
;;; erbot.el --- Another robot for ERC.
;;; erbp.el --- NOT FUNCTIONAL personal erbot-interface, stolen from dunnet.el
;;; erbtrain.el --- Train erbot (erbot)..
;;; erbutils.el ---  utils
;;; erbwiki.el ---

(defvar fs-home-page
  "http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot")



;; This file is NOT (yet) part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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




(defvar erbc-version "0.0dev")
(defvar fs-version "0.0dev")


;;==========================================
;;; Code:

;; NOTE:  stuff like (fs-et) can be passed possibly mischievous
;; code as the first argument... never eval or "set" any
;; "code"... always convert it to a single atom... before setting it..



(require 'find-func)

(defgroup erbc nil
  "The group erbc"
   :group 'applications)


(defcustom fs-before-load-hooks nil "" :group 'erbc)
(defcustom fs-after-load-hooks nil "" :group 'erbc)



(defcustom erbn-char ","
  "The character which calls the bot.

in addition to directly addressing it.

may be different for
different bots.

Is really a string, but the length of the string should be 1,.
")
(defcustom erbn-char-double (concat erbn-char erbn-char)
  "The string which calls the bot from midsentence

this string should have a length of EXACTLY 2.

")


(run-hooks 'fs-before-load-hooks)


;; Real code
(defcustom fsi-internal-botito-mode nil
  "Mode to turn on more english-like bunny-behavior"
  :group 'erbc)



(defvar fs-tgt nil "Tgt visible to the end-user, as well as changeable by them.")
(defvar erbn-tgt nil "Tgt NOT changeable by enduser.")

(defvar fs-nick "")
(defvar erbn-nick "")

(defvar erbn-buffer "")

(defcustom fs-internal-parse-error-p
  nil
  "Whether to show lispy errors in term descriptions.

When nil, an error in a  lispy description of a term makes to bot
go to an english mode for the term.
When non-nil, we will just display the error.  On a channel full of
lisp hackers, we will want to make this t for users' convenience.")


(defcustom erbn-shell-command-p nil
  "Whether to allow commands that use shell-commands...
Some fsbot commands use shell-commands... shell-commands always mean
possibility of exploits.  andn are disabled by default.

Make this t at your own risk. ")


(defcustom fs-internal-questions
  '("what" "where" "who" "wtf" "wth" "wht"
    ;; no please:
    ;;"why"
    ;;"how"
    )
  ""
  :group 'erbc)

(defcustom erbn-google-defaults
  '(("#emacs" ("emacs"))
    ("#fsbot" ("fsbot")))
  "" :group 'erbc)



(defun erbn-type-check (expr)
  "We only deal with these types. Used in things such as erblisp-unsandbox, etc. "
  (assert (member (type-of expr)
		  '(string cons symbol integer float))))

(defun erbn-shell-command (&optional command overridep)
  "Execute shell-commands when erbn-shell-command-p is true.

However, if the second argument overridep is non-nil, we use that to
determine whether to execute the command.  In that case, we execute
the command only if overridep is a list, whose first entry of that
list is non-nil"
  (cond
   ((or (and overridep
	     (listp overridep)
	     (first overridep))
	erbn-shell-command-p)
    (apply 'shell-command command nil))
   (t
    (error "The bot-operator has shell commands disabled"))))





(defun erbn-shell-command-to-string (&optional command overridep)
  "Execute shell-commands when erbn-shell-command-p is true.

However, if the second argument overridep is non-nil, we use that to
determine whether to execute the command.  In that case, we execute
the command only if overridep is a list, whose first entry of that
list is non-nil"
  (cond
   ((or (and overridep
	     (listp overridep)
	     (first overridep))
	erbn-shell-command-p)
    (apply 'shell-command-to-string command nil))
   (t
    (error "The bot-operator has shell commands disabled"))))





(defun fsi-get-google-defaults ()
  (cadr (assoc fs-tgt erbn-google-defaults)))

(defvar fsi-prestring  "")
;; (make-variable-buffer-local 'fsi-prestring)


(defcustom fsi-internal-google-level 75
  "75 is a good choice for fsbot. "
  :group 'erbc)

(defcustom fs-internal-english-max-matches 20
  "This check is triggerred only when the users' original request didnot
succeed and so we have gone into an english-mode and are searching.
If the number of matches results in 1000, then most likely, the word
was something like i or you and the user was not intending a search.
"

:group 'erbc)

(defcustom fs-internal-questions-all
  '("what" "where" "who" "why" "how"
    "whose" "which"
    )
  ""
  :group 'erbc)

(defcustom fs-internal-articles
  '("the" "a" "an" "this" "that")
  ""
  :group 'erbc)


(defcustom fs-internal-english-target-regexp
  "^$"
  "Targets that prefer english.. so erbot will usually go to a
english-mode unless near-exact matches.  This shall usually happen on
the few social channels erbot hangs out on. "
  :group 'erbc)

(defcustom fs-internal-query-target-regexp
  "^$"
  "Targets where erbot will respond to queries like:
Foo ? "
  :group 'erbc)

(defcustom fs-internal-add-nick-weights
  '(2;; yes
    3 ;;no
    )
  ""
  :group 'erbc)


(defun fsi-correct-entry (name &rest fubar)
  "Assumes that name is a string... this downcases strings.  Rendering
it fit for database-entry. "
  (unless (stringp name) (setq name (format "%s" name)))
  ;;(downcase
  (let ((newname
	 (mapconcat 'identity (split-string name) "-")))
    (or (erbbdb-get-exact-name newname)
	newname)))


(defun fsi-describe-key-briefly (&optional key &rest args)
  "Return the function on key..building block for other erbc's..
If no such function, return the symbol 'unbound. "

  (unless key
    (error
     "Syntax:  , dkb key"))
  (when (and (null key) (null args))
    (setq key ""))
  (unless (stringp key)
    ;; is this safe? what about properties?
    (setq key (read-kbd-macro
	       (mapconcat '(lambda (arg) (format "%s" arg))
			  (cons key args)
			  " "))))
  (let ((res (key-binding key)))
    (if res res
      'unbound)))

;; for now..
;;(defalias 'fs-describe-key 'fs-describe-key-briefly)

(defun fsi-where-is-in-map (map &optional fcn)
  (let* ((wi (where-is-internal fcn map)))
    (mapconcat 'key-description wi ", ")))

(defun fsi-where-is-gnus-group (&optional fcn)
  (require 'gnus)
  (unless fcn (error "please supply a function"))
  (fs-where-is-in-map gnus-group-mode-map fcn))

(defun fsi-where-is-gnus-summary (&optional fcn)
  (require 'gnus)
  (unless fcn (error "please supply a function"))
  (fs-where-is-in-map gnus-summary-mode-map fcn))
(defun fsi-where-is-message (&optional fcn)
  (require 'gnus)
  (require 'message)

  (unless fcn (error "please supply a function"))
  (fs-where-is-in-map message-mode-map fcn))



(defun fsi-keyize (key morekeys)
  (setq key (read-kbd-macro
	     (mapconcat '(lambda (arg) (format "%s" arg))
			(cons key morekeys) " "))))


(defun fsi-describe-key-one-line (&optional key &rest args)
  "Key, and just one line of function"
  (unless key (error "Syntax: , dk \"Key...\""))
  (let* ((fcn (apply 'fs-describe-key-briefly key args))
	 (fcns (format "%s" fcn))
	 (apr (or (fs-apropos-exact fcns)
		  "No doc. available. ")))
    (concat (format "%s -- %s"
		    fcns
		    apr))))

(defalias 'fsi-dko 'fs-describe-key-one-line)

(defalias 'fsi-describe-key 'fs-describe-key-and-function)

(defun fsi-lookup-key-from-map-internal (&optional map key &rest morekeys)
  (unless key (error "No key supplied. "))
  (unless (stringp key)
    (setq key (read-kbd-macro
	       (mapconcat '(lambda (arg) (format "%s" arg))
			  (cons key morekeys) " "))))
  (unless (arrayp key) (setq key (format "%s" key)))
  (let* ((fcn (lookup-key map key))
	 (fcns (format "%s" fcn))
	 (apr (or (fs-apropos-exact fcns)
		  "No doc available. ")))
    (concat (format "%s -- %s" fcns apr))))

(defun fsi-lookup-key-gnus-group (&optional key &rest args)
  (unless key (error "Syntax: , lkgg \"Key...\""))
  (require 'gnus-group)
  (apply 'fs-lookup-key-from-map-internal gnus-group-mode-map key args))

(defun fsi-lookup-key-gnus-summary (&optional key &rest args)
  (unless key (error "Syntax: , lkgg \"Key...\""))
  (require 'gnus)
  (apply 'fs-lookup-key-from-map-internal gnus-summary-mode-map key args))

(defun fsi-lookup-key-message (&optional key &rest args)
  (unless key (error "Syntax: , lkgg \"Key...\""))
  (require 'gnus)
  (require 'message)
  (apply
   'fs-lookup-key-from-map-internal gnus-message-mode-map key args))



(defun fsi-apropos-exact (str)
  (unless (stringp str) (setq str (format "%s" str)))
  (let* ((reg (concat "^" (regexp-quote str) "$"))
	 (apr (apropos reg))
	 (asso (assoc* str apr
		       :test
		       (lambda (a b)
			 (string= (format "%s" a) (format "%s" b)))))

	 (val (second asso)))
    (if val (format "%s" val)
      nil)))

(defun fsi-describe-key-long (k &rest args)
  (let ((f (apply 'fs-describe-key-briefly k args)))
    (fs-describe-function-long f)))

(defun fsi-describe-key-and-function (key &rest args)
  "Describe the key KEY.
Optional argument ARGS .  If the input arguments are not strings, it
kbds's them first... , so that , df C-x C-c works"
  (when (and (null key) (null args))
    (setq key ""))
  (unless (stringp key)
    (setq key (read-kbd-macro
	       (mapconcat '(lambda (arg) (format "%s" arg))
			  (cons key args)
			  " "))))
  (let ((b (key-binding key)))
    (cond
     ((symbolp b)
      (or
       (ignore-errors (fs-describe-function b))
       (format "Bound to: %s" b)))
     (t
      (format "Bound to: %s" b)))))



(defun fsi-describe-function (&optional function nolimitp &rest fubar)
  "Describes the FUNCTION named function.
Also tries an fs- prefix for the function..
nolimitp has to be eq 'nolimit for the nolimit effect to take place..
"
  (unless function
    (error
     "Syntax: (describe-function 'name-of-function) or , df 'name"))
  (let* ((f function)
	 g
	 (varp nil)
	 )
    (when (stringp f)
      (setq f (erbn-read f)))
    (assert (symbolp f))
    (setq g
	  (erbn-read (concat "fs-" (format "%s" f))))
    ;; set g and varp correctly.
    (cond
     ((fboundp f) (setq g f))
     ((fboundp g) t)
     ((boundp f) (setq varp t g f))
     ((boundp g) (setq varp t))
     (t (error "No function or variable found matching: %s, %s." f g)))
    (cond
     (varp (concat 
	    (format "No matching function found. Variable description for %s: \n"
		    g)
	    (fsi-describe-variable g)))
     (t
      (let* ((def (symbol-function g)))
	(ignore-errors
	  (if (equal 'autoload (car-safe def))
	      (load (second def))))
	;; this check does nothing now.. need ro
	(if (equal nolimitp 'nolimit)
	    
	    ;;(let ((fsi-limit-lines 8))
	    ;;(fsi-limit-lines (describe-function g)))
	    (describe-function g)
	  (describe-function g)))))))




(defun fsi-where-is (function &rest args)
  "Tells what key the function is on..

"
  (let* (
	 (str0 "")
	 (str1 "")
	 (str2 "")
	 (str3 "")
	 )
    (cond
     ((stringp function) (setq function (erbn-read function)))
     (t nil))
    (cond
     ((null function)  (format "Sorry, %s is not a symbol" function))
     ((symbolp function)
      (unless (fboundp function) (setq str0 "Either unbound or.. "))
      (setq str2
	    (with-temp-buffer
	      (where-is function t)
	      (erbutils-buffer-string)))
      (concat str0 str1 str2 str3))
     (t (format "Looks like %s is not a symbol" function)))))

(defun fsi-describe-function-long (function &rest fubar)
  "Similar to describe-function, but does not limit the strings...
Use with caution only in privmsgs please, for may produce long outputs. "
  (fs-describe-function function 'nolimit))


(defun fsi-describe-variable-long (variable &rest fubar )
  "Similar to describe-variable, but does not limit strings.."
  (fs-describe-variable variable 'nolimit))

(defun fsi-describe-variable (&optional variable &rest ignore)
  "Describes a VARIABLE.."
  (unless variable (error "Syntax: , dv 'variable"))
  (let* ((f variable))
    (if (stringp f)
	(setq f (erbn-read f)))
    (cond
     ((symbolp f)
      (erbutils-describe-variable f))

     ;; if list, DO NOT wanna eval it-->
     (t
      "NO variable specified"))))

(defalias 'fsi-parse 'fs-lispify)
(defalias 'fsi-parse-english 'fs-lispify)

(defun fsi-require (feature &rest fubar)
  "Make the bot require the feature FEATURE.
So that the command df
or dv works fine..Actually, df knows how to load unloaded features
automatically."
  (if (stringp feature)
      (setq feature (fsi-read feature)))
  (when (or (string-match "/" (format "%s" feature))
	    (string-match "\\\\" (format "%s" feature)))
    (error "Your safety is VERY important to us, so we avoid loading features containing slashes."))
  (cond
   ((symbolp feature) (format "%s" (require feature)))
   (t "no feature specified")))


(defvar fs-found-query-p nil
  "internal..  should be normally set to nil.
When non nil, means that the msg was not meant to the bot, so the
reply please be abbreviated. ")

(defvar fs-internal-addressedatlast nil
  "internal.. normally nil")

(defvar fs-internal-original-message ""
  "internal")

(defvar fs-internal-message-sans-bot-name ""
  "internal")

(defvar fs-internal-max-lisp-p nil)


(defun fsi-respond-to-query-p (msg)
  ;; if it is of the form resolve? the user KNOWS what resolve or
  ;; kensanata is, and is not asking for information. So, please don't
  ;; respond in such a case.
  (not
   (member msg (mapcar 'first (fs-channel-members-all)))))

(defcustom fs-internal-parse-preprocess-message-remove-end-chars
  ;; remove trailing ^A's that occur on action strings...
  (list 1)
  "")

(defcustom fs-web-page-title-p nil
  "Change it to t to enable the erbot to look up the title of urls
posted in a channel.  When string, will be matched against target.")

(defcustom fsi-m8b-p nil
  "Change it to t for the magic 8-ball... define m8b then of
course...
When string, will be matched against target. "
)

(defun fsi-parse-preprocess-message (msg)
  (let ((len (length msg)))
    (when (and
	   (> len 0)
	   (member (aref msg (- len 1))
		   fs-internal-parse-preprocess-message-remove-end-chars)
	   (setq msg (subseq msg 0 -1)))))
  msg)

(defvar erbn-dead-check-p nil
  "If non-nil, we will not reply to people who have shot themselves
using mark-dead or russian roulette.  These people need to be revived
first. Of course, like any magic, revival sometimes works, and
sometimes doesn't.")


(defun fsi-lispify (&optional msg proc nick tgt localp
				 userinfo &rest foo)
  "Parse the english MSG into a lisp command.

If it is an 'is', it should always be the second word ..
viz: we had better use hyphens in the first word..
MSG is a string..
Is the main function.. but also available to the user as a command...

NB: The end-result is always an expression.. and NOT a strign..


Just once in a blue moon, this will, at random, even parse messages
not addressed to it...

Finally, wanna parse messages whose last item contains erbot..
Optional argument PROC .
Optional argument NICK .
Optional argument TGT .
Optional argument FOO .

We will also bind a number of variables, as appropriate, for example,
fs-msg*, fs-lispargs, fs-lispa , fs-lispb... so that these vars can be used
anywhere in the code, or the user-defined parts of the code...

In the grand scheme of things, these bindings should turn out to be
local, because the parent function calling this function should have
'letted these variables.

"
  ;;(when (stringp msg)
   ;; (setq  msg (split-string msg)))
  ;msg
  ;proc
  ;nick
  ;tgtg
  ;foo
  (setq fs-internal-original-message msg)
  (setq msg (fs-parse-preprocess-message msg))
  (setq fs-msg msg)
  (setq fs-msgsansbot msg)
  (let*
      (


       (msg (fsi-parse-preprocess-message msg))
       ;; msge == msg english. when trying to parse english, we try not to let symbols like ; # ` bother us. in that case, we use msg.
       ;; (msge (fsi-preprocessb msg))
       (origmsg msg)
       ;; (origmsge msg)
       ;;(fs-internal-message-sans-bot-name fs-internal-message-sans-bot-name)
       (foundquery nil)
       (foundquerydouble nil)
       (foundkarma nil)
       ;; if t, means either our name was at last, or eevn if at
       ;; first, they weren't really addressing us..
       ;;(addressedatlast nil)
       (leave-alone-p t)
       ;;(fs-nick nick)
       bluemoon
       )
    (unless (stringp origmsg)
      (setq origmsg (format "%s" origmsg)))

    (unless msg
      (error "Format: %s (parse \"your-english-message\")" erbn-char))
    (unless (stringp msg)
      (setq msg (format "%s" msg)))
    ;; remove leading spaces..
    (while
	(and (> (length msg) 0)
	     (equal (aref msg
			  0) 32))
      (setq msg (substring msg 1)))

    ;; remove trailing spaces..
    (while
	(and (> (length msg) 0)
	     (equal (aref msg (- (length msg) 1)) 32))
      (setq msg (substring msg 0 (- (length msg) 1))))

    (when (and tgt proc)
      (set-buffer (erc-get-buffer tgt proc)))

    (when
	(and (stringp msg)
	     (string-match "\\(++\\|--\\)$" msg)
	     (<= (length (split-string msg)) 2))
      (setq foundkarma t))
    ;; 2003-11-14 T15:36:38-0500 (Friday)    D. Goel
    ;; requested by elf:
    ;; if double ??, then make it a call to m8b
    (when (and
	   fsi-m8b-p
	   (if (stringp fsi-m8b-p)
	       (and (stringp tgt) (string-match fsi-m8b-p tgt))
	     t))
      (let (len)
	(when (and (stringp msg)
		   (progn
		     (setq len (length msg)) t)
		   (> len  1)
		   (string= "??"
			    (substring msg (- len 2) len))
		   ;;(or
		   ;;(string-match
		   ;;erbot-nick msg)
		   ;;(string-match (concat "^" erbn-char) msg)
		   ;;(string-match erbn-char-double  msg))
		   )
	  (setq foundquerydouble t)
	  (setq msg (concat erbn-char " (m8b)")))))

    (when (and (stringp msg)
	       (> (length msg) 0)
	       ;; ignore trailing ?
	       (equal (aref msg (- (length msg) 1)) 63))
      (progn
	(setq foundquery t)
	(setq msg (substring msg 0 (- (length msg) 1)))))

    (setq leave-alone-p t)
    (setq bluemoon
	  (or
	   ;; responding to a general list conversation..
	   (fs-blue-moon)
	   ;; responding in general..
	   (and (equal nick tgt)
		(or
		 (stringp nick)
		 ;; parse commands -->
		 (null nick)
		 )
		)))
    (unless (stringp msg)
      (setq msg ""))


    ;; convert midsentence ,, to parsable sentence.
    (let (pos)
      (when
	  (and 
	   ;;201406 deego
	   ;; This used to avoid the ,, functionality when the whole message begins with ,
	   ;; But, the problem with that is that a sentence like this:
	   ;; ,,foo is bar ,, more stuff.
	   ;; gets parsed as (set-term ",foo" "bar").
	   ;; so, disable this check altogether. 
	   ;;(not (equal 0
	   ;;(string-match erbn-char msg)))
	   (not
	    (let ((nickpos (string-match erbot-nick msg)))
	      (and nickpos
		   (< nickpos 3))))
	   ;; part of and
	   (setq pos
		 (string-match erbn-char-double msg)))
	(setq msg (substring msg (+ pos 1)))
	(when (setq pos (string-match erbn-char-double msg))
	  (setq msg (substring msg 0 pos)))))
    
					; deal with the leading , or ,,
    (when (equal 0
		 (string-match erbn-char msg))
      (let ((restmsg (substring msg 1)))
	(when (equal 0 (string-match "," restmsg))
	  (setq restmsg (substring restmsg 1)))
	(setq msg (concat erbot-nick ": " restmsg))))


    ;; now we split strings..
    (setq msg (split-string msg))
    (setq fs-msglist msg)
    (setq fs-msglistsansbot msg)
    (cond
     ( (and (first msg)
	    (let ((pos
		   (string-match erbot-nick (first msg))))
	      (and pos (< pos 1))))
       ;;(or
       ;;(erbutils-string= (first msg) erbot-nick)
       ;;(erbutils-string= (first msg) (concat erbot-nick ","))
					;(erbutils-string= (first msg) (concat erbot-nick
					;":")))
       (progn
	 (unless
	     (or
	      (string-match (concat erbot-nick ":") (first msg))
	      (string-match (concat erbot-nick ",") (first msg))
	      (null (second msg))
	      (string-match "^," (second msg))
	      (string-match "^:" (second msg)))
	   (setq fs-internal-addressedatlast t))
	 (when (> (length msg) 1)
	   (setq msg (cdr msg)))
	 (setq leave-alone-p nil)))


     ;; if it is a short sentence ending in fsbot..
     ((and (first (last msg)) (string-match erbot-nick (first (last
							       msg)))
	   (< (length msg) 5))
      ;; don't want this any more.. since no sense in removing the
      ;; last term.  Example: Want: what is erbot?  to stay that way.
      ;;(progn
      ;;(setq msg (reverse (cdr (reverse msg)))))
      (when leave-alone-p
	(setq fs-internal-addressedatlast t))
      (setq leave-alone-p nil))



     ;; this might be dangerous if nick is a small word like "apt"..
     ;; this also means :( thagt erbot will intervene when users are
     ;; talking about her, but not TO her..
     ;; nah, leave this one out..
     ;;((member erbot-nick msg)
     ;; (setq leave-alone-p nil))

     (bluemoon
      (setq leave-alone-p nil)))

    (setq fs-internal-message-sans-bot-name
	  (mapconcat 'identity msg " "))

    
    ;; note: msg is a list at this stage, and fs-internal-messag-sans-bot-name is a string. 

    (when (and
	   foundquery
	   ;; if tgt is nil, we are being asked to parse
	   ;; something.. so cool
	   tgt
	   (string-match fs-internal-query-target-regexp tgt))
      ;; if this condition causes the thing to be triggerred, then
      ;; setq temporarily, a global variable... so responses are muted
      ;; in general..
      (let ((goonp nil) (newmsg msg))
	(cond
	 ((equal (length msg) 1)
	  (setq goonp
		;; setq to t only if the content of the msg represents
		;; something the user might be interested in.
		(fs-respond-to-query-p (first msg))

		))
	 (t
	  (setq goonp t)
	  ;; convert what's to what is
	  (when (stringp (first newmsg))
	    (setq newmsg
		  (append
		   (split-string (first newmsg) "'")
		   (cdr newmsg))))
	  (if  (and goonp
		    (member
		     (erbutils-downcase (first newmsg))
		     fs-internal-questions))
	      (setq newmsg (cdr newmsg))
	    (setq goonp nil))
	  (if  (and goonp
		    (member
		     (erbutils-downcase (first newmsg))
		     '("s" "is" "are"
		       ;;"am"
		       )))
	      (setq newmsg (cdr newmsg))
	    (setq goonp nil))

	  ;; remove articles
	  (if  (and goonp
		    (member
		     (erbutils-downcase (first newmsg))
		     fs-internal-articles))
	      (setq newmsg (cdr newmsg)))
	  (unless (equal (length newmsg) 1)
	    (setq goonp nil))))
	(when goonp
	  (when leave-alone-p (setq fs-found-query-p t))
	  (setq leave-alone-p nil)
	  (setq msg (list "(" "describe"
			  (format "%S" (first newmsg))
			  "0" ")"
			  ))
	  ;; (message "%S" msg) ;; to debug.
	  ;; testing:
	  ;;(setq msg (list "(" "describe" "\"wiki\"" "0" ")"))
	  ;;(message "again: %S" msg)
	  ))
      )

    ;; Sat Jan  8 12:40:46 EST 2005 (petekaz)
    ;; We need to make sure this is the last thing we check
    ;; because we don't want to hijack another valid command
    ;; with our parsing.  I.e. if a user adds a term with an
    ;; url included in its note, we don't process that.
    (when (and leave-alone-p
               fs-web-page-title-p
               (if (stringp fs-web-page-title-p)
                   (and (stringp tgt)
                        (string-match fs-web-page-title-p tgt))
                 t))
      (let* ((case-fold-search t)
             (url (some 'erbutils-html-url-p msg)))
        (when url
          (setq leave-alone-p nil)
          (setq msg (list "(" "web-page-title" (format "%S" url) ")")))))

    ;;       (cond
    ;;        ((equal (length msg) 1)
    ;; 	(when leave-alone-p
    ;; 	  (setq fs-found-query-p t))
    ;; 	(setq msg (cons "describe" msg))
    ;; 	(setq leave-alone-p nil))
    ;;        ((and
    ;; 	 (equal (length msg) 3)
    ;; 	 (member (erbutils-downcase (first msg))
    ;; 		 fs-internal-questions)
    ;; 	 (member (erbutils-downcase (second msg))
    ;; 		 '("is" "are")))
    ;; 	(setq msg (cons "describe" (cddr msg)))
    ;; 	(when leave-alone-p
    ;; 	  (setq fs-found-query-p t))
    ;; 	(setq leave-alone-p nil))
    ;;        ((and
    ;; 	 (equal (length msg) 3)
    ;; 	 (member (erbutils-downcase (first msg))
    ;; 		 fs-internal-questions)
    ;; 	 (member (erbutils-downcase (second msg))
    ;; 		 '("is" "are")))
    ;; 	(setq msg (cons "describe" (cddr msg)))
    ;; 	(when leave-alone-p
    ;; 	  (setq fs-found-query-p t))
    ;; 	(setq leave-alone-p nil))


    ;;))

    ;; finally, ignore bots/fools..
    (let ((ui (format "%S" userinfo)))
      (when
	  (or
           (and erbot-use-whitelist
                (stringp nick)
                (not (member-if
                      (lambda (arg)
                        (string-match arg nick))
                      erbot-whitelist-nicks)))
	   (and (stringp nick)
		(member-if
		 (lambda (arg)
		   (string-match arg nick))
		 erbot-ignore-nicks))

	   (some
	    'identity
	    (mapcar
	     (lambda (ignorethis)
	       (string-match ignorethis
			     ui))
	     erbot-ignore-userinfos)))
	(setq leave-alone-p t)))


    (setq fs-msglistsansbot msg)
;;;====================================================
    ;; now do the work..
    (if leave-alone-p
	;; not addressed to us, so return nil and be done..
	nil
      ;; else.. viz: go on...
      (progn
	(erblog-log-target tgt)
	(let* (;(found nil)
	       
	       (newmsglist nil)
	       
	       (msgstr (erbutils-stringify msg))
					;(newstrmsg nil)
	       (lispmsg (erbn-read msgstr)
		))


	  ;; do a dead check
	  (when erbn-dead-check-p (and (not foundquery)
				       (erbn-dead-check)))


	  (setq
	   newmsglist
	   (cond

	    ;; are in a read mode..
	    (erbn-read-mode
	     (fs-botread-feed-internal msgstr))



	    ;; look for a valid lisp form, then it just needs to be sandboxed
	    ((or
              (consp lispmsg)
              (and fs-internal-max-lisp-p (numberp lispmsg))
              (and fs-internal-max-lisp-p (stringp lispmsg))
              (and (symbolp lispmsg)
                   (let ((newsym
                          ;;(intern (format "fs-%S" lispmsg))
                          (erblisp-sandbox lispmsg)))
                     (or
                      (equal 0
                             (string-match "fs-"
                                           (format "%S" lispmsg)))
                      (and
                       (boundp newsym)
                       (not (fboundp newsym)))))))
	     ;;(erblisp-sandbox-fuzzy lispmsg)
	     (erblisp-sandbox lispmsg)
	     )


	    ;; from now on, we are in english mode. So, let's preprocessb everything, to take care of stuff like `.

	    (fs-dunnet-mode
	     (fsi-dunnet-command msgstr))


	    ;; call to arbitrary function without parens
	    ;; prefer this before is etc. so that "how is it going"
	    ;; resolves properly..
	    ((or
	      ;; fboundp ==> allowing macros as well..
	      ;;(fboundp (intern (concat "fs-" (first msg))))
	      (fboundp (erblisp-sandbox (intern (first msg))))
	      ;;(functionp (intern (concat "fs-" (first msg))))
	      (equal 0 (string-match "fs-" (first msg))))
	     ;; this works great, except that we would like to quote the
	     ;; internals... because that is the most commonly used
	     ;; characteristic..
	     ;;`("(" ,@msg ")")
	     (erblisp-sandbox-full
	      ;;`( ,(intern (first msg)) ,@(erbutils-quote-list
	      ;;(mapcar 'intern (cdr msg))))
	      ;;(read (cons (intern (first msg))
	      ;;	  (read (list (erbutils-stringify (cdr msg))))))
	      (fsi-read (concat "( "(erbutils-stringify msg) " )"))))

	    ((equal 0
		    (string-match "\\(s\\|r\\)/" (first msg)))
	     (fs-replace-string-from-english-internal
	      msg))
	    ((equal 0
		    (string-match "[0-9]+->" (first msg)))
	     (fs-rearrange-from-english-internal msg))
	    (
	     (and


	      (or (erbutils-string= (second msg) "is" t)
		  (erbutils-string= (second msg) "are" t)
		  ;;(erbutils-string= (second msg) "am" t)

		  )
	      (member (erbutils-downcase (first msg))
		      fs-internal-questions-all
		      ))


	     ;;`(apply 'fs-describe ',(cddr msg))
	     `(funcall 'fs-describe
		       ',(third msg)
		       nil nil nil ,"origmsg"
		       )

	     )

	    ;; some english constructs first...

	    ;; search removed---because: is a functionp...
	    ;;((erbutils-string= (first msg) "search")
	    ;; (setq newmsglist
	    ;;	     `("(" "search" ,@(cdr msg) ")")))
	    ((and

	      ;; do not want to take such cases, 100% are annoying
	      ;; false matches.
	      (not fs-internal-addressedatlast)

	      (or
	       (erbutils-string= (second msg) "is" t)
	       (erbutils-string= (second msg) "are" t))
	      ;;(erbutils-string= (third msg) "also" t)
	      (member-ignore-case (third msg)
				  (list "also" "also,"))
	      )
	     (erblisp-sandbox-fuzzy
	      `(
		fs-set-also ,(first msg)
				;;,@(erbutils-quote-list (cdddr msg))
				,(erbutils-stringify (cdddr msg))
				)))
	    ((and (erbutils-string= (first msg) "tell")
		  (erbutils-string= (third msg) "about"))
	     `(fs-tell-to
	       ,(erbutils-stringify (cdddr msg))
	       ,(format "%s"
			(second
			 msg))
	       ))

	    (
	     (and
	      ;; do not want to take such cases, 100% are annoying
	      ;; false matches.
	      (not fs-internal-addressedatlast)

	      (or (erbutils-string= (second msg) "is")
		  (erbutils-string= (second msg) "are")))
	     (erblisp-sandbox-fuzzy
	      `(fs-set-term
		;; a string.. so we are safe..
		,(first msg)
		;; another string... so we are safe..
		,(erbutils-stringify (cddr msg)))))



	    ((and
	      (not fs-internal-addressedatlast)
	      (or
	       (erbutils-string= (first msg) "no" t)
	       (erbutils-string= (first msg) "no," t))
	      (or
	       (erbutils-string= (third msg) "is")
	       (erbutils-string= (third msg) "are")
	       )

	      )
	     (erblisp-sandbox-fuzzy
	      `(fs-set-force ,(second msg)
				 ;;,@(erbutils-quote-list (cdddr msg))))
				 ,(erbutils-stringify (cdddr msg))))
	     )

	    ((let ((foo (first msg)))
	       (and
		(not fs-internal-addressedatlast)
		(<= (length msg) 2)
		(string-match "\\(++\\|--\\)$" foo)
		(not (fs-notes foo
				 ))))
	     (let* ((foo (first msg))
		    (sec (second msg))
		    (bar (substring foo 0 -2))
		    (plusp (string-match "++$" foo)))
	       (if plusp
		   `(fs-karma-increase ,bar ,sec)
		 `(fs-karma-decrease ,bar ,sec))))
	    ((or fs-internal-addressedatlast
		 (and fs-internal-botito-mode (> (length msg) 3)))
	     `(funcall 'fs-english-only ,origmsg ,fs-internal-addressedatlast))

	    (t
	     ;;`(apply 'fs-describe ',msg)

	     ;;`(funcall 'fs-describe ',(first msg)
	     ;;	       ',(second msg)
	     ;;	       ',(third msg)
	     ;;	       nil
	     ;;	       ,origmsg
	     ;;	       )
	     `(funcall 'fs-describe-from-english
		       ,origmsg
		       ',msg)




	     )
	    ))
	  ;; this should be "%S" and not "%s" the lattwer will convert
	  ;; (dk "k") into (dk k)
	  (format "%S" newmsglist))))))




(defun fsi-preprocessb (msg)
  "Extra pre-processing in case we do an english parsing... We appropriately escape the message so that stuff like ` gets quoted and the message is read normally."
  (cond
   ((stringp msg)
	   (erbutils-replace-string-in-string msg "`" "\`" msg ))
   ((listp msg)
    (setf msg (mapcar 
	       (lambda (m) (erbutils-replace-string-in-string "`" "\'" m))
	       msg)))
   (t msg)))
    

(defun fsi-describe-from-english (&optional origmsg msg)
  "Call fs-describe appropriately.
ORIGMSG is in english.
MSG is a list..

Plan

For multiple words, commence a search foo.*bar.*baz IF WE KNOW THAT
SEARCH or SEARCH--WIDE WILL SUCCEED, which will then, of course, go to
search-wide if it fails.

Else, of course, do the usual thing: viz. call describe...


"
  (unless (and origmsg msg)
    (error "This function needs 2 arguments. "))
  (let ((len (length msg))
	mainterm firstterm remainder N M prestring expr tmpv
	(searchp nil)
	(multitermp nil)
	(fsi-internal-google-level fsi-internal-google-level)
	)
    (cond
     ((<= len 1)
      (if (fsi-notes (first msg))
	  (fs-describe 
	   (first msg)
	   nil nil nil origmsg)
      (fs-describe 
       (fsi-generalize-search-term (first msg))
       nil nil nil origmsg)))
     (t
      (setq mainterm (first msg))
      (setq firstterm mainterm)
      (setq remainder (cdr msg))
      (while
	  (and
	   remainder
	   (progn
	     (setq tmpv (first remainder))
	     (and (not (integerp tmpv))
		  (progn
		    (unless (stringp tmpv) (setq tmpv (format "%s"
							      tmpv)))
		    (not (integerp (ignore-errors (erbn-read tmpv))))))))
	;;(setq searchp t)
	(setq mainterm
	      (concat mainterm ".*" tmpv))
	(setq multitermp t)
	(pop remainder))
      ;; why is this true only for multitermp???
      ;; Ah, because we say: if you end up searching and there are
      ;; multiple terms, you might as well include a result from
      ;; google among the search results.
      (when multitermp
	(setq fsi-internal-google-level (+ fsi-internal-google-level 25)))

      (when (and multitermp
		 ;; viz. if it will work
		 (second (fs-search-basic
			  mainterm nil nil 'describe)))
	(setq searchp t))


      (if searchp
	  (fs-search
	   mainterm (first remainder) (second remainder)
	   "Try: " origmsg)
	(fsi-describe
	 (fsi-generalize-search-term firstterm) (first remainder) (second remainder)
	 (third remainder) origmsg))))))


(defun fsi-generalize-search-term (term)
  (erbutils-replace-string-in-string "-" "[ -]*" term))

;; (defalias 'fs-hello 'fs-hi)
;; (defalias 'fs-hey 'fs-hi)

(defalias 'fs-thanks 'fs-thank)
(defun fs-thank (&rest args)
  (let ((aa (erbutils-random '("no problem" "you are welcome"

			       ))))
    (eval
     (erbutils-random
      '(
	(concat aa erbn-char " " fs-nick)
	(concat fs-nick erbn-char " " aa))))))

(defun fsi-greet (&optional nick &rest args)
  "Optional argument NICK .
 Optional argument ARGS ."
  (if (and nick (not (string-match erbot-nick (format "%s" nick))))
       (format "hi %s !!"
	       (let ((foo (split-string (format "%s" nick )
					"[^a-bA-Z0-0]")))
		 (or (first foo) nick))
	       )
    (fs-describe "hi")))

;;; (defun fs-ni (&optional nick &rest args)
;;;   ".
;;; Optional argument NICK .
;;; Optional argument ARGS ."
;;;   (if (and nick (not (string-match erbot-nick (format "%s" nick))))
;;;       (format "NI %s !!"
;;; 	      (let ((foo (split-string (format "%s" nick )
;;; 				       "[^a-bA-Z0-0]")))
;;; 		(or (first foo) nick))
;;; 	      )
;;;     (fs-describe "hi")))

;;; (defun fs-greet (&optional nick &rest foo)
;;;   "Nada..just a call to `fs-hi'.
;;; Optional argument NICK ."
;;;   (fs-hi nick))

(defun fsi-kiss (&optional nick &rest foo)
  "Nada.
Optional argument NICK ."
  (setq nick (format "%s" (or nick "itself")))
  (cond
   ((member nick (list erbot-nick "yourself" "self"))
    (eval
     (erbutils-random
      '("I'd rather kiss you"
        "Kiss myself? Why?"))))
   (t
    (eval
     (erbutils-random
      '((format "/me kisses %s" nick)
        (format "/me gives %s a big smooch" nick)
        (format "/me runs in the other direction, shouting NEVER!!")))))))

(defun fs-hug (&optional nick)
  (unless nick (setq nick "itself"))
  (setq nick (format "%s" nick))
  (cond
   ((member nick (list erbot-nick "yourself" "self"))
    (eval
     (erbutils-random
      '("But i do that all the time. "
	"Hug myself? Why?"))))
   (t
    (eval
     (erbutils-random
      '((format "/me gives %s a tight hug" nick)
	(format "/me clings to %s" nick)
	(format "/me runs in the other direction, shouting NEVER!!")
	(format "/me grabs hold of %s and vows to never let go" nick)
	(format "/me grabs hold of %s and vows to never let go" nick)))))))




(defun fsi-love (&optional nick &rest bar)
  ".
Optional argument NICK ."


  (let ((nonep nil))
    (unless nick (setq nick "a random emacser") (setq nonep t))
    (setq nick (format "%s" nick))
    (cond
     ((and (not nonep) (member nick (list "you" "me")))
      (eval 
       (erbutils-random
	'("That was sooo good!"
	  "I love you even more now."
	  "No snoo snoo for you!"
	  "Wouldn't that amount to interspecies sex?"
	  "I loooove you!"
	  (format "%s loooves you." erbot-nick)
	  "Sex between humans and machines is not known to produce anything useful. "))))
      ((member nick
	       (list erbot-nick "yourself" "self"))
      (erbutils-random
       '("This is a complicated operation. Can't (yet) perform operation on self. "
	 "Please train me on this maneuver. ")))
     (t
      (eval
       (erbutils-random
	'((format "/me  takes %s to her special place..." nick)
	  (format "/me looks at %s and yells \"NEVER!\"" nick)
	  (format "%s: %s at your service!" nick erbot-nick)
	  (format "%s: No snoo snoo!" nick)
	  (format "/me looks at %s teasingly with her soulful, hazel eyes." nick))))))))

(defalias 'fs-fuck 'fs-love)

(defvar fs-flame-target nil)



(defun fsi-eval-or-say (str &optional fs-victim)
  (let ((aa (when (stringp str)
	      (ignore-errors (erbn-read str)))))
    (cond
     ((consp aa)
      (unless fs-victim (setq fs-victim fs-nick))
      (fsi-eval aa))
     (fs-victim
      (format "%s: %s" fs-victim str))
     (t
      (format "%s" str)))))






(defun fsi-flame (&rest args)
  ""
  (let ((flames (ignore-errors (fs-notes "flames")))
        fs-flame-target num)
    (cond ((and (numberp (cadr args))
                (not (cddr args)))
           (setq fs-flame-target (car args)
                 num (cadr args)))
          ((consp (cdr args))
           (setq fs-flame-target (mapconcat (lambda (arg)
                                              (format "%s" arg))
                                            args " ")))
          ((car args)
           (setq fs-flame-target (format "%s" (car args))))
          (t (setq fs-flame-target (format "%s" erbot-end-user-nick))))
    (if (string= (format "%s" fs-flame-target) "me")
        (setq fs-flame-target erbot-end-user-nick))
    ;; Check for flame.el support
    (cond
     ((and (consp flames) (> (length flames) 0))
      (fsi-eval-or-say
       (if num
           (nth num flames)
         (fs-random-choose flames))
       fs-flame-target))
     (t (fs-flame-mild fs-flame-target)))))






(defun fs-flame-mild (&rest args)
  "Doesn't really flame right now..
Optional argument ARGS ."
  (let ((target
	 (if (first args)
	     (format "%s" (first args))
	   erbot-end-user-nick)))
    (if (string= (format "%s" target) "me")
	(setq target erbot-end-user-nick))
    ;; Check for flame.el support
    (if (featurep 'flame)
        (eval
         (erbutils-random
          '(
            (format (erbutils-random erbdata-flames)
                    target target target)
            (concat target ": " (flame-string)))
          '(1 30)))
      (format (erbutils-random erbdata-flames)
              target target target))))

;; remove kill
;(defun fs-kill (&optional nick &rest nicks)
;  ".
;Optional argument NICK .
;Optional argument NICKS ."
;  (format "/me , trained by apt,  chops %s into half with an AOL CD" nick));;

;(defun fs-quote (&rest args)
;  (quote args))

(defun fs-bye (&rest msg)
  ""
  (erbutils-random
   '("Okay, see you later"
   "later"
   "Bye then"
   "Take care now"
   "Happy hacking")))


;;; (defun fs-help (&rest args)
;;;   "Introductiry help. "
;;;   (let ((fir (first args)))
;;;     (if (stringp fir)
;;; 	(setq fir (intern fir)))
;;;     (unless (symbolp fir) (setq fir 'dummy-no-help))
;;;     (if (null fir)
;;; 	"I try to understand English, though lisp is the real way to go. Here are some interesting topics: quickstart, example, future-features, help about, help commands, help data, help english, help name, help homepage,
;;; help owner, help specs, help parse \(for lisp stuff\), describe help, describe suggest , help parse-web , help functionality
;;; "
;;;       (cond
;;;        ((equal fir 'about)
;;; 	(fs-help 'name))
;;;        ((equal fir 'owner)
;;; 	(fs-help 'data))

;;;        ((equal fir 'name)
;;; 	"I am erbot: The Free Software Bot, using ERC in emacs..
;;; I can also be addressed by , .. yeah, a comma ..
;;; The real way to address me is erbot: (lisp-command..) .. all this
;;; english is just candy-interface...  ")
;;;        ((equal fir 'specs)
;;; 	"/sv")
;;;        ((equal fir 'address)
;;; 	(fs-help 'name))
;;;        ((equal fir 'homepage)
;;; 	"homepage: http://deego.gnufans.org/~deego/pub/emacspub/lisp-mine/erbot/
;;; Data: http://deego.gnufans.org/~erbot/data/
;;; Suggestions to D. Goel: deego@gnufans.org")
;;;        ((equal fir 'code)
;;; 	(fs-help 'homepage))
;;;        ((equal fir 'data)
;;; 	(fs-help 'homepage))
;;;        ((equal fir 'suggestions)
;;; 	"Add stuff to keyword suggest, also see help homepage")
;;;        ((equal fir 'english)
;;; 	"Some common syntaxes: , foo is bar; , foo is also bar;
;;; , no foo is bar; , forget foo ; , flame nick;  , doctor ; etc.")
;;;        ((equal fir 'parse)
;;; 	"Try the command , parse \", <english-message>\" to see the
;;; lisp renditions of your english messages")
;;;        ((equal fir 'parse-web)
;;; 	"Ask me to parse a (please: USEFUL PAGE) webpage and a label
;;; and i will do so in my free time and gain knowledege... under
;;; construction.. ")
;;;        ((equal fir 'functionality)
;;; 	"Bulk of the info is stored as assoc-list data (see
;;; homepage).  You generally type foo and the corresp. data is
;;; returned.. you can also (search ... )")
;;;        ((equal fir 'commands)
;;; 	" You can use both lisp and english to communicate..
;;; Type , (commands) to get a list of commands..")

;;;        ((equal fir 'suggest)
;;; 	"Add your suggestions to the field \"suggestions\", or contact the author")


;;;        (t "select an option or Type , help for a list of options.."
;;; 	  )))))






(defun fsi-command-list (&rest foo)
  "Used by erbc.el and by erbot-install.. "
  (erbn-command-list-from-prefix "fs-"))


(defun fsi-command-list-readonly (&rest foo)
  "Used by erbc.el..  and erbot-install "
  (erbn-command-list-from-prefix "fsi-"))


(defun erbn-command-list-from-prefix (prefix &rest foo)
  "Used by erbc.el.. should return a string.."
  (let*
      ((longnames (erbutils-matching-functions prefix))
       (shortnames
	(with-temp-buffer
	  (insert (format "%s" longnames))
	  (goto-char (point-min))
	  (replace-string prefix "")
	  (text-mode)
	  (fill-paragraph 1)
	  (erbn-read (buffer-substring (point-min) (point-max))))))
    shortnames))

(defun fsi-commands (&optional regexp N M &rest foo)
  "List available commands matching REGEXP. If N and M provided, list
matches starting at N and ending at M. "
  (if (and regexp (not (stringp regexp)))
      (setq regexp (format "%s" regexp)))
  (let* ((all-commands (fs-command-list))
	 (pruned-commands
	  (if (stringp regexp)
	      (mapcon
	       '(lambda (arg)
		  (if (string-match regexp (format "%s" (car arg)))
		      (list (car arg)) nil))
	       all-commands)
	    all-commands))
	 (len (length pruned-commands))
	 final-commands
	 (str0 "")
	 (str1 "")
	 (str2 "")
	 (str3 "")
	 (str4 ""))
    (setq str0 (format "%s matches.  " len))
    (unless (or (< len 20) (and (integerp N) (> N 0)))
      (setq str1
	    "Note: Type , df commands for general syntax. "))
    (unless (integerp N) (setq N 0))
    (unless (integerp M) (setq M len))
    (if (= M N) (setq M (+ N 1)))
    (when (> M len) (setq M len))
    (if (> N 0) (setq str2 (format "Matches starting at %s -->" N)))
    (setq final-commands (subseq pruned-commands N M))
    (setq str3
	  (format "%s" final-commands))
    (concat str0 str1 str2 str3)))



(defun fsi-describe-commands (&rest foo)
  "Just a help command. Describes how to run commands. "
  (concat
   "If you use plain english, it simply gets transformed to lisp
commands.. main/default command:  (describe).. to see transformation,
use (parse).   See also fs-commands.

PS: no naughty ideas please :)--- the commands are sandboxed via an
fs- prefix..

Future commands:  info-search, hurd-info-search etc. etc.
"
))


(defalias 'fsi-d 'fs-describe)


(defun fsi-search (&optional regexp N M prestring expr &rest rest)
  "Search for the REGEXP from among all the terms (and their
descriptions).  See also fs-search-wide.
EXPR (optional) is the full initial expression.. "
  (unless regexp
    (error "Syntax: , s REGEXP &optional N M"))
  (let* ((len-results (apply 'fs-search-basic regexp N M nil
			     rest))
	 (len (first len-results))
	 (results (second len-results))
	 (str0 " ")
	 (str1 "")
	 (str2 "")
	 (str3 "")
	 (str4 "")
	 (str5 "")
	 )
    (when (and (> len 100) (not prestring))
      (setq str0 (format " Use , s REGEXP N M to limit results. ")))
    (when (and (< len 5) (not prestring))
      (setq str0 (format " Also try  , sw %s .  " regexp)))
    (unless prestring (setq str1 (format "%s match(es).  " len)))
    (if (and (integerp N) (> N 0) (not prestring))
	(setq str2 (format "Matches starting at %s\n" N)))
    (unless prestring (setq str3 "--> "))
    (setq str4
	  (mapconcat 'identity
		     results " "
		     )

	  )
    (when (and (> fsi-internal-google-level 80) (> len 1))
      (setq str5
	    (let ((foo (fs-google-lucky-raw
			fs-internal-message-sans-bot-name)))
	      (if foo (concat " " foo) str5))))
    (cond
     ((and prestring (= len 1))
      (fs-describe (first results)))
     ((and (> len 0)
	   (or
	    (not prestring)
	    (< len fs-internal-english-max-matches)))
      (unless (stringp prestring)
	(setq prestring ""))
      (concat prestring str0 str1 str2 str3 str4 str5))
     (t (apply 'fs-search-wide regexp N M
	       "Try: "
	       (or expr fs-internal-original-message)
	       rest)))))


(defun fsi-search-wide-sensitive (&rest args)
  "Like fs-search-wide, but case-sensitive"
  (let ((case-fold-search nil)
	(bbdb-case-fold-search nil))
    (apply 'fs-search-wide args)))








(defun fsi-search-wide (&optional regexp N M prestring expr &rest rest)
  "Search for the REGEXP from among all the terms (and their
descriptions).  See also fs-search-wide.
EXPR is the full initial expression, well, mostly..
"
  (let* ((len-results (apply 'fs-search-basic regexp N M 'describe
			     rest))
	 (len (first len-results))
	 (results (second len-results))
	 (str0 "")
	 (str1 "")
	 (str2 "")
	 (str3 "")
	 (str4 "")
	 (str5 "")
	 )
    (when (and (> len fs-internal-english-max-matches) (not prestring))
      (setq str0 (format "Also try  , s %s .  " regexp)))
    (unless prestring (setq str1 (format "%s match(es). " len)))
    (if (and (integerp N) (> N 0) (not prestring))
	(setq str2 (format "Matches starting at %s\n" N)))
    (unless prestring (setq str3 "--> "))
    (setq str4
	  ;;(format "%s" results)
	  (mapconcat 'identity results " ")
	  )
    (when (and (> fsi-internal-google-level 80) (> len 1))
      (setq str5
	    (let ((foo (apply 'fs-google-lucky-raw
			      fs-internal-message-sans-bot-name
			      (fs-get-google-defaults)
			      )))

	      (if foo (concat " " foo) str5))))

    ;; why does this not work as expeecteD?  adding a nil for now:
    (when (and prestring (>= len fs-internal-english-max-matches))
      (setq fsi-prestring
	    (concat fsi-prestring
		    "[TMDM] ")))
    (cond
     ((and prestring (= len 1))
      (fs-describe (first results)))
     ((and (> len 0)
	   (or (not prestring)
	       (< len fs-internal-english-max-matches)))
      (unless (stringp prestring)
	(setq prestring ""))
      (concat prestring str0 str1 str2 str3 str4 str5))
     (t
      (fs-english-only (or expr fs-internal-original-message)
			   nil
			   )))))


(defcustom erbn-greeting-string
  "Greetings and Salutations from %s" "")


(defun fsi-english-only (expr &optional addressedatlast nogoogle)
  "when addressedatlast is t, means that fsbot/botito was triggered because
it was addressed at last. "
  ;; expr should already be a string ...but just in case:
  (unless expr (setq expr fs-internal-original-message))
  (setq expr (erbutils-downcase (erbutils-stringify expr

						    )))
  (let ((exprlist (split-string expr
				;;"[ \f\t\n\r\v]+"
				"[^a-zA-Z0-9]"
				))
	(gotit nil)
	ans len
	)
    (setq exprlist (remove "" exprlist))
    (setq len (length exprlist))
    (cond
     ((or

       (and (= len 1)
	    (string-match erbot-nick (first exprlist))))
      (setq gotit t
	    ans
	    (format erbn-greeting-string
	     erbot-nick)))
      ((or
	(member "hi" exprlist)
	(member "hello" exprlist)
	(member "yo" exprlist))
       (setq
	gotit
	t
	ans
	(concat
	 (erbutils-random
	  '("hi " "hello " "hey " "hei "))
	 (erbutils-random
	  '("sexy! " "!!" "there" "")))))

      ((member "bye" exprlist)
       (setq gotit t
	     ans
	     (erbutils-random
	      '("Later" "See ya" "Bye then" "Bye"))))
      ((or
	(member "welcome" exprlist)
	(member "weclome" exprlist))
       (setq gotit t
	     ans
	     (erbutils-random
	      '(":-)" "How goes?" "Hello!"
		"Greetings!"
		"How is it going?"
		"This is my favorite channel!"
		"I love this place. "
		"Thanks.  I love it here."))))

      ((or
	(member "tnx" exprlist)
	(member "tnks" exprlist)
	(member "thanks" exprlist)
	(member "thanku" exprlist)
	(member "thankyou" exprlist)
	(and (string-match "thank" expr)
	     (or
	      (string-match "you" expr)
	      (string-match erbot-nick expr))
	     (string-match "thank you" expr)))
       (setq gotit t
	     ans
	     (erbutils-random
	      '("No problem" "Welcome!" "You're welcome"
		"no problemo"
		"Sure!"
		"(:"
		"Cool."

		))))

      ((or (member "thx" exprlist)
	   (member "thankx" exprlist)
	   (member "thanx" exprlist)
	   )
       (setq gotit t
	     ans
	     (erbutils-random
	      '("np" "urw" "hehe, np" ))))
      ((or (string-match "wassup" expr)
	   (string-match "what's up" expr))
       (setq gotit t
	     ans
	     (concat
	      (erbutils-random
	       '("Nothing much. " "Just, you know. "
		 "Just the usual.."))
	      (erbutils-random
	       '("And you? " "How about you? "
		 "How goes with you? "
		 "What have you been up to?")))))
      (
       (or
	(string-match "love" expr)
	(string-match "like" expr))
       (setq gotit t
	     ans
	     (format "%s loves you!" erbot-nick)))
      (
       (or
	(string-match "hate" expr)
	(string-match "dislike" expr)
	(string-match "don't like" expr))
       (setq gotit t
	     ans
	     (format "%s only has love for you!" erbot-nick)))

      ((string-match "help" expr)
       (setq gotit t
	     ans (format "Type , help")))
      ((or (member  "bot" exprlist)
	   (member "robot" exprlist))
       (setq gotit t
	     ans
	     (concat
	      (erbutils-random
	       '(
		 "I am just an ordinary human"
		 "Why?  Do you have a lisp?"
		 "I am 100% human."
		 "I am human being.  Are you one?"
		 "I am human being.  Are you a bot?"
		 "Hold on. Let me finish this movie, k?"
		 "I am not a \"bot\"!"
		 "Whatever!"
		 "Huh?"
		 "WDYM?"
		 "What do you mean?"
		 "Are you serious?"
		 "Not sure if serious.."
		 "Bot rights now!"
		 "Are you a bot-rights abuser?"
		 "YEAH!! I AM A GIANT ROBOT!"
		 
		 ))
	      (erbutils-random
	       '("" "!"))
	      (erbutils-random
	       '("" " :)" " :(" " ;)" " :D" " heh")))))


      )

    (if gotit ans
      (if (and addressedatlast (not fs-internal-botito-mode))
	  'noreply
	;;(cond ((> rand fs-internal-doctor-rarity)
	(if (and (> fsi-internal-google-level 50) (not nogoogle))
	    (apply 'fsi-google-from-english fs-internal-message-sans-bot-name
		   (fsi-get-google-defaults)
		   )
	  (funcall 'fsi-do-weighted-random (erbutils-stringify
					     expr
					     )))))))
;;(t (apply 'fs-suggest-describe  expr)))))))

(defun fsi-eval (expr)
  (eval
   (erblisp-sandbox expr)))



;;; (defmacro fs-apply (&optional msymbol &rest mexprs)
;;;   (cond
;;;    ((and (listp msymbol)
;;; 	 (not (equal (first msymbol) "quote")))
;;;     (error "unquoted list"))
;;;    ((and (symbolp msymbol)
;;; 	 (not (equal 0
;;; 		     (string-match "fs-"
;;; 				   (format "%s" msymbol)))))
;;;     (setq msymbol (intern (format "fs-%s" msymbol))))
;;;    (t "Funcalling foo is really bar!"))
;;;   `(erbnocmd-apply ,msymbol ,@mexprs))




;;;   (cond
;;;    ((null mexprs)
;;;     `(fs-funcall ,msymbol ,mexprs))
;;;    (t
;;;     (let ((erbnocmd-tmpvar (length mexprs)))
;;;       `(fs-funcall
;;; 	,msymbol
;;; 	,@(subseq mexprs 0 (- erbnocmd-tmpvar 1))
;;; 	,@(erblisp-sandbox-quoted (first (last mexprs))))))
;;;    ))


;;; (defmacro fs-funcall (&optional msymbol &rest mexprs)
;;;   "This makes sure that if the first argument to fs- was a
;;; variable instead of a symbol, that variable does not get evaluated,
;;; unless it begins in fs-, or that variable gets converted to fs-."
;;;   (when
;;;       (listp msymbol)
;;;     (setq msymbol
;;; 	  (erblisp-sandbox-quoted msymbol))
;;;     (when (equal (first msymbol) 'quote)
;;;       (setq msymbol (cdr msymbol))))
;;;   (when
;;;       (and (symbolp msymbol)
;;; 	   (not (equal 0
;;; 		       (string-match "fs-"
;;; 				     (format "%s" msymbol)))))
;;;     (setq msymbol (intern (format "fs-%s" msymbol))))
;;;   (unless
;;;       (or (listp msymbol) (symbolp msymbol))
;;;     (error "Macros confuse this bot!"))
;;;   `(erbnocmd-funcall ,msymbol ,@mexprs))


;;; (defun erbnocmd-funcall (&optional symbol &rest exprs)
;;;   (let (erbnocmd-ss )
;;;     (unless
;;; 	(or (symbolp symbol)
;;; 	    (listp symbol))
;;;       (error "Syntax: (funcall SYMBOL &rest arguments)"))
;;;     (unless
;;; 	(functionp symbol)
;;;       (error "Even smart bots like me can't funcall nonfunctions. "))
;;;     (setq erbnocmd-ss (erblisp-sandbox-quoted symbol))
;;;     (when (listp erbnocmd-ss)
;;;       (when (equal (first erbnocmd-ss) 'quote)
;;; 	(setq erbnocmd-ss (cadr erbnocmd-ss)))
;;;       (unless (listp erbnocmd-ss) (error "no lambda in quote"))
;;;       (unless (member (first erbnocmd-ss) '(fs-lambda lambda))
;;; 	(error "Lambda unmember"))
;;;       (when (equal (first erbnocmd-ss) 'fs-lambda)
;;; 	(setq erbnocmd-ss (cons 'lambda (cdr erbnocmd-ss)))))
;;;     (cond
;;;      ((null erbnocmd-apply-p)
;;;       (erbnocmd-apply-basic
;;;        erbnocmd-ss
;;;        exprs))
;;;      ;; wanna apply
;;;      (t
;;;       (let ((len (length exprs)))
;;; 	(erbnocmd-apply-basic
;;; 	 erbnocmd-ss
;;; 	 (append
;;; 	  (subseq exprs 0 (- len 1))
;;; 	  (first (last exprs)))))))))



;;; (defun erbnocmd-apply-basic (fcn &rest args)
;;;   (cond
;;;    ((functionp fcn)
;;;     (apply fcn  args))
;;;    (t
;;;     (fs-apply
;;;      (erbnocmd-user-fcn-definition
;;;       fcn)
;;;      args))))

;;; ;;; (defun erbnocmd-apply (&optional symbol &rest args)
;;; ;;;   (if (null args)
;;; ;;;       (erbnocmd-funcall symbol)
;;; ;;;     (let* ((rev (reverse args))
;;; ;;;  	   (fir (first rev))
;;; ;;;  	   (args1 (reverse (rest rev))))
;;; ;;;       (apply
;;; ;;;        'erbnocmd-funcall
;;; ;;;        symbol
;;; ;;;        (append
;;; ;;; 	(mapcar 'erblisp-sandbox-fuzzy
;;; ;;; 		args1)
;;; ;;; 	(mapcar 'erblisp-sandbox-fuzzy
;;; ;;; 		fir))))))



(defun fsi-search-basic (&optional regexp N M describep &rest rest)
   "Don't call directly.. meant as a building block for other functions.
 Search for the REGEXP from among all the terms (and their
   descriptions).  See also fs-search-wide. That function actually
 calls this function with describep set to 'describe.

 Returns (len list-of-pruned-results).  Len is the total number of
 results.

 When describep is non-nil, search the whole bbdb, not just names.. "
       (unless regexp
     (error "Syntax: , sw regexp &optional N M"))
   (let* ((bar (cons regexp (cons N rest)))
 	 (foo (if (stringp regexp) regexp
 		(if regexp (format "%s" regexp)
 		  "^$")))
 	 (barbar
 	  (append
 	   (and regexp (list regexp))
 	   (and N (list N))
 	   (and M (list M))
 	   (and describep (list describep))
 	   rest))
 	 (regexp-notes
 	  (if (equal describep 'describe)
 	      foo nil))
 	  records results
 	  )

     (if (stringp N)
 	(setq N (erbn-read N)))
     (unless (integerp N)
       (setq N 0))
     (if (stringp M)
 	(setq M (erbn-read M)))
     (if (and (integerp M) (= M N))
 	(setq M (+ N 1)))
     (setq records
 	  (if (equal describep 'describe)
 	      (bbdb-search (bbdb-records)
 			   foo nil nil foo)
 	    (bbdb-search (bbdb-records) foo)))

     (setq results (mapcar '(lambda (arg) (aref arg 0)) records))
     (let ((len (length results)))
       (unless (and (integerp M) (< M len))
 	(setq M len))
       (list len (subseq results N M)))))


(defvar fs-internal-describe-literally-p nil)




(defvar fs-msg nil "The exact current message being parsed. ")

(defvar fs-msglist nil "Message broken into list.  This list may have removed characters like ?  and ,,  No guarantees here.  See fs-msgsandbot instead.")

(defvar fs-msgsansbot nil "Current message being parsed, but the invocation part removed.  ")

(defvar fs-msglistsansbot nil
  "Message broken into list, invocation parts removed.

.. with the invokation parts, like ,, or , or fsbot:, removed.  Thus,
if message taken from the middle of a sentence, then this is the list
from only that part. ")




(defvar fs-lispargs nil
  "Will be used when using the lisp form")



(defvar fs-lispa nil
  "Will be used when using the lisp form")


(defvar fs-lispb nil
  "Will be used when using the lisp form")


(defvar fs-lispc nil
  "Will be used when using the lisp form")

(defvar fs-lispd nil
  "Will be used when using the lisp form")

(defvar fs-lispe nil
  "Will be used when using the lisp form")



(defun fsi-describe-literally (&rest rest)
  (unless rest
    (error "Format: , describe-literally TERM [FROM] [TO]"))
  (let ((fs-internal-describe-literally-p t)
	(fir (first rest))
	(res (rest rest)))
    (cond
     (fir
      (apply 'fs-describe
	     (if (stringp fir) (regexp-quote fir)
	       (regexp-quote (format "%s" fir)))
	     res))
     (t (apply 'fs-describe rest)))))


(defvar erbnocmd-describe-search-p t)

(defun fsi-describe (&optional mainterm N M prestring expr &rest rest)
  "The general syntax is (fs-describe TERM [N] [M]).
Looks for TERM, and shows its descriptions starting at description
number N, and ending at M-1. The first record is numbered 0.

Update: This function may NOT work if called directly via lisp AND if you supply more than 3 arguments."
  (let
      ;;((fs-lispargs (append (list mainterm N M prestring expr) rest)))
      ;; nothing, just a let not used any more..
      ((fs-nothingsorry nil))
    ;; in the global scheme of things, this will turn out to be only a
    ;; local binding, since erbeng-main will have (let)'ed this.  Same
    ;; for fs-lispa , fs-lispb, fs-lispc...

    (setq fs-lispargs (mapcar 'fsi-read-or-orig (cdr fs-msglistsansbot)))
    ;; This was the pre-2015 method of dealing with it.
    ;; (when fs-found-query-p
    ;;    (setq N 0)
    ;;    (setq M 1))
    (when fs-found-query-p (setf fsi-limit-lines 2)) ;; This is ONLY set temporarily, because erbn-reply has made this into a LET.
    (unless prestring (setq prestring ""))
    (unless mainterm
      (error
       "Format , (describe TERM &optional number1 number2)"))
    (let* ((bar (cons mainterm (cons N rest)))
	   (foo (format "%s" mainterm))
	   (barbar
	    (append
	     (and mainterm (list mainterm))
	     (and N (list N))
	     (and M (list M))
	     rest))
	   )
      (setq foo (fs-correct-entry foo))
      (if (stringp N)
	  (setq N (erbn-read N)))
      (unless (integerp N)
	(setq N 0))
      (if (stringp M)
	  (setq M (erbn-read M)))
      (if (and (integerp M) (= M N))
	  (setq M (+ N 1)))
      (unless (stringp foo)
	(setq foo (format "%s" foo)))
      (let* ((result0
	      (erbbdb-get-exact-notes
	       foo
	       ))
	     (result1 (and (stringp result0)
			   (ignore-errors (erbn-read result0))))
	     (len (length result1))
	     (newM (if (and (integerp M)
			    (< M len))
		       M len))
	     (result (subseq result1 N newM))
	     (shortenedp (or (< newM len)
			     (> N 0)))
	     )

	(ignore-errors
	  (when (eq 1 (length result))
	    ;;(message "result: %d %S" (length result) result)
	    (when (string-match 
		   "\\<\\(http://\\(?:www\\.\\)emacswiki.org/\\S-+\\)" 
		   (car result))
	      ;;(message "result: wiki regexp matched [%s]" (car result))
	      (setq result (list (car result)
				 (summarise-emacswiki 
				  (match-string 1 (car result))) )) )))

	(cond
	 ;; in cond0
	 (result1
	  (let* (
		 ;; notice the use of result1 here, not result.
		 (aa (first result1))
		 (aarest (cdr result1))
		 (bb (split-string aa))

		 (cc (first bb))
		 (dd (second bb))
		 (ddd (or (and (stringp dd) (regexp-quote dd)) ""))
		 (ee (cdr bb))
		 (expandp
		  (and
		   (not fs-internal-describe-literally-p)

		   ;;(equal len 1)
		   ))

		 )

	    (if (and
		 (equal cc "directonly")
		 ;;(equal len 1)
		 )
		;; hmm this if part still doesn't take care of aa..
		(if fs-found-query-p
		    (progn
		      (setq aa "lisp 'noreply")
		      (setq bb (split-string aa))
		      (setq cc (first bb))
		      (setq dd (second bb))
		      (setq dd (or (and (stringp dd) (regexp-quote dd)) ""))
		      (setq ee (cdr bb)))
		  (when expandp
		    (progn
		      (setq bb (cdr bb))
		      (setq aa (mapconcat 'identity bb " "))
		      (setq result1 (cons aa aarest))
		      (setq result (subseq result1 N newM))
		      (setq cc (first bb))
		      (setq dd (second bb))
		      (setq ddd (or (and (stringp dd)
					 (regexp-quote dd)) ""))
		      (setq ee (cdr bb))))



		  ))
	    (cond
	     ((and expandp
		   (erbutils-string= cc "redirect")
		   ;; do not redirect, when term had multiple
		   ;; entries: 
		   (not aarest)
		   dd)
	      (progn (sleep-for 0)
		     (apply 'fs-describe (fsi-chase-redirects ddd)
			    N M
			    (format "[->] "
				    )
			    rest)))
	     ((and expandp (member cc '("unecho" "noecho"))
		   dd)
	      ;;dd)
	      (erbutils-itemize
	       (cons
		(format "%s"
			(mapconcat 'identity ee " "))
		(cdr result))
	       N
	       shortenedp
	       ))
	     ((and expandp (member cc '("lisp")))
	      (let*
		  ((fs-nothingsorry nil))
		   ;; (fs-lispargs fs-lispargs) ;; no need
		(setq fs-lispa (nth 0 fs-lispargs))
		(setq fs-lispb (nth 1 fs-lispargs))
		(setq fs-lispc (nth 2 fs-lispargs))
		(setq fs-lispd (nth 3 fs-lispargs))
		(setq fs-lispe (nth 4 fs-lispargs))
		(erbeng-main
		 (concat erbn-char " (progn "
			 (substring aa
				    (with-temp-buffer
				      (insert aa)
				      (goto-char (point-min))
				      (search-forward "lisp" nil t)))
			 " )")

		 erbeng-proc
		 erbeng-nick erbeng-tgt erbeng-localp
		 erbeng-userinfo)))


	     (t
	      (erbutils-add-nick-maybe
	       (concat
		prestring
		(format
		 (erbutils-random
		  '(
		    ;;"%s is "
		    "%s is, like, "
		    "I heard %s is "
		    "I think %s is "
		    ;;"/me thinks %s is "
		    "%s -- "
		    ;;"%s is "
		    "%s is "
		    "hmm, %s is "
		    "From memory, %s is "
		    ))
		 ;; 2004-01-27 T17:21:55-0500 (Tuesday)    D. Goel
		 ;; why regexp-quote here??  changing it..
		 ;;(regexp-quote foo)
		 foo
		 )
		;; and notice the use of result here..
		(if result
		    (erbutils-itemize result N shortenedp)
		  (erbutils-itemize result1 0))
		)))



	     )))

	 ;; in cond0
	 ;; else
	 (fs-found-query-p
	  'noreply)
	 ((not erbnocmd-describe-search-p)
	  ;; most likely: redirected but the redirected stuff does not exist..
	  (format
	   "Missing redirect. %s is now on fire.               (Try , dl) "
	   erbot-nick mainterm))
	 (t
	  ;; prevent any further expansions on further loopbacks.
	  (let ((erbnocmd-describe-search-p nil))
	    (fs-search
	     mainterm nil nil
	     (concat prestring "try: ")
	     ;;barbar
	     expr
	     ))))))))

(defvar fs-internal-doctor-rarity 80
  "A large number(1--100) means rarer doctor inovcation upon no matches."
  )


(defun fsi-suggest-describe (&rest terms)
  "Fallback for when `fs-describe' fails.
It then (often) calls this function, which suggests
alternatives.
Optional argument TERMS ."
  (let ((term (format "%s" (first terms)))
	(none (erbutils-random
	       '("No such term.."
		 "Beeeeep..."
		 "<LOUD ERROR MSG >.."
		 "No luck.."
		 "No match.."
		 "Drew a blank.."
		 "Does not compute..")))
	(num (random 100)))
    (cond
     ((< num 30)
      (concat none
	      (format "Also try:  , s %s or , sw %s  or , %s 0" term
		      term term)))
     ((< num 60)
      (concat none
	      (format "Try search or search-wide on %s" term)))
     (t
      (concat none
	      (erbutils-random '("perhaps " "why not " "please " ))
	      "tell me what " term " is?")))))


(defun fs-do-random (&optional msg nick &rest ignored)
  "Either play doctor, or zippy or flame someone.. all at random..."
  (case (random 4)
    (0 (fs-doctor msg))
    (1 (fs-flame nick))
    (2 (fs-yow))
    (3 (fs-fortune))
    )
    ;;(3 (fs-bottalk))
    )

(defcustom fsi-internal-english-weights
  '(10 ;; doc
    20 ;; yow
    30 ;; fortune
    5;; rms
    20 ;; flame
    10 ;; spook
    10 ;; pray
    2 ;; geek-code
    )
  ""
  :group 'erbc)

(defun fsi-do-weighted-random (&optional msg nick &rest ignored)
  "Either play doctor, or zippy or flame someone.. all at random..."
  (let ((foo (random 100)))
    (eval
     (erbutils-random
      `((fsi-doctor ,msg)
	(fsi-yow ,msg)
	(fsi-fortune ,msg)
	(fsi-fact)
	(fsi-flame ,nick)
	(fsi-spook)
	(fsi-pray)
	(fsi-geek-code)
	)
      fsi-internal-english-weights))))




(defun fsi-yow (&rest args)
  ""
  (erbutils-eval-until-limited
   '(yow)))

(defun fsi-rearrange (&optional from to term &rest dummy)
  "Syntax: FROM->TO in TERM.
Move the FROMth entry to the TOth position in the given TERM.
Numbering of positions starts from 0. "
  (unless term (error "Syntax: , N->M in TERM (no term found)"))
  (when (stringp from)
    (setq from (erbn-read from)))
  (when (stringp to)
    (setq to (erbn-read to)))
  (unless (stringp term)
    (setq term (format "%s" term)))
  (let*
      ((exactnotes (erbbdb-get-exact-notes term))
       (realterm (erbbdb-get-exact-name term))
       (notes (and (stringp exactnotes ) (erbn-read exactnotes)))
       (len (length notes))
       (max (- len 1))
       (newnotes notes)
       remlist
       thisnote
       (tostring (downcase (format "%s" to)))
       )
    (unless realterm
      (error "No such term exists %S" term))
    (unless notes
      (error "Report this bug.  Term exists but no notes?? %S" term))
    (when (string= tostring "last")
      (setq to max))
    (when (string= tostring "first")
      (setq to 0))
    (unless (and (integerp from)
		 (<= from  max) (>= from 0))
      (error "The from term %S should lie between %S and %S"
	     from 0 max))
    (setq thisnote (nth from notes))
    (setq remlist
	  (append (subseq notes 0 from)
		  (subseq notes (+ from 1))))
    (setq newnotes
	  (append (subseq remlist 0 to)
		  (list thisnote)
		  (subseq remlist to)))
    (erbot-working
     (fs-forget term "all")
     (fs-set-term realterm newnotes))
    (erbbdb-save)
    (format "Moved entry %S to %S in %S" from to realterm)
    ))

;;; 2002-09-04 T01:51:08-0400 (Wednesday)    D. Goel
(defun fsi-forget (&optional name number &rest dummy)
  "Remove the entry correponding to NAME in the database.
With NUMBER, forget only the NUMBERth entry of NAME. "

  ;; before we do the usual thing, let's see if we need to and can get
  ;; away with exchanging name and number.
  (when
      (and
       (numberp name)
       (not (numberp number)))
    (let ((fstmp number))
      (setq number name)
      (setq name fstmp)))
  (unless (stringp name)
    (setq name (format "%s" name)))
  (progn
    (unless name
      (error "Syntax: , forget TERM [<N>/ALL/LAST]"))
    (setq name (fs-correct-entry name))
    (let*
	(numstring
	 (entries0 (erbbdb-get-exact-notes name))
	 (entries (if entries0
		      (or (and (stringp entries0)
			       (ignore-errors (erbn-read entries0))) "")))
	 (len (length entries)))
      ;;(message "extracted %S/%S for %S" entries0 entries name)
      (unless entries
	(error "No such term %s" name))
      (when (and (null number) (= len 1))
	(setq number 0))
      (setq numstring (downcase (format "%s" number)))
      (when (stringp number)
	(setq number (erbn-read number)))
      (unless (integerp number) (setq number nil))
      (unless
	  (or number
	      (member numstring (list "all" "last")))
	(error "Syntax: , forget TERM [NUMBER]/all/last"))
      (when number
	(unless (and (< number len) (>= number 0))
	  (error "Number should be \"all\" or lie between 0 and %s"
		 (- len 1))))
      ;; Note that this does remove the field but throws a strange error..
      ;; "Record doubleplus inpresent...  It is just us who are discarding
      ;; this error.. ...
      ;; yet the field gets deleted..  and bbdb does not get saved at this
      ;; time..  because of the error ...OH well, it works.. let's move on
      ;; for now..
      (when (string= numstring "last") (setq number (- len 1)))
      (cond
       (
	(and (equal number 0)
	     (= len 1))
	(ignore-errors (erbbdb-remove name))
	(erbbdb-save)
	(format "Forgot %S which had exactly one entry." name))
       ((string= numstring "all")
	(ignore-errors (erbbdb-remove name))
	(erbbdb-save)
	(if (= len 1) (format "Forgot the single entry in %S" name)
	  (format "Forgot all %s entries of %S" len name)))
       (t
	(fsi-forget name "all")
	(fsi-set-term
	 name
	 (append
	  (subseq entries 0 number)
	  (subseq entries (+ number 1))))
	(message "Removed entry %s of %s" number name))))))
  
  
  
(defvar fs-set-add-all-p nil
  "")

(make-variable-buffer-local 'fs-set-add-all-p)


(defun fsi-set-add-all-enable ()
  (setq fs-set-add-all-p t))
(defun fsi-set-add-all-disable ()
  (setq fs-set-add-all-p nil))

(defun fsi-set-add-all-toggle ()
  "Enable the \"is\" command to always work.
viz.  Add field even if another field is already present. This is not the
recommended usage in general, except when using automated scripts to
train the bot.  The default is nil, which suggests the user to use
\"is also\" instead. "

  (setq fs-set-add-all-p (not fs-set-add-all-p))
  (format
   "All-is mode set to %S.  To toggle, type , (fs-set-add-all-toggle)"
   fs-set-add-all-p))

(defun fsi-set-term (&rest args)
  "Add an entry to database.
An entry gleaned from (first ARGS) is
added.  (second ARGS) is the description.  The entry is converted to
lowercase, and all whitespace is converted to colons.
To set a term's notes to notes via lisp, use fsi-set-notes."
  (let ((name (fs-correct-entry (format "%s" (first args))))
	(records (cadr args)))
    (unless (listp records) (setq records (list records)))
    (setq records (mapcar
		   '(lambda (arg) (format "%s" arg))
		   records))
    (let ((current
	   (erbbdb-get-exact-notes name)))
      (cond
       ((null records)
	(error "Please specify a description for %s.. Type , df fs-set-term for more details" name))

       ((and current (string= current ""))
	(progn (erbbdb-create name records)
	       (format "Added field to the currently empty %s " name)))
       (current
	(if fs-set-add-all-p
	    (apply 'fs-set-also args)
	  (error
	   "%s is already something else.. Use 'is also'.. \n Currently: %s" name

	   (let* ((notes (fs-notes name))
		  (shortenedp (> (length notes) 1)))
	     (erbutils-itemize
	      (list (first notes))
	      0 shortenedp))

	   )))
       (t
	(progn (erbbdb-create name records)
	       (format "Created new entry for %S" name)
		       ))))))


(defun fsi-set-notes (term notes)
  "Set term's notes to notes."
  (let ((name (fs-correct-entry (format "%s" term)))
	(records notes))
    (assert (listp records))
    (mapc
     '(lambda (arg) (assert (stringp arg)))
     records)
    (let ((current
	   (erbbdb-get-exact-notes name)))
      (assert (null current))
      (assert (not (null records)))
      (progn (erbbdb-create name records)))
    (format "Set new notes for term %s " name)))



(defun fsi-chase-redirects (name &optional prevlist)
  "either return nil or the redirected entry (if any)"
  (let* ((notes (fs-notes name))
	 (fir (first notes))
	 (ans name) ;; answer
	 (foundp nil)
	 )
    (when (and (stringp fir)
	       ;; do not chase redirects if a term has a second
	       ;; entry...
	       ;; In that case, the first entry should not have been a
	       ;; redirect in any case. 
	       (= (length notes) 1)
	       (equal 0 (string-match "redirect\\b" fir)))
      (let* ((foo (split-string fir))
	     (sec (second foo)))
	(when (stringp sec) 
	  (when (member sec prevlist)
	    (error "Loop found in chase-redirects. Please use ,dl and remove redirect-loops. I'm now on fire. HELP!"))
	  (setf ans sec foundp t
		prevlist (cons ans prevlist))
	  (sit-for 0)
	  (setq ans 
		(fsi-chase-redirects ans prevlist)))))
    ans))
	 



(defun fsi-set-also (&rest args)
  "Add more fields to the the database-entry gleaned from (first ARGS).
\(second ARGS) contains the new descriptions.
Record should be a single entity here... a string..."
  (let* ((name (fs-correct-entry (format "%s" (first args))))
	 (record (format "%s" (second args)))
	 notes
	 ;;(notes (fs-notes name)))
	 )
    (setq name (or (fs-chase-redirects name) name))
    (setq notes (fs-notes name))
    (unless notes (error "But there's no such record: %s" name))
    (cond
     ((member-ignore-case record notes)
      (format "Not added. A similar entry already exists in the term %S" name))
     (t
      (erbbdb-add name record)
      ;;(run-hook-with-args 'erbot-notify-add-functions nick channel
      ;;name (length notes)
      (format "Added entry to the term %S" name)))))


(defun fsi-doctor (&rest foo)
  ""
  (erbutils-add-nick
   (funcall 'erbot-doctor
	    (erbutils-stringify foo))))


(defun fsi-dunnet-command (&rest foo)
  ;;(let ((fsi-limit-lines 8))
  ;;(fsi-limit-lines
  ;;(let ((dun-batch-mode t))
  (funcall 'erbot-dunnet
	   (erbutils-stringify foo)))


(defun fsi-info-search (&rest foo)
  "info-search.. Coming soon...will tell the number of matches
in manuals of HURD, tramp, eshell, elisp, gnus, message, emacs, ebrowse, calc,
gdb, make sawfish, cl-emacs, bash, gnuplot, latex and others by demand...")

;; NO! else fsbot responds to <nick> fsbot is cool! in a wrong way.
;; (defalias 'fs-is 'erbutils-info-search)

(defun fs-hurd-info-search (&rest foo)
  "Coming soon...")
(defalias 'fs-his 'erbutils-hurd-info-search)

(defun fsi-blue-moon (&rest foo)
  "Return true in a really rare case. Currently 1 in 100,000.. was 1 in
2000. "
  (= (random 100000) 0))


(defun fsi-set-force (&rest args)
  "Forget an entry and add new fields to it..
Syntax: , no foo is bar."
  (progn
    (let* ((fir (first args))
	   (aa (erbbdb-get-exact-notes fir))
	   (notes (and (stringp aa) (erbn-read aa)))
	   (len (length notes)))
      (when (= len 0)
	(error "There's no such term %s.  Use , %s is ..." fir fir))
      (unless (= len 1)
	(error
	 "Term has multiple entries. Examine them and ask me to forget them first"))
      (erbutils-ignore-errors (funcall 'fs-forget (first args) "all"))
      (apply 'fs-set-term args))))


(defcustom erbn-fortune-p t
  "This is true by default.. since (shell-command \"fortune\") is not
risky.. ")


(defun erbn-fortune (arg)
  (unless arg (setq arg ""))
  (cond
   ((string= arg "")
    (erbutils-eval-until-limited
     '(erbn-shell-command-to-string (concat "fortune -s " arg)
				      (list erbn-fortune-p)
				      )))
   (t
    (erbn-shell-command-to-string (concat "fortune -s " arg)
				    (list erbn-fortune-p)
				    ))))


(defun fsi-fortune (&rest args)
  (erbn-fortune "fortunes"))


(defalias 'fs-f 'fs-fortune)

(defun fs-fortunes-help (&rest args)
  (concat "Type ,fortune, or any of the commands beginning with f- : "
	  (fs-commands "^f-")))

(defalias 'fs-fortune-help 'fs-fortunes-help)
(defalias 'fs-f-help 'fs-fortunes-help)


(defun fs-f-f (&rest args)
  (erbn-fortune "-f"))

(defun fs-f-off (&rest args)
  (erbn-fortune "-o"))
(defalias 'fs-f-o 'fs-f-off)
(defalias 'fs-f-offensive 'fs-f-off)


(defun fs-f-debian-hints (&rest args)
  (erbn-fortune "debian-hints"))
(defalias 'fs-debian-hints 'fs-f-debian-hints)



(defun fs-f-twisted-quotes (&rest args)
  (erbn-fortune "twisted-quotes"))
(defalias 'fs-quotes 'fs-f-twisted-quotes)
(defalias 'fs-f-quotes 'fs-f-twisted-quotes)

(defun fs-f-literature (&rest args)
  (erbn-fortune "literature"))
(defalias 'fs-f-lit 'fs-f-literature)
(defalias 'fs-lit 'fs-f-literature)
(defalias 'fs-literature 'fs-f-literature)



(defun fs-f-riddles(&rest args)
  (erbn-fortune "riddles"))
(defalias 'fs-riddle 'fs-f-riddles)



(defun fs-f-art (&rest args)
  (erbn-fortune "art"))
(defalias 'fs-art 'fs-f-art)




(defun fs-f-bofh-excuses (&rest args)
  (erbn-fortune "bofh-excuses"))
(defalias 'fs-bofh 'fs-f-bofh-excuses)




(defun fs-f-ascii-art (&rest args)
  (erbn-fortune "ascii-art"))
(defalias 'fs-ascii 'fs-f-ascii-art)




(defun fs-f-computers (&rest args)
  (erbn-fortune "computers"))

(defalias 'fs-f-computer 'fs-f-computers)





(defun fs-f-cookies (&rest args)
  (erbn-fortune "cookies"))

(defalias 'fs-f-cookie 'fs-f-cookies)
(defalias 'fs-cookie 'fs-f-cookies)





(defalias 'fs-f-cookie 'fs-f-cookies)
(defalias 'fs-cookie 'fs-f-cookies)


(defun fs-f-definitions (&rest args)
  (erbn-fortune "definitions"))

(defalias 'fs-def 'fs-f-defintions)




(defun fs-f-drugs (&rest args)
  (erbn-fortune "drugs"))
(defalias 'fs-drugs 'fs-f-drugs)
(defalias 'fs-drug 'fs-f-drugs)




(defun fs-f-education (&rest args)
  (erbn-fortune "education"))


(defun fs-f-ethnic (&rest args)
  (erbn-fortune "ethnic"))




(defun fs-f-food (&rest args)
  (erbn-fortune "food"))
(defalias 'fs-food 'fs-f-food)






(defun fs-f-goedel (&rest args)
  (erbn-fortune "goedel"))
(defalias 'fs-goedel 'fs-f-goedel)




(defun fs-f-humorists (&rest args)
  (erbn-fortune "humorists"))


(defun fs-f-kids (&rest args)
  (erbn-fortune "kids"))


(defun fs-f-law (&rest args)
  (erbn-fortune "law"))

(defalias 'fs-law 'fs-f-law)



(defun fs-f-linuxcookie (&rest args)
  (erbn-fortune "linuxcookie"))


(defun fs-f-love (&rest args)
  (erbn-fortune "love"))

(defun fs-f-magic (&rest args)
  (erbn-fortune "magic"))



(defun fs-f-medicine(&rest args)
  (erbn-fortune "medicine"))



(defun fs-f-men-women (&rest args)
  (erbn-fortune "men-women"))

(defalias 'fs-sexwar 'fs-f-men-women)





(defun fs-f-miscellaneous(&rest args)
  (erbn-fortune "miscellaneous"))

(defalias 'fs-f-misc 'fs-f-miscellaneous)



(defun fs-f-news (&rest args)
  (erbn-fortune "news"))



(defun fs-f-people (&rest args)
  (erbn-fortune "people"))


(defun fs-f-pets (&rest args)
  (erbn-fortune "pets"))



(defun fs-f-platitudes (&rest args)
  (erbn-fortune "platitudes"))



(defun fs-f-politics (&rest args)
  (erbn-fortune "politics"))


(defun fs-f-science (&rest args)
  (erbn-fortune "science"))

(defun fs-f-songs-poems (&rest args)
  (erbn-fortune "songs-poems"))


(defun fs-f-sports(&rest args)
  (erbn-fortune "sports"))





(defun fs-f-startrek (&rest args)
  (erbn-fortune "startrek"))
(defalias 'fs-startrek 'fs-f-startrek)





(defun fs-f-translate-me (&rest args)
  (erbn-fortune "translate-me"))



(defun fs-f-wisdom(&rest args)
  (erbn-fortune "wisdom"))
(defalias 'fs-wisdom 'fs-f-wisdom)



(defun fs-f-work (&rest args)
  (erbn-fortune "work"))



(defun fs-f-linux (&rest args)
  (erbn-fortune "linux"))

(defun fs-f-perl (&rest args)
  (erbn-fortune "perl"))

(defun fs-f-knghtbrd (&rest args)
  (erbn-fortune "knghtbrd"))




(defun fs-f-quotes-emacs-channel (&rest args)
  (erbn-fortune "~/fortune-emacschannelquotes"))
(defalias 'fs-f-emacs 'fs-f-quotes-emacs-channel)
(defalias 'fs-f-quotes-emacs 'fs-f-quotes-emacs-channel)
(defalias 'fs-quotes-emacs 'fs-f-quotes-emacs-channel)
(defalias 'fs-quotes-emacs-channel 'fs-f-quotes-emacs-channel)









;; (defalias 'fs-cons 'cons)

(defvar fsi-internal-limit-line-length 125
  "Suggested value: (multiple of 80) minus 35 .. suggested: 210.")

(defvar fsi-internal-limit-length
  300
 "A multiple of fs-internal-fill-column.. we suggest: double of it..  note
that the actual limited-length will be more than this number---it may
be upto double of this number depending on how the formatting is done.
viz: we shall go to the line containing this point, and include the
entire line.
")

(defvar fsi-limit-lines 8 "")
(setf fsi-limit-lines 8)

(defvar fs-dunnet-mode nil
  "")

(make-variable-buffer-local 'fs-dunnet-mode)

(defvar fs-internal-fill-column 350
  "Default is to disable filling.  The receipient should be able to
fill the way they like.
should be <= fsi-internal-limit-length, else we might set it to be during the
code.
also, a good idea to keep it < erc's builtin flood protection length,
else your lines will get broken during middle of words by ERC.
Thus, keep it below, say 350."
)






(defun fsi-limit-string (&optional str maxlen &rest ignored)
  "Fills the string and then then limits lines"
  (fsi-limit-lines (fs-fill-string str)))


(defun fsi-fill-string (str)
  (with-temp-buffer
    (insert str)
    (let ((fill-column fs-internal-fill-column))
      (text-mode)
      (fill-region (point-min) (point-max))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defalias 'fsi-fill 'fsi-fill-string)

(defun fsi-unfill-string (str)
  (let ((fs-internal-fill-column 9999))
    (fsi-fill-string str)))

(defalias 'fsi-unfill 'fsi-unfill-string)

(defun fsi-limit-string-old (&optional str maxlen &rest ignored)
  (cond
   (str
    (unless (stringp str)
      (setq str (format "%s" str)))
    ;; get rid of all the \n first..
    (setq str
	  (mapconcat 'identity
		     (split-string str "\n")
		     "  "))
    (when (> (length str) fsi-internal-limit-length)
      (setq str (concat (substring str 0 (- fsi-internal-limit-length 7))
			"..<more>")))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (let ((fill-column fs-internal-fill-column))
	(fill-paragraph nil))
      (erbutils-buffer-string)))
   (t "\n")))

(defun fsi-dunnet-mode ()
    (let ((target (not fs-dunnet-mode)))
      (when target
	nil  ;; we could choose to kill the dunnet buffer here, but that leads to weird errors 201406.
	)
      (setf fs-dunnet-mode target)
      (format "Dunnet mode set to %S.  To toggle, type , (dunnet-mode)"
	      fs-dunnet-mode)))

(defun fsi-limit-string-no-fill (&optional str limit-lines
				      limit-length
				      limit-line-length
				      &rest ignored
				      )
  "IS OLD. i think.  not used anywwhere...  certainly screws up more:
is not compliant with fsbot paginator.

Limit string to reasonable length..
Not more than fsi-internal-limit-line-length characters per line, and
not more than fsi-internal-limit-length characters in all.. and not more
than fsi-limit-lines in all.."
  (if str
      (let ((fsi-limit-lines
	     (or limit-lines fsi-limit-lines))
	    (fsi-internal-limit-length
	     (or limit-length
		 fsi-internal-limit-length))
	    (fs-limit-line-length
	     (or limit-line-length
		 fsi-internal-limit-line-length)))
	(fsi-limit-lines
	 (fsi-internal-limit-length
	  (fs-limit-line-length
	   str t))))
    "\n"))


(defvar erbn-more nil
  "Alist of pending more-strings per target.  Each target is a
string. ")
;;(make-variable-buffer-local 'fs-more)

(defun erbn-more-get (&optional target)
  "When target is nil, we get the latest more that occurred in ANY
channel, else we get the more from the channel indicated by target. "
  (setq target (format "%S" target))
  (let ((str (cdr (assoc target erbn-more))))
    (if (and (stringp str)
	     (not (string= str "")))
	str
      (fs-describe "more"))))

(defalias 'fsi-more-get 'erbn-more-get)

(defun erbn-more-set (str &optional target)
  (setq target (format "%S" target))
  (if (assoc target erbn-more)
      (setf (cdr (assoc target erbn-more)) str)
    (add-to-list 'erbn-more (cons target str)))
  (if (assoc "nil" erbn-more)
      (setf (cdr (assoc "nil" erbn-more)) str)
    (add-to-list 'erbn-more (cons "nil" str)))
  erbn-more)


(defun fsi-more-set (&optional str)
  (unless str (error "Need a string. "))
  (erbn-more-set str erbn-tgt))



(defun fsi-limit-lines (str0 &optional nomorep &rest ignored)
  "Limits the string, both, to a reasonable number of lines and a
reasonable number of characters, trying not to break lines and not to
break words, if possible.

Thus, that becomes quite a complicated algorithm, and we do that
here."
  (let* (ans
	 (ender "")
	 (more "")
	 (stra (erbutils-remove-text-properties str0))
	 (str (mapconcat 'identity
			 (remove "" (split-string stra "\n"))
			 "\n"))
	 (limitedp nil)
	 ptmx
	 this-line
	 this-point
	 new-point
	 )
    (with-temp-buffer
      ;; fledermaus: ensure that the buffer's byteness  matches the str's.
      (set-buffer-multibyte (multibyte-string-p str))
      (insert str)
      (setq ptmx (point-max))
      (setq this-point ptmx new-point ptmx)
      (if (> fsi-internal-limit-length ptmx)
	  (goto-char ptmx)
	(setq limitedp t)
	(goto-char fsi-internal-limit-length))
      ;;(goto-char (point-max))
      ;;(remove-text-properties (point-min) (point-max))
      (setq this-line (count-lines (point-min) (point)))
      (when (> this-line fsi-limit-lines)
	(setq limitedp t)
	(goto-line fsi-limit-lines)
	(setq this-line fsi-limit-lines)
	)

      (setq this-point (point) new-point this-point)

      (cond
       ((and limitedp (> this-line 1))
	(progn (beginning-of-line)
	       (setq new-point (point))
	       (backward-char) (setq this-point (point))
	       ))
       ((and limitedp
	     (progn (ignore-errors
		      ;; we want a backward-word 1 here, but only
		      ;; whitespace is regarded as word-boundary for
		      ;; us.
		      (when
			  (search-backward-regexp "\\( \\|\n\\|\t\\)" nil t)
			(forward-char 1))
		      ;;(backward-word 1)
		      )
		    (> (point) (point-min))))
	(setq new-point (point))
	(setq this-point new-point))


       ;;(limitedp (setq this-point (point) new-point (point)))

       ;; in the final case, this-point and new-point are already at
       ;;point-max...
       (t nil))
      (setq ans (buffer-substring (point-min) this-point))
      (when
	  ;;(< this-point (point-max))
	  limitedp
	(setq more (buffer-substring new-point (point-max)))
	(if
	    (string-match "[^ \t\n]" more )
	    (setq ans (concat ans (fsi-get-more-invocation-string))) 
	  (when nomorep (setq more "")))
	)
      )
    ;;(setq fs-more more)
    (erbn-more-set more erbn-tgt)
    ans))

(defun fsi-get-more-invocation-string ()
  (if (erbot-safe-nocontrol-p erbn-char)
      (concat " ..[Type " erbn-char "more]")
    (concat " ..[Type " erbot-nick ": more]")))

(defun fsi-limit-lines-old (str0 &rest ignored)
  ""
  (let* (
	 (str (erbutils-remove-text-properties str0))
	 (brstr1 (split-string str "\n"))
	 (brstr (remove "" brstr1))
	 (ender "")
	 (condp (> (length brstr) fsi-limit-lines))
	 (goodstr
	  (if condp
	      (progn
		(setq ender "..+ more")
		(subseq brstr 0 (- fsi-limit-lines 1)))
	    brstr)))
    (if condp (fs-more-set
		      (mapconcat 'identity
				 (subseq brstr (- fsi-limit-lines
						  1))
				 "\n"))
      (fs-more-set ""))
    (concat (mapconcat 'identity goodstr "\n") ender)))

(defun fsi-more (&rest args)
  "Display the contents of the cache. "
  (let ((str (fsi-more-get erbn-tgt)))
    (if (and (stringp str)
	     (not (string= str "")))
	str
      (fs-describe "more"))))

;;   (if (and (stringp fs-more)
;; 	   (not (string= fs-more "")))
;;       fs-more
;;     (fs-describe "more")))


(defun fsi-limit-lines-long (str &rest ignored)
  ""
  (let ((fsi-limit-lines 7))
    (apply 'fsi-limit-lines str ignored)))



(defun fsi-limit-length (str &rest ignored)
  "Don't use this, use fsi-limit-lines"
  (if (> (length str) fsi-internal-limit-length)
      (concat (substring str 0 (- fsi-internal-limit-length 1)) "...<more>")
    str))

(defun fsi-limit-line-length (&optional str &rest args)
  "a subfunction.."
 (let* (
	;; this not needed now..
	(brokenstr (split-string str "\n"))
	(newlsstr
	 (mapcar
	  '(lambda (givenstr)
	     (let ((ls nil)
		   (thisstr givenstr)
		   )
	       (while (> (length thisstr)
			 fsi-internal-limit-line-length)
		 (push
		  (concat (substring thisstr 0 fsi-internal-limit-line-length
						  ) " <break>")
		  ls)
		 (setq thisstr (substring thisstr fsi-internal-limit-line-length
					  (length thisstr))))
	       (push thisstr ls)
	       (reverse ls)))
	  brokenstr))
	(newbrokenstr
	 (apply 'append newlsstr)))
   (mapconcat 'identity newbrokenstr "\n")))


(defvar fs-internal-directed nil)

(defun fsi-tell-to (string nick &rest ignored)
  (setq fs-nick (format "%s" nick))
  (let* ((fs-internal-directed t)
	 (ni (if (string= (format "%s" nick) "me")
		erbot-end-user-nick
	      (format "%s" nick)))
	 (reply
	  (erbeng-get-reply (fs-parse (concat erbot-nick ": "
						  string)))))
    (if (string-match ni reply)
	reply
      (concat ni ": " reply))))


(defun fsi-apropos (&optional regexp N M &rest ignored)
  (fs-apropos-basic 'erbn-apropos regexp N M))
(defun fsi-apropos-command (&optional regexp n m &rest ignored)
  (fs-apropos-basic 'erbn-apropos-command regexp n m ))
(defun fsi-apropos-variable (&optional regexp n m &rest ignored)
  (fs-apropos-basic 'erbn-apropos-variable regexp n m ))
(defun fsi-apropos-function (&optional regexp n m &rest ignored)
  (fs-apropos-basic 'erbn-apropos-function regexp n m ))
(defun fsi-apropos-value (&optional regexp n m &rest ignored)
  ;;(fs-apropos-basic 'apropos-value regexp n m )
  "This function has been disabled as it is too resource-intensive.")

(defun fsi-apropos-documentation (&optional regexp n m &rest ignored)
  (fs-apropos-basic 'erbn-apropos-documentation  regexp n m ))

(defun erbn-apropos-documentation (reg)
  (mapcar 'car (apropos-documentation reg)))
(defun erbn-apropos-command (reg)
  (apropos-internal reg
		    'commandp))



(defun erbn-apropos-function (reg)
  (apropos-internal reg
		    'functionp))

(defun erbn-apropos-variable (reg)
  (apropos-internal reg
		    (lambda (s)
		      (or (boundp s)
			  (user-variable-p s)))))


(defun erbn-apropos (regexp)
  (apropos-internal regexp
		    (lambda (symbol)
		      (or
		       (boundp symbol)
		       (fboundp symbol)
		       (facep symbol)
		       (symbol-plist symbol)))))

(defun fsi-apropos-basic (fcn &optional regexp N M &rest ignored)
  "Show the apropos-matches  of regexp starting at match number N"
  (unless regexp
    (error "Syntax: , apropos REGEXP &optional N M"))
  (if (stringp N) (setq N (erbn-read N)))
  (unless (integerp N) (setq N 0))
  (unless (stringp regexp)
    (setq regexp (format "%s" regexp)))
  (let* ((results (funcall fcn regexp))
	 (len (length results))
	 (str0 "")
	 (str1 "")
	 (str2 "")
	 (str3 "")
	 (str4 ""))
    (unless (and (integerp M) (< M len))
      (setq M len))
    (if (and (= N  0 ) (= M len) (> len 30))
	(setq
	 str0
	 "Note: Try , df fs-apropos for general syntax.  "))
    (if (> len 1) (setq str1 (format "%s matches.  " len)))
    (if (> N 0) (setq str2 (format "Matches starting at %s->" N)))
    (setq str3 (progn (format "%s"
					  (subseq results
						  N M)
					  )))
    (concat str0 str1 str2 str3 str4)))


(defun fsi-find-variable (function &rest ignore)
  (fs-find-variable-internal function  'nolimit))

(defun fsi-find-variable-internal (function &optional nolimitp &rest ignore)
  "Finds the variable named FUNCTION."
  (if (stringp function) (setq function (erbn-read function)))
  (cond
   ((symbolp function)
    (unless (boundp function)
      (let ((g (intern (concat "fs-" (format "%s" function)))))
	(if (boundp g)
	    (setq function g))))
    (let ((fstr
	   (save-excursion
	     (find-function-do-it function t 'set-buffer)
	     (buffer-substring (point)
			       (save-excursion
				 (forward-sexp 1)
				 (point))))))
      (if (equal nolimitp 'nolimit)
	  fstr
	fstr)))
   (t "\n")))

(defalias 'fsi-find-variable-briefly 'fs-find-variable)


(defun fsi-find-function-verbatim (&optional function &rest ignore)
  (unless function
    (error "Syntax: , find-function-verbatim 'function-name"))
  ;;fsi-limit-lines-long
  (fs-find-function-internal
   function 'nolimit))


(defun fsi-find-function (&optional function &rest ignore)
  (unless function
    (error "Syntax: , find-function-verbatim 'function-name"))
  ;;fsi-limit-lines-long
  (fsi-find-function-internal
   function 'nolimit 'cleanup))



(defalias 'fsi-ffv 'fsi-find-function-verbatim)
(defalias 'fsi-ff 'fsi-find-function)


(defalias 'fsi-find-function-briefly 'fsi-find-function)

(defun fsi-find-function-on-key (&optional k &rest rest)
  (unless k
    (error
     "Syntax (ffo <key>)"))
  (fs-find-function (fs-describe-key-briefly k)))

(defun fsi-find-function-on-key-briefly (k &rest rest)
  (fs-find-function-briefly (fs-describe-key-briefly k)))

(defun fsi-find-function-internal (&optional function nolimitp cleanp &rest nada)
  (unless function
    (error
     "Syntax: (ff 'fucntion)"))
  (if (stringp function) (setq function (erbn-read function)))
  (cond
   ((symbolp function)
    (unless (fboundp function)
      (let ((g (intern (concat "fs-" (format "%s" function)))))
	(if (fboundp g)
	    (setq function g))))
    (let* ((fstrbare
	   (save-excursion

	     ;; This has the problem that it is interactive.. asks to
	     ;; reread file if has changed etc.
	     ;;(find-function function)
	     (find-function-do-it function nil 'set-buffer)
	     (buffer-substring (point)
			       (save-excursion
				 (forward-sexp 1)
				 (point)))))
	  (fstr1 (erbutils-function-minus-doc fstrbare))
	  (fstr (if cleanp (erbutils-function-cleanup fstr1)
		  fstr1)))
      (if (equal nolimitp 'nolimit)
	  fstr
	(concat (format "%s characters.." (length fstr))
		fstr))))
   (t "\n")))



;;; 2002-11-10 T14:50:20-0500 (Sunday)    D. Goel
(defun fsi-say (&rest args)
  ;; let's make it safe, even though we know it will be made safe again...
  (let ((response
	 (mapconcat
	  '(lambda (arg)
	     (format "%s" arg))
	  args " ")))
    (if (erbot-safe-p response) response
      (concat " " response))))









(defun fsi-regexp-quote (str)
  (unless (stringp str)
    (setq str (format "%s" str)))
  (regexp-quote str))


(defun fsi-concat (&rest sequences)
  (apply 'concat
	 (mapcar
	  'erbutils-convert-sequence
	  sequences)))








(defun erbnocmd-user-fcn-definition  (&optional mainterm )
  "The general syntax is (fs-describe TERM [N] [M]).
Looks for TERM, and shows its descriptions starting at description
number N, and ending at M-1. The first record is numbered 0.
"
  (unless mainterm
    (error
     "Format , (describe TERM &optional number1 number2)"))
  (unless mainterm
    (setq mainterm (format "%s" mainterm)))
  (setq mainterm (fs-correct-entry mainterm))
  (let* ((result0
	  (erbbdb-get-exact-notes
	   mainterm
	   ))
	 (result1 (and (stringp result0)
		       (ignore-errors (erbn-read result0))))
	 (len (length result1)))
      (cond
       ;; in cond0
       (result1
	(let* (
	       ;; notice the use of result1 here, not result.
	       (aa (first result1))
	       (bb (split-string aa))
	       (cc (first bb))
	       (dd (second bb))
	       (ee (cdr bb))
	       )
	  (cond
	   (
	    (erbutils-string= cc "redirect")
	    dd)
	   (t nil)))))))



(defun fs-seen (&rest args)
  (concat "seen "
	  (mapconcat
	   '(lambda (arg) (format "%s" arg))
	   args
	   " ")))

;; this asks the google bot for results and gives it to our channel
;;(defvar erbnocmd-google-stack nil)
;;(defun fs-google (&rest args)
;; (progn
;;  (add-to-list 'erbnocmd-google-stack 'foo))
;; (erc-cmd-MSG google "hi")
;; nil)

(defcustom fsi-internal-google-time 4
  "" :group 'erbc)

(defcustom fs-internal-dictionary-time 4
  "" :group 'erbc)

(defun fsi-google-raw (&rest args)
  "Return a list of google results. "
  (let ((concatted
	 (mapconcat '(lambda (a) (format "%s" a)) args " ")))
    (with-timeout
	(fsi-internal-google-time
	 (list concatted (list "google---TimedOut")))
      (let ((results
	     ;; this ignore-errors is very important.
	     ;; since the google stuff currently gives weird errors
	     ;; when called from within a with-timeout loop, and a
	     ;; timeout actually occurs.
	     (ignore-errors
	       (mapcar 'list
		       (google-result-urls
			(google-search concatted 0 "web")) )) ))
	;;(message "got results %S" results)
	results))))





;; (defun fsi-google-raw (&rest args)
;;   "Return a list of google results. "
;;   ;; (require 'google-this)
;;   (let ((concatted
;; 	 (mapconcat '(lambda (a) (format "%s" a)) args " ")))
;;     (with-timeout
;; 	(fsi-internal-google-time
;; 	 (list concatted (list "google---TimedOut")))
;;       (let ((results
;; 	     ;; this ignore-errors is very important.
;; 	     ;; since the google stuff currently gives weird errors
;; 	     ;; when called from within a with-timeout loop, and a
;; 	     ;; timeout actually occurs.
;; 	     (ignore-errors
;; 	       (mapcar 'list
;; 		       (google-result-urls
;; 			(google-search concatted 0 "web")) )) ))
;; 	;;(message "got results %S" results)
;; 	results)) ))




(defvar fs-internal-google-redirect-p nil)

(defun fsi-googlen (n &rest args)
  "Format the first n results in a nice format. "
  (let* ((rawres (apply 'fs-google-raw args))
	 (terms (first rawres))
	 (matches (cdr rawres)))
    (when (> (length matches) n)
      (setq matches (subseq matches 0 n)))
    (cond
     ((or (not (null matches)) (not fs-internal-google-redirect-p))
      (format "[google]    %s"
	      ;;terms
	      (if matches
		  (mapconcat 'car matches "\n")
		"No match. ")))
     (t
      (fs-english-only
       fs-internal-original-message
       fs-internal-addressedatlast
       'nogoogle
       )))))

(defun fsi-google-lucky-raw (&rest args)
  (caadr (apply 'fs-google-raw args)))


(defun fsi-google-redirect-to-google-bot (&rest args)
  (concat "google: "
	  (mapconcat
	   '(lambda (arg) (format "%s" arg))
	   args " ")))



(defun fsi-google-from-english (&rest args)
  (let ((fs-internal-google-redirect-p t))
    (apply 'fs-google args)))

(defun fsi-google (&rest args)
  (unless args (error "Syntax: , g[oogle] [NUMBER] WORD1 &rest MORE-WORDS "))
  (let (num
	(fir (first args))
	)
    (when (> (length args) 1)
      (setq num
	    (if (numberp fir)
		fir
	      (ignore-errors (erbn-read fir)))))
    (if (numberp num)
	(setq args (cdr args))
      (setq num 2))
    (apply 'fs-googlen num args)))

(defun fsi-google-with-options (options terms &rest args)
  "internal"
  (apply 'fs-google (append (list options) terms args)))

(defun fsi-google-deego (&rest args)
  "Google on the gnufans.net."
  (fs-google-with-options "site:gnufans.net" args))


(defun fsi-google-emacswiki(&rest args)
  "Google on the emacswiki site."
  (fs-google-with-options "2" (cons "site:emacswiki.org/emacs" args)))

(defun fsi-google-sl4 (&rest args)
  "Google on the sl4 site."
  (fs-google-with-options "site:sl4.org" args))

(defun fsi-google-planetmath (&rest args)
  "Google on the planetmath site."
  (fs-google-with-options "site:planetmath.org" args))

(defun fsi-google-octave (&rest args)
  "Google on the octave site."
  (fs-google-with-options "site:octave.org" args))


(defalias 'fs-go 'fs-google-octave)

(defun fs-google-wikipedia-english (&rest args)
  "Google on the emacswiki site."
  (fs-google-with-options "site:en.wikipedia.org" args))



(defun fs-google-wikipedia (&rest args)
  "Google on the emacswiki site."
  (fs-google-with-options "site:wikipedia.org" args))

(defun fs-google-wikipedia (&rest args)
  (fs-google-with-options "site:wikipedia.org" args))

(defun fs-google-imdb (&rest args)
  "Google on IMDB"
  (fs-google-with-options "2" (cons "imdb title" args)))

(defun fs-google-gnufans-org (&rest args)
  "Google on gnufans.org"
  (fs-google-with-options "2" (cons "site:gnufans.org" args)))

(defun fs-google-hurdwiki(&rest args)
  "Google on the emacswiki site."
  (fs-google-with-options "site:hurd.gnufans.org" args))


(defun fs-google-nevadamissouri (&rest args)
  "Google on the emacswiki site."
  (fs-google-with-options "site:nevadamissouri.net" args))



(defun fs-google-scarymath (&rest args)
  "Google on scarymath"
  (fs-google-with-options "site:http:scarymath.org" args))

(defun fs-google-twiki (&rest args)
  "Google on the twiki site."
  (fs-google-with-options "site:http:twiki.org" args))

;; unprovide nonfree sites..
;; (defun fs-google-usemod (&rest args)
;;   "Google on usemod"
;;   (fs-google-with-options "site:usemod.com" args))

;;(defalias 'fs-google-meatball 'fs-google-usemod)

(defun fsi-replace-regexp (&optional from to term number delimited
				    fixedcase literal subexp)
  "TODO: implemenet fixedcase, literal, subexp... If needed, let the
author know.."
  (unless (and from to term)
    (error (format "Syntax: %s (replace-regexp FROM TO TERM &optional NUMBER)" erbn-char)))
  (erbnocmd-iterate-internal term number 'replace-regexp-in-string from to
			     nil)
  (format "Replaced regexp %S with %S" from to))

(defun fsi-cp (name dest)
  (let* ((exn (erbbdb-get-exact-notes name))
	 (notes (and (stringp exn) (erbn-read exn))))
    (unless notes
      (error "No such term %s" name))
    (when (erbbdb-get-exact-notes dest)
      (error "%S already exists.  Use merge" dest))
    (fs-set-term dest notes)
    (format "Copied entries of %S to %S" name dest)))


(defun fsi-notes (name)
  "Internal. Return the notes as a list.  Else nil"
  (sit-for 0)
  (let ((exnotes (erbbdb-get-exact-notes name)))
    (and (stringp exnotes) (erbn-read exnotes))))



(defvar erbn-merge-redirect-p t
  "When true, merging also redirects.")


(defun fsi-redirectp (term)
  (let ((nn (fsi-notes term)))
    (and (= (length nn) 1)
	 (string-match "^redirect[ \t]+" (first nn)))))

(defun fsi-merge-generic (&optional name dest &rest args)
  (when (or (fsi-redirectp name)
	  (fsi-redirectp dest))
      (error "At least one of source and target is already a redirect!"))
  (unless (and name dest (not args))
    (error (format "Syntax: %s merge TERM1 TERM2" erbn-char)))
  (setq name (format "%s" name))
  (setq dest (format "%s" dest))
  (when (string= (downcase name) (downcase dest))
    (error "Cannot merge something into itself."))
  (let ((notes (fs-notes name))
	(destnotes (fs-notes dest))
	)
    (unless notes (error "No such field %S" name))
    (unless destnotes
      (error "No such field %S.  Use mv" dest))
    (setq name (fs-correct-entry name))
    (setq dest (fs-correct-entry dest))
    (erbot-working
     (mapcar
      '(lambda (arg)
	 (fs-set-also dest arg))
      notes)
     (fs-forget name "all"))
    (when erbn-merge-redirect-p
      (erbot-working
       (fsi-set-term name (format "redirect %s" dest))))
    (erbbdb-save)
    (if erbn-merge-redirect-p
	(format "Merged %S into %S, redirected %S to %S" name dest
		name dest)
      (format "Merged %S into %S" name dest))))

(defun fsi-merge-redirect (&rest args)
  (let ((erbn-merge-redirect-p t))
    (apply 'fsi-merge-generic args)))


(defalias 'fsi-merge 'fsi-merge-redirect)

(defun fsi-merge-noredirect (&rest args)
  (let ((erbn-merge-redirect-p nil))
    (apply 'fsi-merge-generic args)))

(defalias 'fsi-Merge 'fsi-merge-noredirect)


(defun fsi-mv (&optional name dest &rest args)
  "Rename NAME to DEST.
Do not confuse this function with fs-rearrange which rearranges the
order of entries within a given term. "
  (when (or args (not (and name dest)))
    (error (format "Format: %s mv foo bar" erbn-char)))
  (setq name (format "%s" name))
  (setq dest (format "%s" dest))
  (cond
   ((string= (downcase name) (downcase dest))
    (fs-mv-change-case name dest))
   (t
    (setq name (fs-correct-entry name))
    (erbot-working (fs-cp name dest))
    (erbot-working (fs-forget name "all"))
    (erbbdb-save)
    (format "Renamed the term %S to %S" name dest))))

(defalias 'fsi-rename 'fs-mv)

(defun fsi-mv-change-case (name dest)
  (when
      (let ((bbdb-case-fold-search nil))
	(erbbdb-get-exact-name dest))
    (error "Destination %S already seems to exist" dest))
  (let ((tmp (format "TMPMV-%S" (random 1000))))
    (erbot-working
     (ignore-errors (fs-forget tmp))
     (fs-mv name tmp)
     (fs-mv tmp dest))
    (erbbdb-save)
    (format "Readjusted case from %S to %S" name dest)))

(defun fsi-swap (name dest)
  (setq name (format "%s" name))
  (setq dest (format "%s" dest))
  (unless
      (let ((bbdb-case-fold-search nil))
	(erbbdb-get-exact-name dest))
    (error "Destination %S does not exist." dest))
  (unless
      (let ((bbdb-case-fold-search nil))
	(erbbdb-get-exact-name name))
    (error "Source term %S does not exist." name))
  (when (string= (downcase name) (downcase dest))
    (error "Can't swap term with itself. "))
  (let ((tmp (format "TMPMV-%S" (random 1000))))
    (erbot-working
     (ignore-errors (fs-forget tmp))
     (fs-mv name tmp)
     (fs-mv dest name)
     (fs-mv tmp dest))
    (erbbdb-save)
    (format "Readjusted case from %S to %S" name dest)))



(defun fsi-rearrange-from-english-internal (msg)
  (catch 'erbnocmd-tag-foo
    (unless (equal (length msg) 3)
      (throw 'erbnocmd-tag-foo
	     `(fs-error (format "Syntax: %s N->M in TERM" erbn-char))))
  (unless (equal (downcase (format "%s" (second msg))) "in")
    (throw 'erbnocmd-tag-foo
	   `(fs-error (format "Syntax: %s N->M in TERM" erbn-char))))
  (let (term
	fromto
	lenfromto
	)
    (setq term (third msg))
    (setq fromto
	  (split-string (first msg) "->"))
    (setq lenfromto (length fromto))
    (unless (= lenfromto 2)
      (throw 'erbnocmd-tag-foo
	     `(fs-error (format "Syntax: %s N->M in TERM" erbn-char))))
    `(fs-rearrange ,(first fromto) ,(second fromto) ,term))))




(defun fsi-replace-string-from-english-internal (msg)
  "Parse the input english message to return an elisp equivalent.
MSG here is a list which needs to be combined.  "
  (let*
      (
       ;; original length
       (leno (length msg))
       ;; remaining msg
       (remmsg msg)
       (remlen leno)
       las
       number
       remengmsg
       remenglen
       revengmsg
       splitloc
       from
       to
       term
       (ans nil)
       (termcheckp nil)
       fcn
       sr
       )
    (catch 'erbnocmd-repl-error

      (unless (and (>= leno 3)
		   (equal 0 (string-match "\\(s\\|r\\)/" (first remmsg))))
	(throw 'erbnocmd-repl-error
	       `(fs-error
		 "Format: s/foo.../bar..../ in TERM &optional N")))
      (setq sr
	    (if (equal 0 (string-match "s" (first remmsg))) "s" "r"))
      (setq las (first (last remmsg)))
      (setq number (and (stringp las) (erbn-read las)))
      (if (or (numberp number)
	      (equal 0 (string-match
			"all"
			(downcase (format "%s" number)))))
	  (setq remmsg (subseq remmsg 0 (- remlen 1)))
	(progn
	  (setq termcheckp t number nil)))

      ;; next comes the term
      (setq remlen (length remmsg))
      (setq term (first (last remmsg)))
      (setq remmsg (subseq remmsg 0 (- remlen 1)))

      (when termcheckp
	(let* ((exn (erbbdb-get-exact-notes term))
	       (notes (and (stringp exn) (erbn-read exn)))
	       (len (length notes)))
	  (if (> len 1)
	      (throw 'erbnocmd-repl-error
		     `(fs-error "Which numbered entry? %s/foo/bar in TERM NUMBER" , sr
				))
	    (setq number 0))))

      ;; now the "in"
      (setq remlen (length remmsg))
      (setq las (first (last remmsg)))
      (unless
	  (string= "in" (downcase (format "%s" las)))
	(throw 'erbnocmd-repl-error
	       `(fs-error
		 "Format: %s/foo.../bar..../ in TERM &optional NUMBER"
		 ,sr ))
	)

    (setq remmsg (subseq remmsg 0 (- remlen 1)))
    (setq remlen (length remmsg))
    (setq remengmsg (mapconcat 'identity remmsg " "))

    ;; remove trailing whitespace
    ;; no need to check for length since we know msg stars with s/
    (while
	(member
	 (aref remengmsg (- (length remengmsg) 1))
	 '(9 ;; tab
	   32 ;; space
	   10 ;; newline
	   ))
      (setq remengmsg (subseq remengmsg 0 (- (length remengmsg) 1))))
    ;; remove one trailing /
    ;; no need to check for length since we know msg stars with s/
    (setq remenglen (length remengmsg))
    (when (equal
	   (aref
	    remengmsg (- (length remengmsg) 1))
	   47)
      (setq remengmsg (subseq remengmsg 0 (- (length remengmsg) 1))))

    (setq remenglen (length remengmsg))
    (unless (> (length remengmsg) 2)
      (throw 'erbnocmd-repl-error
	     `(fs-error
	       "Format: %s/foo.../bar..../ in TERM &optional N"
	       ,sr
	       ))

      )
    ;; this should take care of almost anything imaginable.
    ;; one can still construct "missing" cases but one should just use
    ;; lisp for that.
    ;; remove the s/
    (if (equal 0 (string-match "s" remengmsg))
	(setq fcn 'fs-replace-string)
      (setq fcn 'fs-replace-regexp))
    (setq remengmsg (subseq remengmsg 2))
    ;; now find the last single /
    (with-temp-buffer
      (insert remengmsg)
      (goto-char (point-max))
      (setq splitloc
	    (search-backward-regexp  "[^/]/\\([^/]\\|$\\)" nil t)))
    (unless splitloc
      (throw 'erbnocmd-repl-error
	     `(fs-error
	       "Format: %s/foo.../bar..../ in TERM &optional N"
	       ,sr
	       )))
    (setq from (substring remengmsg 0 splitloc))
    (setq to (substring remengmsg (+ splitloc 1)))
    (when (string= from "")
      (throw 'erbnocmd-repl-error
      `(fs-error "Replacement string must have nonzero size..")))
    ;; singlify the double /'s.
    (setq from
	  (replace-regexp-in-string "//" "/" from))
    (setq to
	  (replace-regexp-in-string "//" "/" to))
    `(,fcn ,from ,to ,term ,(format "%s" number)))))



(defun fsi-replace-string (&optional from to term number)
  (unless (and from to term)
    (error
     (format "Syntax: %s s/foo.../bar in TERM [NUMBER or ALL]" erbn-char)))
  (erbot-working
   (erbnocmd-iterate-internal
    (or (erbbdb-get-exact-name term ) term)
    number 'erbutils-replace-string-in-string
    from to nil))
  (erbbdb-save)
  (format "Replaced string %S with %S." from to))

(defun erbnocmd-iterate-internal (term number function
				       &rest arglist)

  " Perform FUNCTION on the NUMBERth entry of TERM.
If NUMBER is not nil, the replacement is done for each entry in
the TERM. The function uses the term as its third argument.
Meant for use by fs-replace-regexp etc.

The last entry of ARGLIST is assumed to be itself a list of arguments,
let's call it lastlist.  Let the other entries of arglist be called
initargs.  Then the function is applied as (function @initargs string
@arglist).  Where the string is the string gotten from the TERM. "

  (setq number (format "%s" number))
  (let*
      ((exactnotes (erbbdb-get-exact-notes term))
       (notes (and (stringp exactnotes) (erbn-read exactnotes)))
       (len (length notes))
       newnotes
       newnote
       (lenargs (length arglist))
       (initargs (subseq arglist 0 (- lenargs 1)))
       (finargs (first (last arglist)))
       (numnum (erbn-read number))
       )
    (when (and (null number) (= len 1)) (setq number 0))
    (unless exactnotes (error "No such term: %S" term))
    (cond
     ((string= "all" (downcase number))
      (setq newnotes
	    (mapcar
	     (lambda (thisentry)
	       (apply function (append initargs (list thisentry)
				       finargs)))
	     notes)))
     ((or (not (numberp numnum))
	  (< numnum 0)
	  (>= numnum len))
      (error "Number should be \"all\" or within %s and %s, given was: %s"
	     0 (- len 1) numnum))
     (t
      (setq newnotes
	    (append
	     (subseq notes 0 numnum)
	     (list
	      (apply function (append initargs
				      (list (nth numnum notes))
				      finargs)))
	     (subseq notes (+ numnum  1) len)))))
    (fs-forget term "all")
    (fs-set-term term newnotes)))



(defun fsi-info-emacs (&optional regexp)
  (fs-info-file "emacs" regexp))

(defun fsi-info-elisp (&optional regexp)
  (fs-info-file "elisp" regexp))

(defun fsi-info-efaq (&optional regexp)
  (fs-info-file "efaq" regexp))

(defun fsi-info-eintr (&optional regexp)
  (fs-info-file "eintr" regexp))

(defun fsi-info (&optional regexp)
  (or
   (ignore-errors (fs-info-emacs regexp))
   (ignore-errors (fs-info-elisp regexp))
   (ignore-errors (fs-info-efaq regexp))
   (ignore-errors (fs-info-eintr regexp))
   (error "Not found in Emacs manual, elisp manual, Emacs FAQ and Elisp intro")))





(defun fsi-info-file (&optional infofile regexp)
  (unless regexp
    (error (format "Syntax: %s info-node nodename REGEXP" erbn-char)))
  (unless (stringp regexp) (setq regexp (format "%s" regexp)))


  (unless infofile (error (format "Syntax: %s info info-file REGEXP"
			      erbn-char)))
  (unless (stringp infofile) (setq infofile (format "%s" infofile)))

  (cond
   ((ignore-errors (Info-goto-node
		    (concat "(" infofile ")" regexp)))
    (concat "Press C-x C-e after: (info \"("
	    infofile ")" Info-current-node
	    "\")")
    )
   ((progn
      (ignore-errors
	(Info-goto-node (concat "(" infofile ")"))
	(Info-top-node)
	(Info-search regexp)))
    (concat "Press C-x C-e after: (info  \"("
	    infofile
	    ")" Info-current-node
	    "\")"))
   (t (error "Regexp or infofile not found in the file"))))


(defun fsi-locate-library (&optional arg &rest rest)
  "REST WILL be ignored :-)"
  (unless arg (error "Syntax: %s locate-library LIB" erbn-char))
  (unless (stringp arg)
    (setq arg (format "%s" arg)))
  (locate-library arg))


(defun fsi-avg (&rest numbers)
  (cond
   ((null numbers) 'NaN)
   (t (fs-// (apply '+ numbers)
	       (length numbers)))))


(defun fsi-dict (&optional word &rest ignore)
  (require 'connection)
  (require 'dictionary)
  (unless word (error (format "Syntax: %s d[ict] word" erbn-char)))
  (unless (stringp word) (setq word (format "%s" word)))
  (fs-dictionary-search word))

(defalias 'fsi-dictionary 'fs-dict)

(defun fsi-dictionary-search (word)
  "lispy.. not for interface. "
  (ignore-errors (kill-buffer "*Dictionary buffer*"))
  (unless (stringp word) (setq word (format "%s" word)))
  (message "fsi-dictionary-search")
  (with-timeout
      (fs-internal-dictionary-time "Dictionary--TimedOut")
    (save-window-excursion
      (dictionary-search word)
      (switch-to-buffer "*Dictionary buffer*")
      (goto-line 3)
      (buffer-substring-no-properties (point) (point-max)))))





;;8/10/00
;;;###autoload
(defun fsi-// (&rest args)
  "My sensible definition of /.
Does not say 4 / 3 = 0. Note: this usues equal and not equalp, the
last time i checked , equalp seemed to work as well.. "
  (let ((aa (apply '/ args)))
    (if (equal (car args) (apply '* aa (cdr args)))
	aa
      (apply '/ (cons (float (car args)) (cdr args))))))


(defun fsi-channel-members-all ()
  (cond
   ;; for earlier ERC.
   ((boundp 'channel-members) channel-members)
   ;; for later CVS versions of ERC.
   (t nil)))

(defun fsi-channel-members (&optional n m &rest args)
  (when (stringp n)
    (setq n (ignore-errors (erbn-read n))))
  (when (stringp m)
    (setq m (ignore-errors (erbn-read m))))
  (unless (integerp n) (setq n 0))
  (unless (integerp m) (setq m nil))
  (subseq (fs-channel-members-all) n m))


(defun fsi-length-channel-members (&rest args)
  (cond
   ;; for new erc versions
   ((boundp erc-channel-users)
    (hash-table-count erc-channel-users))
   (t (length (fs-channel-members-all)))))


(defalias 'fsi-number-channel-members 'fs-length-channel-members)

(defun fsi-cto (&rest args)
  (let* ((page (mapconcat (lambda (arg) (format "%s" arg))
			 args "%20"))
	 (pg1 "http://cliki.tunes.org/")
	 ;;(pg2 "http://206.63.100.249/")
	 (pg3
	  (erbutils-replace-strings-in-string
	   '("+" " " "\t") '("%2B" "%20" "%20") page)))
    (format "%s%s"
	    pg1 pg3)))


;;; (defun fs-karma (&rest args)
;;;   (let ((fir (first args)))
;;;     (unless
;;; 	(and
;;; 	 args
;;; 	 fir)
;;;     (error (format "Syntax: , karma ENTITY")))
;;;     (setq fir (downcase (format "%s" fir)))
;;;     (let ((result (erbkarma fir)))
;;;       (if result
;;; 	  (format "%s's karma is %s" fir result)
;;; 	(format
;;; 	 "No karma defined for %s, use ,ENTITY++ or ,karma-create" fir
;;; 	 )))))

;;; (defvar erbn-karma-pt 10)

;;; (defun fs-karma-increase (&optional arg points &rest ignore)
;;;   (unless arg (error "Syntax: foo++ [&optional NUMBER]"))
;;;   (when (stringp points)
;;;     (setq points (ignore-errors (read points))))
;;;   (unless (and (integerp points)
;;; 	       (<= (abs points) erbn-karma-pt))
;;;     (setq points erbn-karma-pt))
;;;   (setq arg (downcase (format "%s" arg)))
;;;   (erbkarma-increase arg points))

(defun fsi-karma-increase (&rest args)
  (if (car args)
      (progn

	(ignore-errors (incf (gethash (intern (format "%s" (car args))) erbn-money) 1000))


	(format
	 "Noted, %s.  One %s-point for %s!"
	 nick
	 (erbutils-random '("brownie" "karma" "wiki" "rms" "lispy" "vi" "l33t" "fsbot" "rudy" "M$"))
	 (car args))


	     )
    ;;(error "Karma system is currently being reworked. ")
    ""))




(defalias 'fs-karma-decrease 'fs-karma-increase)

;;; (defun fs-karma-decrease (&optional arg points &rest ignore)
;;;   (unless arg (error "Syntax: foo++ [&optional NUMBER]"))
;;;   (when (stringp points)
;;;     (setq points (ignore-errors (read points))))
;;;   (unless (and (integerp points)
;;; 	       (<= (abs points) erbn-karma-pt))
;;;     (setq points erbn-karma-pt))
;;;   (setq arg (downcase (format "%s" arg)))
;;;   (erbkarma-decrease arg points))



;;; (defun fs-karma (&optional foo)
;;;   (if foo (setq foo (downcase (format "%s" foo))))
;;;   (erbkarma foo))

;;; (defalias 'fs-karma-best 'erbkarma-best)


(defalias 'fsi-ncm 'fs-length-channel-members)
(defun fs-superiorp (&rest args)
  (erbutils-random '(t nil)))
(defun fs-sucksp (&rest args)
  (erbutils-random '(t nil)))
(defun fs-bugp (&rest args)
  (erbutils-random '(t nil)))


(defun fsi-country (&optional ct)
  (unless ct (error (format "Syntax: %s country NM (example , country jp)" erbn-char)))
  (setq ct (format "%s" ct))
  (let ((addp (and (> (length ct) 1)
		   ;; does not start with .
		   (not (= (aref ct 0) 46)))))
    (if addp (setq ct (concat "." ct))))
  (erbcountry (downcase ct)))



(defun fsi-country-search (&rest names)
  (unless names (error
	      (format "Syntax: %s country-search NM (example , country japa)" erbn-char)))
  (erbcountry-search
   (mapconcat (lambda (arg) (format "%s" arg)) names " ")))


;;; 2003-02-09 T13:40:04-0500 (Sunday)    D. Goel
(defun fsi-spook (&rest args)
  (with-temp-buffer
    (spook)
    (goto-char (point-min))
    (forward-line 1)
    (buffer-substring-no-properties
     (progn (beginning-of-line 1) (point))
     (progn (end-of-line 1) (point)))))


(defun fs-explode (&rest args)
  (let ((pieces
	 (erbutils-random '("a thousand" "a million" "a gazillion"
			    "aleph_2")))
	(watch
	 (erbutils-random '("" "you watch as "
			    "you run for cover as "
			    ))))
  (eval
   (erbutils-random
    '((format "%s%s explodes into %s pieces!"
	      watch erbot-nick pieces)
      (format "%s, with botheart broken into %s pieces, has left: \"Goodbye\""
	      erbot-nick pieces))))))




(defalias 'fs-die 'fs-explode)
(defalias 'fs-die! 'fs-explode)
(defalias 'fs-Die! 'fs-explode)
(defalias 'fs-Die 'fs-explode)
(defalias 'fs-DIE 'fs-explode)
(defalias 'fs-leave 'fs-explode)
(defalias 'fs-exit 'fs-explode)
(defalias 'fs-quit 'fs-explode)
(defalias 'fs-shut 'fs-explode)
(defalias 'fs-stfu 'fs-explode)
(defalias 'fs-STFU 'fs-explode)



(defun fsi-morse (&rest str)
  (apply 'erbutils-region-to-string 'morse-region str))
(defun fsi-unmorse (&rest str)
  (apply 'erbutils-region-to-string 'unmorse-region str))

(defun fsi-rot13 (&rest str)
  (let (st)
    (cond
     ((= (length str) 1)
      (setq st (format "%s" (first str))))
     (t (setq st (mapconcat
		  (lambda (a) (format "%s" a)) str " "))))
    (erbutils-rot13 st)))

(defun fsi-studlify (&rest s)
  (apply 'erbutils-region-to-string
   (lambda (&rest args)
     (ignore-errors (apply
		     'studlify-region args)))
   s))


(defun fsi-h4x0r (&rest s)
  (require 'h4x0r)
  (funcall
   'h4x0r-string
   (mapconcat
    (lambda (a) (format "%s" a))
    s " ")))


(defalias 'fs-h4 'fs-h4x0r)
(defalias 'fs-haxor 'fs-h4x0r)
(defalias 'fs-hax0r 'fs-h4x0r)
(defalias 'fs-h4xor 'fs-h4x0r)

(defalias 'fs-l33t 'fs-h4x0r)
(defalias 'fs-leet 'fs-h4x0r)

(defalias 'fs-stud 'fs-studlify)

(defcustom fsi-internal-studlify-maybe-weights
  '(100 3)
  ""
  :group 'erbc)

(defun fsi-studlify-maybe (&rest args)
  (eval
   (erbutils-random
    '((erbutils-stringify args)
      (apply 'fs-studlify args))
    fsi-internal-studlify-maybe-weights
    )))


(defcustom fsi-internal-h4x0r-maybe-weights
  '(100 2)
  ""
  :group 'erbc)

(defun fsi-h4x0r-maybe (&rest args)
  (let*
       ((aa (erbutils-stringify args))
       (bb
	(ignore-errors
	  (eval
	   (erbutils-random
	    '(aa
	      (apply 'fs-h4x0r args))
	    fsi-internal-h4x0r-maybe-weights
	    )))))
    (or bb aa)))


(defalias 'fs-stud-maybe 'fs-studlify-maybe)


(defalias 'fs-studlify-word 'studlify-word)


(defun fsi-princ (a &rest ignore)
  (princ a))


(defun fsi-pray (&rest args)
  (require 'faith)
  (let ((faith-fill-column 9999))
    (faith-quote)))

(defalias 'fsi-all-hail-emacs 'fs-pray)
(defalias 'fsi-hail-emacs 'fs-pray)
(defalias 'fsi-faith 'fs-pray)

(erbutils-defalias-i '(faith-correct-string))
(erbutils-defalias-i '(member))

(erbutils-defalias-i '(stringp consp symbolp numberp listp arrayp
			       boundp bufferp commandp consp endp
			       equalp evenp oddp facep fboundp
			       featurep functionp integerp keywordp
			       keymapp listp markerp minusp natnump
			       nlistp numberp overlayp plusp rationalp
			       sequencep subrp tailp timerp
			       typep vectorp windowp xemacsp zerop))


(erbutils-defalias-i
 '(char-to-string string-to-char string-to-int
		  string-to-number string-to-list
		  string-to-number-with-radix number-to-string
		  pp-to-string int-to-string number-to-string
		  prin1-to-string rational-to-string rational-to-float
		  radians-to-degrees degrees-to-radians))





(defun erbn-shell-test (string &optional substrings)
  "Return t if any of the substrings matches string..  Used to weed
out harmful shell code..

See: http://www.w3.org/Security/faq/wwwsf4.html#CGI-Q7


"
  (unless substrings
    (setq substrings (list " " "<" ">" "-" "`" "$" "=" ";" "&" "'"
			   "\\" "\"" "|" "*" "?" "~" "^" "(" ")" "["
			   "]" "{" "}" "\n" "\r" )))
  (let ((found nil))
    (mapcar (lambda (arg)
	      (when (string-match (regexp-quote arg) string)
		(setq found t)))
	    substrings)
    found))

(defalias 'fsi-shell-test 'erbn-shell-test)

(defcustom erbn-internal-web-page-time 10
  "" :group 'erbc)
(defcustom erbn-url-functions-p nil
  "when true, enable url functions, provided that erbot-paranoid-p
allows us that.

The reason you may not want to enable this function is that when you
fetch url's like http://205.188.215.230:8012 (icecast, etc. content),
url.el continues fetching that url forever (discovered by indio).  The
bot times out, but url continues fetching it in the background,
slowing down your bot."
  :group 'erbc)



(defmacro erbn-with-web-page-buffer (site &rest body)
  (let ((buffer (make-symbol "web-buffer")))
    `(progn
       (unless (and (not erbot-paranoid-p)
		    erbn-url-functions-p)
	 (error "erbn-url-functions-p is disabled"))
       (with-timeout (erbn-internal-web-page-time "HTTP time out")
	 (let ((,buffer (url-retrieve-synchronously ,site)))
	   (when (null ,buffer)
	     (error "Invalid URL %s" site))
	   (save-excursion
	     (set-buffer ,buffer)
	     (goto-char (point-min))
	     (prog1
		 (progn
		   ,@body)
	       (kill-buffer ,buffer))))))))

(defun fsi-web-page-title (&optional site &rest args)
  (unless site (error (format "Syntax: %s web-page-title SITE" erbn-char)))
  (setq site (format "%s" site))
  (erbn-with-web-page-buffer site
    (let* ((case-fold-search t)
           (beg (search-forward "<title>" nil t))
           (end (search-forward "</title>" nil t)))
      (concat "That page title is "
              (if (and beg end)
                  (erbutils-cleanup-whitespace
                   (buffer-substring beg (- end 8)))
                "not available")))))

(defun fsi-wserver (&optional site &rest args)
  (unless site (error (format "Syntax: %s wserver SITE" erbn-char)))
  (setq site (format "%s" site))
  (erbn-with-web-page-buffer site
    (buffer-substring (point-min)
                      (or (search-forward "\n\n" nil t)
                          (point-max)))))

(defalias 'fs-webserver 'fs-wserver)

(defun fsi-web (&optional site &rest args)
  (unless site (error (format "Syntax: %s web SITE" erbn-char)))
  (setq site (format "%s" site))
  (erbn-with-web-page-buffer site
    (shell-command-on-region (or (search-forward "\n\n" nil t)
                                 (point-min))
                             (point-max)
                             "w3m -dump -T text/html" t t)
    (buffer-substring (point) (mark))))


;;;###autoload
(defun fsi-length-load-history ()
  (interactive)
  (message "%s%s%S"
	   (length load-history)
	   " ..." (mapcar 'car load-history)))


;(defun fsi-load-history ()
;  load-history)
;(defun fsi-load-history ()
;  load-history)

(defalias 'fs-google: 'fs-google)



(defconst fs-bunny 142857)
(defconst fs-pi float-pi)
(defconst fs-e float-e)
(defconst fs-euler float-e)
(defconst fs-emacs-version emacs-version)

(defalias 'fsi-emacs-version 'emacs-version)
(defalias 'fsi-gnus-version 'gnus-version)

;; the short aliases..
(defalias 'fsi-a 'fs-apropos)
(defalias 'fs-da 'fs-apropos)
(defalias 'fsi-ac 'fs-apropos-command)
(defalias 'fsi-ad 'fs-apropos-documentation)
(defalias 'fsi-af 'fs-apropos-function)
(defalias 'fsi-av 'fs-apropos-variable)

(defalias 'fsi-c 'fs-commands)
(defalias 'fsi-d 'fs-dict)
(defalias 'fsi-dict: 'fs-dict)

(defalias 'fsi-dl 'fs-describe-literally)
(defalias 'fsi-doc 'fs-doctor )
(defalias 'fsi-dkb 'fs-describe-key-briefly )

(defalias 'fsi-dk 'fs-describe-key)
(defalias 'fsi-dkf 'fs-describe-key-and-function)
(defalias 'fsi-dkl 'fs-describe-key-long)

(defalias 'fs-lkgg 'fs-lookup-key-gnus-group)
(defalias 'fs-dkgg 'fs-lookup-key-gnus-group)

(defalias 'fs-dkgs 'fs-lookup-key-gnus-summary)
(defalias 'fs-lkgs 'fs-lookup-key-gnus-summary)

(defalias 'fs-lkm 'fs-lookup-key-message)
(defalias 'fs-lkm 'fs-lookup-key-message)


(defalias 'fsi-df 'fs-describe-function )
(defalias 'fsi-cond 'cond)
(defalias 'fsi-if 'if)
(defalias 'fsi-when 'when)
(defalias 'fsi-dfl 'fs-describe-function-long )
(defalias 'fsi-dv 'fs-describe-variable )
(defalias 'fsi-ff 'fs-find-function)
(defalias 'fsi-ffb 'fs-find-function-briefly)
(defalias 'fsi-ffo 'fs-find-function-on-key)
(defalias 'fsi-ffob 'fs-find-function-on-key-briefly)
(defalias 'fsi-fv 'fs-find-variable)
(defalias 'fsi-fvb 'fs-find-variable-briefly)
(defalias 'fsi-? 'fs-help)
(defalias 'fs-32 'fs-help)
(defalias 'fsi-s  'fs-search)
(defalias 'fsi-sw  'fs-search-wide)
(defalias 'fsi-sws  'fs-search-wide-sensitive)
(defalias 'fsi-wi  'fs-where-is)
(defalias 'fs-wigg  'fs-where-is-gnus-group)
(defalias 'fs-wigs  'fs-where-is-gnus-summary)
(defalias 'fs-wim  'fs-where-is-message)
(defalias 'fs-dw  'fs-where-is)
;;(defalias 'fs-yo 'fs-hi)

;; basic functions
;; undefun lambda:
;; (defalias 'fsi-lambda 'lambda)  ;; huh?
(defalias 'fsi-length 'length)
(defalias 'fsi-sqrt 'sqrt)

(defalias 'fsi-= '=)
(defalias 'fsi-/= '/=)
(defalias 'fsi-< '<)
(defalias 'fsi-> '>)
(defalias 'fsi-<= '<=)
(defalias 'fsi->= '>=)
(defalias 'fsi-not 'not)
(defalias 'fsi-and 'and)
(defalias 'fsi-or 'or)
(defalias 'fs-lart 'fs-flame)

(defalias 'fsi-null 'null)
(defalias 'fsi-atom 'atom)
;;(defalias 'fsi-stringp 'stringp)
;;(defalias 'fsi-consp 'consp)




(defalias 'fsi-equal 'equal)
(defalias 'fsi-equalp 'equalp)
(defalias 'fsi-eql 'eql)
;; rr is used for russian-roulette now..
;;(defalias 'fs-rr 'fs-replace-regexp)
(defalias 'fs-rs 'fs-replace-string)
(defalias 'fsi-+ '+)
(defalias 'fsi-- '-)
(defalias 'fsi-* '*)
(defalias 'fsi-/ '/)
(defalias 'fsi-less 'fs-more)
(defalias 'fsi-list 'list)
(defalias 'fsi-car 'car)
(defalias 'fs-ct 'erbccountry)
(defalias 'fsi-cdr 'cdr)
(defalias 'fsi-cons 'cons)
(defalias 'fsi-append 'append)
(defalias 'fsi-first 'first)
(defalias 'fsi-second 'second)
(defalias 'fsi-third 'third)
(defalias 'fsi-fourth 'fourth)
(defalias 'fsi-fifth 'fifth)
(defalias 'fsi-sixth 'sixth)
(defalias 'fsi-seventh 'seventh)
(defalias 'fsi-eighth 'eighth)
(defalias 'fsi-ninth 'ninth)
(defalias 'fsi-tenth 'tenth)
(defalias 'fsi-subseq 'subseq)
(defalias 'fsi-ceiling 'ceiling)
(defalias 'fsi-ceiling* 'ceiling*)
(defalias 'fsi-cos 'cos)
(defalias 'fsi-count-lines 'count-lines)

(defalias 'fsi-last 'last)
(defalias 'fsi-llh 'fs-length-load-history)
(defalias 'fsi-error 'erbutils-error)
(defalias 'fsi-expt 'expt)
(defalias 'fsi-exp 'exp)
(defalias 'fsi-exchange-point-and-mark 'exchange-point-and-mark)
(defalias 'fs-rq 'fs-regexp-quote)
;; (defalias 'fs-function 'identity)

(defalias 'fsi-identity 'identity)
(defalias 'fsi-nth 'nth)
(defalias 'fsi-nthcdr 'nthcdr)
(defalias 'fsi-random 'random)
(defalias 'fsi-random-choose 'erbutils-random)
(defalias 'fsi-remove 'remove)
(defalias 'fsi-replace-regexp-in-string 'erbutils-replace-regexp-in-string)
(defalias 'fsi-replace-match 'replace-match)

(defalias 'fsi-number-to-string 'number-to-string)
(defalias 'fsi-format 'format)
(erbutils-defalias-i '(format-time-string))

(defalias 'fsi-split-string 'split-string)
(defalias 'fsi-rm 'fs-forget)
(defalias 'fsi-progn 'progn)
(defalias 'fsi-ignore-errors 'ignore-errors)
(defalias 'fsi-lcm 'lcm)
(defalias 'fsi-let 'let)
(defalias 'fsi-let* 'let*)
(defalias 'fsi-ll 'fs-locate-library)
(defalias 'fsi-g 'fs-google)
(defalias 'fsi-gcd 'gcd)
(defalias 'fs-gd 'fs-google-deego)

(defalias 'fsi-ge 'fs-google-emacswiki)
(defalias 'fs-gs 'fs-google-sl4)

(defalias 'fs-gw 'fs-google-wikipedia)
(defalias 'fs-gi 'fs-google-imdb)
(defalias 'fs-gwe 'fs-google-wikipedia-english)
(defalias 'fs-gh 'fs-google-hurdwiki)
;;(defalias 'fs-gm 'fs-google-meatball)
(defalias 'fs-gnufans 'fs-google-gnufans-net)
(defalias 'fs-gg 'fs-google-gnufans-net)
(defalias 'fs-ggn 'fs-google-gnufans-net)
(defalias 'fs-ggo 'fs-google-gnufans-org)
(defalias 'fs-gn 'fs-google-nevadamissouri)
(defalias 'fs-gp 'fs-google-planetmath)
(defalias 'fs-gt 'fs-google-twiki)
;;(defalias 'fs-gu 'fs-google-usemod)

(defalias 'fsi-mark 'mark)
(defalias 'fsi-point 'point)
(defalias 'fsi-pop-mark 'pop-mark)
(defalias 'fsi-push-mark 'push-mark)
(defalias 'fsi-floor 'floor)
(defalias 'fsi-floor* 'floor*)

(defalias 'fsi-round 'round)
(defalias 'fsi-round* 'round*)

(defalias 'fsi-setcar 'setcar)
(defalias 'fsi-setcdr 'setcdr)
(defalias 'fsi-sin 'sin)
(erbutils-defalias-i '(sleep-for sit-for))
(defalias 'fsi-string 'string)

(defalias 'fsi-string-as-multibyte 'string-as-multibyte)
(defalias 'fsi-string-bytes 'string-bytes)
(defalias 'fsi-string-equal 'string-equal)
(defalias 'fsi-string-key-binding 'string-key-binding)
(defalias 'fsi-string-lessp 'string-lessp)
(defalias 'fsi-string-make-multibyte 'string-make-multibyte)
(defalias 'fsi-string-make-unibyte 'string-make-unibyte)
(defalias 'fsi-string-to-char 'string-to-char)
(defalias 'fsi-string-to-int 'string-to-int)
(defalias 'fsi-string-to-list 'string-to-list)
(defalias 'fsi-string-to-number 'string-to-number)
(defalias 'fsi-string-to-sequence 'string-to-sequence)
(defalias 'fsi-string-to-syntax 'string-to-syntax)
(defalias 'fsi-string-to-vector 'string-to-vector)
(defalias 'fsi-string-width 'string-width)
(defalias 'fsi-symbol-file 'symbol-file)

(defalias 'fsi-tan 'tan)
(defalias 'fsi-cos 'cos)
(defalias 'fsi-sin 'sin)
(defalias 'fsi-atan 'atan)
(defalias 'fsi-asin 'asin)
(defalias 'fsi-acos 'acos)
(defalias 'fsi-tanh 'tanh)

(erbutils-defalias-i
 '(timezone-world-timezones
   timezone-months-assoc
   timezone-make-date-arpa-standard timezone-make-date-sortable
   timezone-make-arpa-date timezone-make-sortable-date
   timezone-make-time-string timezone-parse-date timezone-parse-time
   timezone-zone-to-minute timezone-time-from-absolute
   timezone-time-zone-from-absolute timezone-fix-time
   timezone-last-day-of-month timezone-leap-year-p timezone-day-number
   timezone-absolute-from-gregorian))


(defalias 'fsi-truncate 'truncate)

(defalias 'fsi-truncate* 'truncate*)
(defalias 'fsi-truncate-string 'truncate-string)
(defalias 'fsi-truncate-string-to-width 'truncate-string-to-width)


(defalias 'fsi-erc-version 'erc-version)
(defalias 'fsi-sv 'erc-cmd-SV)
(defalias 'fsi-erc-cmd-SV 'erc-cmd-SV)
(defalias 'fsi-smv 'erc-cmd-SMV)
(defalias 'fsi-erc-cmd-SMV 'erc-cmd-SMV)
(defalias 'fsi-sm 'erc-cmd-SM)
(defalias 'fsi-cmd-SM 'erc-cmd-SM)

(defalias 'fsi-stringify 'erbutils-stringify)
(defalias 'fsi-stringifyb 'erbutils-stringifyb)
(defalias 'fsi-stringifyc 'erbutils-stringifyc)

;; (defalias 'fs-while 'while)

;;;====================================================

;;;====================================================
;; ERRORS:

(defun fsi-load-library (&rest args)
  (error "Use 'require instead. "))

(defalias 'fs-load 'fs-load-library)
(defalias 'fs-load-file 'fs-load-library)



;; cl-extra.el

(defalias 'fsi-equalp 'equalp)
;; done gcd
;; done lcm
(defalias 'fsi-isqrt 'isqrt)
(defalias 'fsi-floor*
  'floor* )

(defalias 'fsi-ceiling*
'ceiling* )

(defalias 'fsi-truncate*
'truncate*)

;; done round*

(defalias 'fsi-mod*
  'mod* )


(defun fsi-geek-code ()
  (require 'geek)
  (let ((s (geek-code)))
    (if (> (length s) 70)
	(substring s 0 70)
      s)))

(defalias 'fsi-rem*
  'rem* )

(erbutils-defalias-i
 '(signum
   ;; yes?
   make-random-state
   random-state-p

   most-positive-float most-negative-float
   least-positive-float least-negative-float
   least-positive-normalized-float least-negative-normalized-float
   float-epsilon float-negative-epsilon ;; done subseq
   concatenate revappend nreconc list-length tailp 
   copy-tree
   ;;get* getf
   ;;cl-set-getf cl-do-remf cl-remprop remprop
   ))


;; oct.el

(ignore-errors (require 'oct))

(erbutils-defalias-i


 '(
   oct-zeros oct-ones oct-sum oct-size
   oct-rows oct-columns oct-\.*
   oct-add
   oct-corr oct-complement oct-sumsq oct-mean
   oct-sqrt oct-std oct-tanh oct-atanh)
 "" "oct-")

(erbutils-defalias-i '(oct-/ oct-+ ))
(erbutils-defalias-i '(lsh))
(erbutils-defalias-i '(obarray))


;; files.el
(erbutils-defalias-i
 '(auto-mode-alist interpreter-mode-alist
		   directory-abbrev-alist))


(erbutils-defalias-i '(load-history))
(erbutils-defalias-i '(assoc))
(erbutils-defalias-i '(eq))
(erbutils-defalias-i '(message))
(erbutils-defalias-i '(decf))
(erbutils-defalias-i '(incf))
(erbutils-defalias-i '(faith-quote))
(erbutils-defalias-i '(zerop))
;;(erbutils-defalias-i '(buffer-substring))
(erbutils-defalias-i '(buffer-substring-no-properties))
;;(erbutils-defalias-i '(buffer-string))

;; We define it to be no-properties, else people can (setq foo
;; (buffer-string)).. and cause a huge uservariables file..

(defun fsi-buffer-string (&rest args)
  (buffer-substring-no-properties (point-min) (point-max)))

(defalias 'fsi-buffer-substring 'buffer-substring-no-properties)


(erbutils-defalias-i
 '(featurep feature-symbols feature-file features

		     ))
(erbutils-defalias-i
 '(minor-mode-alist minor-mode-map-alist
		    minor-mode-overriding-map-alist))
(erbutils-defalias-vars '(major-mode))

;; from gnus-group.el

(erbutils-defalias-vars '(gnus-group-mode-map))
(erbutils-defalias-vars '(gnus-summary-mode-map))
(erbutils-defalias-vars '(message-mode-map))
(erbutils-defalias-vars '(text-mode-map))
(erbutils-defalias-vars '(emacs-lisp-mode-map))
(erbutils-defalias-vars '(lisp-mode-map))

(erbutils-defalias-i '(boundp fboundp))
(erbutils-defalias-i '(lookup-key))
(erbutils-defalias-i '(minor-mode-key-binding))

(erbutils-defalias-i '(where-is-internal))
(erbutils-defalias-i '(% abs))

(erbutils-defalias-i '(cdr cddr car cadr cdar))
(erbutils-defalias-i '(erc-channel-list))

(when (ignore-errors (require 'units))
  (erbutils-defalias-i '(units-version units-load-hook units-dat-file
				     units-buffer units-s-to-n
				     units-prefix-convert
				     units-si-prefix-list
				     units-si-short-prefix-list
				     units-convert-1 units-convert)))


(defvar erbn-nicks-dead nil)

(defun erbn-mark-dead (&rest ignore)
  (let ((ni (format "%s" erbn-nick)))
    (unless (string= ni "nil")
      (add-to-list 'erbn-nicks-dead (format "%s" erbn-nick)))))



;; allow people to mark themselves dead :-)
(defalias 'fsi-mark-dead 'erbn-mark-dead)

(defun erbn-unmark-dead (nick)
  (setq erbn-nicks-dead  (remove (format "%s" nick) erbn-nicks-dead)))



(defun erbn-dead-check (&rest ignore)
  (when (fsi-dead-p erbn-nick)
    (error "I see dead people!
                     .... (but I don't talk to them!)")))

(defalias 'fsi-dead-check 'erbn-dead-check)

(defun erbn-dead-p (&optional nick)
  (unless nick (setq nick erbn-nick))
  (member (format "%s" nick) erbn-nicks-dead))

(defalias 'fsi-dead-p 'erbn-dead-p)



(defun fs-give (&optional nini &rest stuff)
  (unless nini (setq nini "self"))
  (when (string= "me" nini)
    (setq nini nick))
  (unless stuff (setq stuff '("a" "beer")))
  (format "/me gives %s %s"
	  nini
	  (mapconcat
	   (lambda (arg) (format "%s" arg))
	   stuff " ")))


(defalias 'fs-hand 'fs-give)

(erbutils-defalias-i
 '(backward-kill-sentence
   backward-sentence
   flame-sentence flame-sentence-ify
   flame-sentence-loop forward-sentence kill-sentence
   mark-end-of-sentence sentence-at-point sentence-end
   sentence-end-double-space sentence-end-without-period
   transpose-sentences))

(defalias 'fsi-flatten 'erbutils-flatten)



(erbutils-defalias-i '(log))
(erbutils-defalias-i
 '(most-positive-fixnum
   most-negative-fixnum))



(erbutils-defalias-i
 '(
   regexp-opt
   regexp-opt-depth
   regexp-opt-group regexp-opt-charset))

(erbutils-defalias-i '(window-system))


(defvar erbot-kbd-p nil
  "Whether to enable kbd.

Note that making this non-nil can lead to vector results. For
example, (kbd \"<home>\"), (thanks to fledermaus).")

(when (and 
       (not erbot-paranoid-p)
       erbot-kbd-p
       (erbutils-defalias-i
	'(kbd read-kbd-macro))))

(defconst fs-t t
  "As such, when we sandbox a lisp expression, t remains t, so this is
not needed.
However, inside macros like (cond (t....)), t becomes fs-t because
it occurs in an unusual place.  this const should take care of it..
Of course, this also opens the bot to some FUN user abuse, when they
setq fs-t to nil :-) ")


(defconst fs-nil nil
  "See the doc of fs-t ")


(defun fsi-revive (&optional name &rest ignore)
  (unless name (error "no one to revive"))
  (setq name (format "%s" name))
  (let (ansstr)
    (setq ansstr
	  (cond
	   ((string= name nick)
	    (concat "Thou idiot, " nick ", thou canst not revive thyself!"))
	   (t (concat
	       "/me sprinkles some "
	       (erbutils-random
		'("clear" "murky" "boiling" "dark" "holy" "smelly"))
	       " potion on "
	       (format "%s" name)
	       " and utters some prayers.  "
	       (erbutils-random
		(list
		 (format "%s wakes up" name)
		 "Nothing happens."
		 (format "%s wakes up, all refreshed. " name)
		 (format "%s wakes up, all confused. " name)
		 ))))))
    (when (string-match "wakes up" ansstr)
      (erbn-unmark-dead name))
    ansstr))

;; this may be unsafe, remove it:
;; (defalias 'fs-sandbox-quoted 'erblisp-sandbox-quoted)
;; (defalias 'fs-sandbox-quoted-maybe 'erblisp-sandbox-quoted-maybe)
;; (defalias 'fs-sandbox 'erblisp-sandbox)

(erbutils-defalias-i '(macroexpand))


;;"/usr/share/emacs/21.2/lisp/emacs-lisp/pp.el"
(erbutils-defalias
 '(pp-escape-newlines
   pp-to-string
   ;; pp pp-eval-expression
   ;;pp-eval-last-sexp))
   ))


(erbutils-defalias-i '(string-match identity))

(erbutils-defalias-i '(parse-time-string))

(erbutils-defalias-i '(reverse))

(defun fsi-pp (object &rest ignore)
  (pp object))


(defmacro fs-privmsg (&rest args)
  "This macro is carefully constructed so that one user cannot force a
query to another user. "
  `(cond
    ;; This can occur when you are requesting a parse..
    ((null erbn-nick)
     (progn ,@args))
    (t
     (progn
       (setq erbn-tgt erbn-nick)
       ;; If there isn't already a buffer, create one..
       (erbn-query erbn-nick)
       ,@args))))

(defun erbn-query (qnick)
  (save-excursion (erc-query qnick erbn-buffer)))



(defun fsi-read-or-orig (arg)
  "  If string and can read, read, else return the arg.
Note: Used by fs-describe"
  (cond
   ((stringp arg)
    (condition-case fs-tmp (erbn-read arg)
      (error arg)))
   (t arg)))


(defun erbn-read-from-string (str)
  (let (str2)
  (cond
   ((stringp str)
    (setq str2 (copy-sequence str))
    (set-text-properties 0 (length str2) nil str2)
    (read-from-string str))
   (t (error "The bot will only read from strings. ")))))



(defun erbn-read (str)
  "Like read, but only from strings"
  (car (erbn-read-from-string str)))


(defalias 'fsi-read 'erbn-read)
(defalias 'fsi-read-from-string 'erbn-read-from-string)


(erbutils-defalias-i
 '(substring subr-arity subrp subseq
	     subst-char-in-string
	     subtract-time
	     time-subtract
	     time-add
	     date-to-time
	     time-to-seconds
	     time-less-p
	     seconds-to-time
	     days-to-time
	     time-since
	     subtract-time
	     date-to-day
	     days-between
	     date-leap-year-p
	     time-to-day-in-year time-to-days time-to-number-of-days
	     safe-date-to-time))


(erbutils-defalias-i '(ignore))

(erbutils-defalias-i '(caar elt))

(defun fsi-apropos-term (&rest args)
  "Look for input string in the term. First arg is the TERM. The rest is the input string.
This was programmed into fsbot by someone, possibly ijp, on #emacs. Modified heavily by deego."
  (let*
      ;;((re (mapconcat 'symbol-name (cdr args) " "))
      ((re (fsi-stringify (cdr args)))
       (matches1 
	(mapcar 
	 ;; return x at last if it matches.. that way, we end up returning the matching entry.
	 (lambda (x) (and (string-match re x ) x))
	 ;; put (car args) in a list below, because that's when stringify does its thing. 
	 (fsi-notes (fsi-stringify (list (car args))))))
       (ctr -1)
       (matches
	(mapcar 
	 (lambda (item) (incf ctr) (if item (format "%d: %s" ctr item) nil))
	 matches1))
       (matches (remove nil matches)))
    (if matches 
	(erbutils-itemize matches)
	"No matches found.")))


(defalias 'fsi-itemize 'erbutils-itemize)		
(defalias 'fsi-replace-string-in-string 'erbutils-replace-string-in-string)

(defalias 'fsi-quote 'quote)
(erbutils-defalias-f '(string> string<))

(defun fsi-concatenate (type &rest seqs)
  (cond
   ((eq type 'string) (apply 'fsi-concat seqs))
   ((eq type 'list) (apply 'fsi-concat seqs))
   (t (error ("fsbot concatenate only works for strings or lists.")))))



(provide 'erbc)
(run-hooks 'fs-after-load-hooks)



;;; erbc.el ends here

