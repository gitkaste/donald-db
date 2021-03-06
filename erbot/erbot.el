;;; erbot.el --- Another robot for ERC.
;; Time-stamp: <2006-08-24 11:11:11 deego>
;; Emacs Lisp Archive entry
;; Filename: erbot.el
;; Package: erbot
;; Authors:  David Edmunston (dme@dme.org)
;; Modified by: D. Goel <deego@gnufans.org>
;; Version: 0.0
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot
;; Maintainer: Deepak Goel <deego@gnufans.org>


(defvar erbot-home-page
  "http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot/")

;; Version:
;; Keywords: ERC, IRC, chat, robot, bot

;; Copyright (C) 2002 Deepak Goel, FSF

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.




;; See also:
;; erc-robot.el from which this was derived...




;; See http://www.emacswiki.org/cgi-bin/wiki/ErBot

;; OLD DOCS:
;; Thanks for erbot's/erbot's behavior and their data go to a lot
;; of people on #emacs, like:
;; kensanata (Alex Schroeder)
;; resolve (Damien Elmes)
;; bpt  (Brian P. Templeton)
;; forcer (Jorgen "forcer" Schaefer)
;; and many others

;; and also to bot(s):
;; apt on debian, for english syntax examples.

;; Thanks for code go to:
;; David Edmonsdon (who wrote erc-robot.el which is what this started
;; out from).
;; Nick Hober (who wrote google.el)






;;; David E's Commentary: (where is it? -dg,2014.)

;; dg's commentary:
;; erbot is a derivative of David's erc-robot.el --- that code was
;; copied over on 2002-09-02 into erbot.el.  Erbot seeks to make the
;; bot similar to apt on #debian.. viz: English style.. yet allowing
;; access to commands via the "cmd" command.  Erbot shall seek to
;; save all its information periodically, and publicly...


;; Erc-robot implements a simple robot for ERC.

;; Installation:

;; The robot uses hooks to gain access to ERC.  The following need to
;; be executed after ERC has loaded:

;;     (load-library "erbot")


;; It is particularly important that the remote robot function is added
;; to the tail of the PRIVMSG hook.


;; old, and wrong:
;; Robot commands are declared using the list "erbot-commands".
;; XXX better description of the functions.
;; An example might be:

;; (setq erbot-commands
;;       '(
;; 	("cmds" t (lambda (args)
;; 		  (concat "commands available: "
;; 			  (mapconcat
;; 			   (lambda (e)
;; 			     (car e))
;; 			   erbot-commands " "))))
;; 	("hello" t (lambda (args) "hello to you too !"))
;; 	("zippy" t (lambda (args) (erc-replace-regexp-in-string "\n" " " (yow))))
;; 	("music" t (lambda (args) (concat "now playing: "
;; 					(let ((track (dme:now-playing)))
;; 					  (if track
;; 					      track
;; 					    "nothing.")))))
;; 	("echo" t (lambda (args) args))
;;	; only i'm allowed to talk to my doctor !
;; 	("doctor" nil erc-doctor)
;; 	("version" t (lambda (args) (erc-version)))
;;       ))


; compatability
;(if (featurep 'xemacs)
;    (defun erc-replace-regexp-in-string
;      (regexp rep string &optional fixedcase literal subexp start)
;      (replace-in-string string regexp rep literal))

(defalias 'erc-replace-regexp-in-string 'replace-regexp-in-string)

(defvar erbot-paranoid-p t
  " Meant as a CATCHALL for security. Setting this variable to non-nil
should disable most features. When non-nil, all potentially funny
functions are disabled.  We think these functions are safe, but we
disable them in any case.  We also disable all functions that we can
that may potentially freeze the bot or severly slow it down upon
receiving weird requests.


t by default.  No enablings like erbot-setf-p, etc. will work
unless this is non-nil. If this is non-nil, erbot is paranoid, it will
not allow apply, setf, funcall, sregex, etc. even if the corresponding
variables are turned on.

NOTE: Making this variable nil and later non-nil in the middle of a
running emacs session will NOT make your bot completely paranoid.  You
need to have this function non-nil BEFORE you load erbot. See, for
example, how we define fs-kbd.
")




(defun erbot-commentary ()
  "Provides electric help regarding variable `erbot-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbot-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:

;;; New features:
(defvar erbot-new-features
  "Help..."
)

(defun erbot-new-features ()
  "Provides electric help regarding variable `erbot-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbot-new-features) nil) "*doc*"))

;;; TO DO:
(defvar erbot-todo
  "Current shortcomings:"

)

(defun erbot-todo ()
  "Provides electric help regarding variable `erbot-todo'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbot-todo) nil) "*doc*"))

(defvar erbot-version "0.0")

;;==========================================
;;; Code:
(require 'cl)

(defcustom erbot-before-load-hooks nil "" :group 'erbot)
(defcustom erbot-after-load-hooks nil "" :group 'erbot)



(defcustom erbot-ignore-nicks '("^apt[0-9]?$" "bot" "google" "serv")
  "A list of REGEXPS.
Nicks matching these regexps will be ignored by the bot, viz. not
generate replies.

I would suggest including atleast bot, google and serv here to prevent
infinite chat loops with other bots.  :)
"
:type '(repeat regexp)
:group 'erbot)

(defcustom erbot-use-whitelist nil "Use a whitelist for accessing the bot.
Any request from another source will be ignored. If a source is present in whitelist
and in `erbot-ignore-nicks' it is ignored"
:type 'boolean
:group 'erbot)

(defcustom erbot-whitelist-nicks nil
"List of the entries that have access to the bot. Used only when `erbot-use-whitelist' is non-nil"
:type '(repeat regexp)
:group 'erbot)

(defcustom erbot-ignore-userinfos "" "list of regex's" :group 'erbot)
(run-hooks 'erbot-before-load-hooks)


(defgroup erbot nil
  "The group erbot"
   :group 'applications)

(defcustom erbot-nick "fsbot"
"Changing this in the middle of things
may have unspecified and unpleasant results..."
:group 'erbot)

(defvar erbot-end-user-nick "dummy-nick"
  "just a temporary variable..")

(defvar erbot-end-user-nick-latest "dummy-end-user-nick-latest"
  "just a temporary variable..")





(defcustom erbot-servers-channels
      '(("irc.openprojects.net"
         ("#testopn"
            ))
	(".gnome.org"
         ("#testgnome")
	 ;; optional but:
	 6667
	 ))
      "Servers and channels ..."
      :group 'erbot)



;  (defalias 'erc-replace-regexp-in-string 'replace-regexp-in-string))


(defface erbot-face '((t (:foreground "yellow")))
  "Face used for your robot's output."
  :group 'erc-faces)

(defcustom erbot-commands nil
  "A list of robot commands and the functions which implement them."
  :group 'erc
  :type '(repeat (list string (choice (const nil) (const t) string) function))
  )



(defcustom erbot-erbmsg-p nil
  "When true, erball.el loads the erbmsg module by default ")


(defcustom erbot-notify-p t 
  "Set it to t if you want RSS notification
for your erbot. 

Note that even if it is t, we will internally setq it to nil temporarily during
the inner workings of the bot.  ")

;; The next part suggested by forcer, See
;; http://www.kollektiv-hamburg.de/~forcer/erbot-notify.txt, which is
;; also copied here: 

;; erbot should include the following function lists, which are
;; called on these events with the specified arguments:

;;  erbot-notify-add-functions
;;    arguments: nick channel term entry-num entry

(defvar erbot-notify-add-functions nil
  "Functions  to call when an erbot add takes place.  Each of these is
called with the arguments arguments: nick channel term entry-num
entry")

;;  erbot-notify-forget-functions
;;    arguments: nick channel term entry-num entry
;;    If entry-num is 'all, entry is a list of entries


;; SPECS CHANGED!
(defvar erbot-notify-forget-functions nil
  "Functions to call when an erbot forget takes place.  Each of these
is called with the arguments arguments: nick channel term entry-num
entry remaining-entries.  If entry-num is 'all, entry is a list of
entries")

;;  erbot-notify-move-functions
;;    arguments: nick channel old-term new-term

(defvar erbot-notify-move-functions nil
  "Functions to call when an erbot move operation takes place.  Each
of these is called with the arguments arguments: nick channel old-term
new-term ")

;;  erbot-notify-rearrange-functions
;;    arguments: nick channel term from-num from-entry to-num
;;    entries

(defvar  erbot-notify-rearrange-functions nil
  "Functions to call when an erbot rearrange operation takes place.  Each
of these is called with the arguments arguments: nick channel term
from-num from-entry to-num entries.  Entries refers to the rearranged
entries. ")


;;  erbot-notify-substitute-functions
;;    arguments: nick channel term entry-num old-entry new-entry
(defvar erbot-notify-substitute-functions nil 
  "Functions to call when an erbot substitute operation takes place.
Each of these is called with the arguments arguments: nick channel
term entry-num old-entry new-entry")

;;; 2005-08-31 T10:56:27-0400 (Wednesday)    D. Goel
(defvar erbot-nickserv-p nil
  "When t, erbot will load the appropriate erc modules and will try to
auto-identify to nickserv.  
   
If using this, we recommend these settings at the *BEGINNING* of your
bot's .emacs: 

     (setq erbot-nickserv-p t)
     (setq erc-prompt-for-nickserv-password nil) 

     (setq erc-nickserv-passwords
          '((freenode     ((\"mybot\" . \"mypassword\")))))

See this page for more details: 
http://www.emacswiki.org/cgi-bin/wiki?ErcNickserv
")

(when erbot-nickserv-p
  (require 'erc-nickserv nil t) ;; old erc 
  (require 'erc-services nil t) ;; erc from emacs22
  (erc-nickserv-mode 1)
  )




;;  erbot-notify-merge-functions
;;    arguments: nick channel old-term new-term new-entries
;; NOW CHANGED SPEC!
(defvar erbot-notify-merge-functions nil
 "Functions to call when an erbot merge operation takes place.
Each of these is called with the arguments arguments: nick channel
from-term to-term from-entries to-entries final-entries")



; This function is used by the example above.
(defun erbot-doctor (args)
  "Glue the doctor into the ERC robot."
  (let* ((thisbuf (current-buffer))
         (dbuf (concat "*doctor: " (buffer-name thisbuf) "*"))
	 (docbuf (get-buffer dbuf))
	 outpoint
	 res)
    (if (not docbuf)
	(progn
          (set-buffer (get-buffer-create dbuf))
          (make-doctor-variables)
          (set-buffer thisbuf)
	  (setq docbuf (get-buffer dbuf))
	  (bury-buffer docbuf)))
    (save-excursion
      (set-buffer docbuf)
      (goto-char (point-max))
      (insert args)
      (goto-char (point-max))
      (setq outpoint (point))
      (doctor-ret-or-read 1)
      (doctor-ret-or-read 1)
      (goto-char outpoint)
      (re-search-forward "^.")
      (setq outpoint (- (point) 1))
      (re-search-forward "^$")
      (erc-replace-regexp-in-string
       "\n" " " (buffer-substring outpoint (point)))
    )))



(defun erbot-dunnet (arg)
  "Glue the dunnet into the ERC robot."
  (save-excursion
    (let ((freshp nil)
	  (dun-batch-mode t)
	  outpoint res ans
	  (pre "")
	  full
	  (dunnet-mode-note
	   ;;"I am in dunnet-mode. For regular bot, type ,(dunnet-mode). For dunnet commands, just type them, or use (dunnet-command...)." )
	   ;; a CYA message. We don't really know if we are in dunnet mode.
	   "I MAY be in dunnet-mode. To toggle, type ,(dunnet-mode). For dunnet commands, just type them when in dunnet-mode, or use (dunnet-command...)." )
	  )
      (when (or (not (boundp 'dun-dead)) dun-dead
		(not (get-buffer "*dungeon*"))
		)
	(setq freshp t)
	(setq dun-dead nil))
      (when freshp (dunnet))
      (set-buffer "*dungeon*")
      ;; switching is important, otherwise dunnet tends to insert its stuff in current buffer! 
      (switch-to-buffer "*dungeon*")
      (goto-char (point-max))
      (when (string-match "save" arg)
	(setq arg "save ~/pub/dunnet/dunnet.game")
	(setq pre "Will save to ~/pub/dunnet/dunnet.game"))
      (cond
       ((string-match "^.?more" arg)
	(setq ans (fsi-more)))
       (t
	(unless freshp (insert arg))
	(goto-char (point-max))
	(setq outpoint (if freshp (point-min) (point)))
	(unless freshp (dun-parse 1))
	(setq ans
	      (buffer-substring-no-properties
	       outpoint (- (point-max) 1)))
	(when (equal arg "quit")
	  (progn (kill-buffer "*dungeon*")))))
      (setq full (concat pre ans))
      (when
	  ;; we could test for dunnet-mode-i-ness here, but that's hard because that's a buffer-local variable.
	  (string-match
	   "I don't understand that"
	   full)
	(setq
	 full
	 (fsi-unfill 
	  (concat full dunnet-mode-note))))
      full)))

(defvar erbot-quiet-p nil
  "When non-nil, the erbot only listens, never replies")

(defun erbot-quiet ()
  (interactive)
  (setq erbot-quiet-p
	(not erbot-quiet-p))
  (message "set to %S" erbot-quiet-p))

(defvar erbot-quiet-target-p-function nil
  "A function.   The function should take up to 3 arguments, TARGET
\(channel) , nick and msg.  If it returns non-nil, then erbot will
listen and do everything but never reply back.")


(defvar erbot-on-new-erc-p nil
  "Whether we use erc >1.660 with new erc-backend.
The value should not be set but is auto-guessed within
`erbot-install'.")


;; A very very main function..
(defun erbot-remote (proc parsed)
  "Implements a simple robot for erc.  Messages to the robot are of the form:
\"nick: !command args\", where:
nick	- the nickname of the user who is the target of the command,
command	- the specific command,
args	- arguments to the command (optional).

For newer erc, see `erbot-on-new-erc-p' and read the specs of
the new erc-backend functions."
  (set-buffer (process-buffer proc))
  (let* (
	 (fsi-limit-lines fsi-limit-lines) ;; so that any temporary modifications do NOT affect the global value
	 (erbn-buffer (erc-server-buffer))
	 (sspec (cond (erbot-on-new-erc-p
		       (erc-response.sender parsed))
		      (t (aref parsed 1))))
	 (userinfo (erc-parse-user sspec))
	 (nick (erbutils-remove-text-properties-maybe (nth 0 userinfo)))
	 ;; bind fs-nick in a let.. so that changes to fs-nick are
	 ;; independent and do not affect each other.. when it is
	 ;; parsing too many messages once..
	 (fs-nick nick)
	 (erbn-nick fs-nick)
	 (cmdargs (and erbot-on-new-erc-p
		       (erc-response.command-args parsed)))
	 (tgta 
	  (erbutils-remove-text-properties-maybe 
	   (cond (cmdargs
		  (nth 0 cmdargs))
		 (t (aref parsed 2)))))
	 (tgt (if (equalp tgta (or (erc-current-nick) erbot-nick))
		  nick
		tgta))
	 (erbn-tgt tgt)
	 (fs-tgt tgt)
	 (msg 
	  (erbutils-remove-text-properties-maybe 
	   (cond (cmdargs
		  (nth 1 cmdargs))
		 (t (aref parsed 3)))))
	 (erbot-end-user-nick nick)
	 (csys     (if (fboundp 'erc-coding-system-for-target)
		       (erc-coding-system-for-target tgt)
		     'utf-8))
	 (code-in  (if (consp csys) (cdr csys)  csys))
	 (code-out (if (consp csys) (car csys)  csys))
	 )
    ;; changing the structure here..
    ;; also changing erbot-command to erbot-reply..
    ;; from now on, erend-main will take care of what to reply..
    ;; erbot-reply will simply take the reply and reply that...
    ;; should not be setq.. else other invocations may change it..
    ;;(setq erbot-end-user-nick nick)

    (setq erbot-end-user-nick-latest erbot-end-user-nick)
    ;;(setq fs-tgt tgt)
    ;;(setq erbn-tgt tgt)

    ;;(setq fs-nick nick)
    ;;(setq erbn-nick nick)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; make sure we decode the raw text we received...
    (unless (multibyte-string-p msg)
      (setq msg (decode-coding-string msg code-in)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (let ((msgg
	   (erbeng-main msg proc nick tgt nil userinfo)))
      ;; erbot-reply needs a correct buffer...
      (set-buffer (process-buffer proc))

      (cond
       (erbot-quiet-p nil)
       ((and erbot-quiet-target-p-function
	     (funcall erbot-quiet-target-p-function tgt nick msg))
	nil)
       (t (erbot-reply
	   msgg
	   proc erbn-nick erbn-tgt msg nil
	   )))

      ))
  nil)


(defun erbot-frob-with-init-string (reply)
  (cond
   ((or (not (stringp reply)) (string= erbot-init-string "")) reply)
   (t
    (with-temp-buffer
      (insert reply)
      (goto-char (point-min))
      (while (re-search-forward "\n" nil t)
	(replace-match
	 (concat "\n" erbot-init-string) nil t))
      (concat erbot-init-string (erbutils-buffer-string))))))

(defvar erbot-init-string ""
  "The basic init string.. should be concated to ALL lines of
replies... right at last.. the values it will hold will look like /msg
foo, and will be set by fs-parse-english, when that function
determines it appropriate..
Currently: we do not use it, since we have found a better way to do
those things..

")

;; this one is probably never used any more... just to make sure,
;; introduced an error command..
;(defun erbot-local (str)
;  "Funnel text typed by the local user to the local robot.  See
;\"erbot-remote\" for details of the command format."
;  (error "foo")
;  (erbot-command erc-process (erc-current-nick) (buffer-name) str t))

(defcustom erbot-reply-p t
  "when nil, don't reply")

(defun erbot-toggle-reply ()
  (interactive)
  (setq erbot-reply-p (not erbot-reply-p))
  (message "erbot-reply-p set to %S" erbot-reply-p)
  )

(defun erbot-reply (main-reply proc from tgt msg locally-generated)
  "Robot worker.  Should do nothing when main-reply is nil or 'noreply
or \"noreply\"

"
  (unless (stringp main-reply)
    (setq main-reply (format "%S" main-reply)))
  (let (
	;;(fsi-limit-lines fsi-limit-lines) ;; so that changing in the body below or the rest of the functinos does NOT change the global value.. which is 8.
	linen
	(me (or (erc-current-nick) erbot-nick))
	;;(if (and erbot-commands
	;;	     (string-match (concat "^" (regexp-quote me)
	;;				   ": !\\([^ ]+\\) ?\\(.*\\)") msg))
	;;					; this is a robot command to me.
	;;	(let* ((cmd (substring msg (match-beginning 1) (match-end 1)))
	;;	       (args (substring msg (match-beginning 2)))
	;;	       (l (assoc cmd erbot-commands))
	;;	       (allowed-users (nth 1 l))
	;;	       (function (nth 2 l))
	;;	       (permitted (or (eq t allowed-users)
	;;			      (and (eq nil allowed-users) locally-generated)
	;;			      (and (stringp allowed-users)
	;;				   (string-match allowed-users
	;;						 (regexp-quote from)))))



	;;(reply (concat from ": " main-reply))
	;; my frobbing of reply..
	(reply
	 (erbot-frob-with-init-string main-reply))


	(rep-buffer (erc-get-buffer tgt proc)))
    ;;(if permitted
    ;;				  (if l
    ;;				      (funcall function args)
    ;;(concat "unknown command: " cmd
    ;;					    ": try \"cmds\""))
    ;;				(concat "no access to command \"" cmd
    ;;					"\" for " from ".")))))
    (erc-log reply)


    (unless
	(or
	 (null erbot-reply-p)
	 (equal main-reply 'noreply)
	 (equal main-reply "noreply"))
      ;; now we are actually gonna reply.
      (save-excursion
	(setq reply (fsi-limit-lines reply))
	(if rep-buffer (set-buffer rep-buffer)
	;;; this alternative reply somehow never gets sent out..
	  ;;(setq reply (concat "msg " from " "
	  ;;		      "No private msgs.. try #testopn"))
	  ;;(set-buffer (erc-get-buffer tgt proc))
	  (progn
	    (ding t)
	    (message "WTF? no rep-buffer? "))
	  )
	       
	(let* ((inhibit-read-only t)
	       (lines (split-string reply "[\n\r]+"))
	       (multiline-p (< 1 (length lines)))
	       p)
	  (mapc
	   (lambda (line)
	     (when (and line
			(not (erbot-safe-p line)))
	       (setq line (erbot-safe-make line)))
	     (goto-char (point-max))
	     (setq p (re-search-backward (erc-prompt)))
	     ;;(insert (erc-format-timestamp) "<" me "> ")
	     (insert ;;(erc-format-timestamp)
	      "<" me "> ")
	     (erc-put-text-property 0 (length line) 'face
				    'erbot-face line)
	     (insert line "\n")
	     (save-excursion
	       (save-match-data
		 (save-restriction
		   (narrow-to-region p (point))
		   (run-hook-with-args 'erc-send-modify-hook)
		   (run-hook-with-args 'erc-send-post-hook))))
	     (set-marker (process-mark erc-process) (point))
	     (set-marker erc-insert-marker (point))
	     (goto-char (point-max))
	     (setq linen (concat line "\n"))
	     ;; fledermaus: I used to force the encoding here, but I now 
	     ;; think that's the wrong thing to do. Hopefully if the data-path 
	     ;; through erc->fsbot->erc is clean, erc will do the right thing 
	     ;; to outbound data.
	     (erc-process-input-line linen nil multiline-p))
	   lines))))))


(defcustom erbot-setf-p nil
  "If you want your bot to allow setf, set this symbol to non-nil at
the beginning of your .emacs")


(defcustom erbot-setf-symbols
  '(caar cadr car cdar cddr cdr eighth elt 
	 first fourth
	 ninth nth
	 nthcdr
	 second
	 seventh sixth
	 subseq substring
	 tenth third)
"Safe symbols for setf...")


      
;;;###autoload
(defun erbot-install ()
  "Run this function AFTER loading all the files..."
  (interactive)
  (erbot-dunnet-install)
  (setq erbot-on-new-erc-p
	(and (boundp 'erc-server-PRIVMSG-functions)
	     (featurep 'erc-backend)))
  (cond (erbot-on-new-erc-p
	 (add-hook 'erc-server-PRIVMSG-functions 'erbot-remote t)
	 ;; Do we need this local command thing...?
	 ;;(add-hook 'erc-send-completed-hook 'erbot-local t)
	 (add-hook 'erc-server-001-functions
		   'erbot-autojoin-channels))
	(t
	 (add-hook 'erc-server-PRIVMSG-hook 'erbot-remote t)
	 ;; Do we need this local command thing...?
	 ;;(add-hook 'erc-send-completed-hook 'erbot-local t)
	 (add-hook 'erc-server-001-hook
		   'erbot-autojoin-channels))
	)
  (erbot-install-symbols)
  (when (and erbot-setf-p (not erbot-paranoid-p))
    (erbot-install-setf))
  ;; A running bot should have these nil, else userfunctions will not
  ;; function right:
  (setq eval-expression-print-length nil)
  (setq eval-expression-print-level nil)
  (setq print-length nil)
  (setq print-level nil)
  )



(defun erbot-install-setf ()
  (interactive)
  (defalias 'fs-setf 'setf)
  (require 'cl)
  (let*
      (
       ;; all possible symbols
       ;;(syms 
       ;;(apropos-internal "" (lambda (a) (get a 'setf-method))))
       (syms erbot-setf-symbols)
       (fssyms 
	(mapcar
	 (lambda (a) (intern (format "fs-%s" a)))
	 syms))
       (fsisyms 
	(mapcar
	 (lambda (a) (intern (format "fsi-%s" a)))
	 syms)))
    (mapcar*
     (lambda (a b c) 
       (let ((foo (get a 'setf-method)))
	 (when (fboundp b) (put b 'setf-method foo))
	 (when (fboundp c) (put c 'setf-method foo))))
     syms fssyms fsisyms)))




(defun erbot-install-symbols ()
  "By now, you should have loaded all pertinent erbot files... If you
add any new functions, don't forget to run (erbot-install) AFTER
that.."
  (interactive)
  (let ((ss (fsi-command-list-readonly)))
    (dolist (s ss)
      
      (if (symbolp s)
	  (let ((f-s (erbutils-concat-symbols 'fs- s))
		(fi-s (erbutils-concat-symbols 'fsi- s)))
	    
	    (defalias f-s fi-s)
	    (put f-s 'readonly t))
	(message "Ignoring fsi->fs for %s" s)))))


  

;;;###autoload
(defun erbot-autojoin-channels (server nick)
  ;;(interactive)
  (dolist (l erbot-servers-channels)
    (when (string-match (car l) (process-name server))
      (dolist (chan (cadr l))
        (erc-send-command (concat "join " chan))))))



(defun erbot-get-servers ()
  (mapcar '(lambda (arg) (list (car arg) (caddr arg)))
	  erbot-servers-channels))


;;;###autoload
(defun erbot-alive-p ()
  "Is atleast one connection still alive?"
  ;;(require 'cl-extra)
  (some
   'identity
   (mapcar
    (lambda (buf)
      (save-excursion
	(set-buffer buf)
	(erc-process-alive)))
    (erc-buffer-list))))

(defvar erbot-reconnection-attempts nil)

;;;###autoload
(defun erbot-keep-alive (&rest args)
  "Periodically check if atleast one connection is still alive.  If
not, try to reconnect. "
  (require 'idledo)
  (idledo-add-periodic-action-crude
   '(unless (erbot-alive-p)
      (add-to-list 'erbot-reconnection-attempts
		   (message "Erbot trying to reconnect at %s"
			    (format-time-string
			     "%Y%m%d-%H%M-%S")))
      (ignore-errors (apply 'erbot-join-servers args)))))

;;;###autoload
(defun erbot-join-servers (&optional server port nick
				   user-full-name
				   not-connect-arg passwd)
  "Try to never join if already joined..."
  (interactive)
  (require 'erc)
  (if (null server)
      (mapcar
       '(lambda (arg)
	  (erbot-join-servers
	   (car arg) (cadr arg) nick user-full-name not-connect-arg passwd)
	  (sit-for 1)
	  )

       ;; get the list of servers
       (erbot-get-servers)

       )
    (progn
      ;;(if (null server)
      ;;	  (setq server erc-server))
      ;; 2002-08-21 T11:22:35-0400 (Wednesday)    D. Goel
      (setq erc-current-server-my server)
      (if (null port) 
	  (setq port 
		(if (fboundp 'erc-compute-port)
		    (erc-compute-port)
		  erc-port)))
      (setq nick (or erbot-nick (erc-compute-nick nick)))
      (let* (
	     (foo 'bar)
             (version nil)
	     ;(nick
	     ; (if (erc-already-logged-in server port nick)
	     ;;	  (read-from-minibuffer
	     ;;	   (erc-format-message 'nick-in-use ?n nick)
	     ;;	   nick
	     ;;	   nil nil 'erc-nick-history-list)
	     ;;	nick)))
	     )
	(if (and passwd (string= "" passwd))
	    (setq passwd nil))
	;; 	(while (erc-already-logged-in server port nick)
	;; 	  (setq nick (read-from-minibuffer
	;; 		      (erc-format-message 'nick-in-use ?n nick)
	;; 		      nick
	;; 		      nil nil 'erc-nick-history-list)))

	(run-hook-with-args 'erc-before-connect server port nick)
	(if (string-match "\\(\\<[[:digit:]]+.[[:digit:]]+\\>\\)" 
			  erc-version-string)
	    (setq version (string-to-number 
			   (match-string 1 erc-version-string)))
	  (setq version 0))

	(unless (erc-already-logged-in server port nick)
	  (if (<= 5.0 version)
	      (erc :server    server 
		   :port      port 
		   :nick      nick 
		   :password  passwd
		   :full-name user-full-name)
	  (erc
	   server port nick user-full-name (not not-connect-arg) passwd) )) 
	))))


(defun erbot-safe-make (line)
  (let* ((ans line)
	 (rlist (string-to-list line)))
    (when (string-match "^/" line)
      (unless (string-match "^/me " line)
	(setq ans (concat " " line))))
    (when (member-if (lambda (a) 
		       (and (< a 32)
			    (not (= a 9))))
		     rlist)
      (setq ans "<control characters>"))
    (when (string-match "[\n\r]" line)
      (setq ans " <newlines> "))
    ans))



    

(defun erbot-safe-p (reply)
  "Determine whether a reply is safe.  Any newlines are simply
reported as unsafe.

If this functions deems a reply as unsafe, you should not send it to
ERC but call `erbot-safe-make' first. "
  (and
   (not (string-match "[\n\r]" reply))
    ;; err on the side of caution.  Demand that the 1st char. be VERY
   ;; safe.  
   (or
    (string-match "^[0-9a-zA-Z]" reply)
    ;;(not (string-match "^/" reply)) -- this is bad.. since, control
    ;;characters are bad... beginnning ^A for example, will send CTCP requests..
    
    ;; Allow /me commands.. but only when the rest of the text has no
    ;; control characters..
    (equal 0 (string-match "^/me " reply)))
   ;; And there be no control characters whatsoever anywhere.
   (erbot-safe-nocontrol-p reply)))

(defun erbot-safe-nocontrol-p (reply)
  (let ((rlist (string-to-list reply)))
    (not (member-if (lambda (a) (< a 32)) rlist))))






(defun erbot-dunnet-install ()
  "Defines some dunnet specific aliases. "
  (interactive)
  (require 'dunnet)
  ;; (defalias 'dun-read-line 'fs-botread)
  (defun dun-type (&rest args) (dun-mprinc "You tried to type through a bot.") (dun-die "bot"))
  ;;(defalias 'dun-mprinc
  ;;'fs-dun-mprinc))
  )



(defmacro erbot-working (&rest args)
  `(let ((erbbdb-save-p nil)
	 (erbot-notify-p nil))
     ,@args))



(provide 'erbot)
(run-hooks 'erbot-after-load-hooks)



;;; erbot.el ends here
