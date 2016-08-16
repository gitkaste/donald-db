


(defun fs-tmpsp1
    (&rest fs-_)
  (erblisp-check-args &rest fs-_)
  (sit-for 0)
  (fs-message "have you tried turning it off and back on again?"))



(defun fs-tmpsp
    (&rest fs-args)
  (erblisp-check-args &rest fs-args)
  (sit-for 0)
  (fs-msg fs-args))



(defun fs-faq
    (&rest fs-args)
  (erblisp-check-args &rest fs-args)
  (sit-for 0)
  (fs-sit-for 0)
  (fs-if
      (fs-null fs-args)
      "List of TeX Frequently Asked Questions: http://www.tex.ac.uk/cgi-bin/texfaq2html"
    (fs-let
	((fs-s
	  (fs-car fs-args)))
      (fs-format "See TeX FAQ entry http://www.tex.ac.uk/cgi-bin/texfaq2html?label=%s" fs-s))))



(defun fs-cs
    (&rest fs-args)
  (erblisp-check-args &rest fs-args)
  (sit-for 0)
  (fs-google-with-options "site:ctan.org" fs-args))



(defun fs-ctan
    (&rest fs-args)
  (erblisp-check-args &rest fs-args)
  (sit-for 0)
  (fs-if fs-args
      (fs-format "Documentation and source code of the %s package can be found at http://www.ctan.org/pkg/%s"
		 (fs-car fs-args)
		 (fs-car fs-args))
    "The Comprehensive TeX Archive Network at http://www.ctan.org/ is the place to get materials related to the TeX typesetting system"))



(defun fs-excuse nil
  (erblisp-check-args)
  (sit-for 0)
  (fs-technobabble-excuse))



