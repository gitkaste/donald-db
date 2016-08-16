
(defun easter-date (&optional YEAR)
  "Called interactively, show the date of easter in YEAR, or of the next
easter if YEAR is not supplied. Called non-interactively, return the same
datum as an iso-formatted date \(yyyy-mm-dd\)"
(interactive
   (let ((now (current-time))
	 (ety (easter-time))
	 (eyr nil)
	 (msg nil)
	 (iyr nil))
     (setq eyr (if (or (> (car  now) (car  ety))
		       (and (=  (car  now) (car  ety))
			    (> (cadr now) (cadr ety))))
		   (+ (elt (decode-time ety) 5) 1)
		 (elt (decode-time ety) 5)))
     (setq msg "Year: ")
     (setq iyr (read-string msg nil nil nil t))
     (if (string= iyr "") nil (list (string-to-int iyr)))))
  (let ((time nil))
    (setq time (decode-time (easter-time YEAR)))
    (setq time (format "%4.4d-%2.2d-%2.2d"
(nth 5 time)
		       (nth 4 time)
		       (nth 3 time)))
    (if (interactive-p) (message "Easter is on %s" time) time)))

(defun easter-time (&optional YEAR)
  "easter-time returns the date of easter sunday for a given year,
based on the algorithm found at http://www.davros.org/misc/easter.html :

The actual value returned is of the type understood by `encode-time'
and `decode-time'.

+---------------------------------------------------------------------------+
|          Divide          |  to get  |             Explanation             |
|--------------------------+----------|                                     |
|         this         |by |quot.|rem.|                                     |
|----------------------+---+-----+----+-------------------------------------|
|year                  | 19|     | a  |a + 1 is the golden number.          |
|----------------------+---+-----+----+-------------------------------------|
|year                  |100|  b  | c  |Split the year into century and      |
|                      |   |     |    |remnant.                             |
|----------------------+---+-----+----+-------------------------------------|
|b                     | 4 |  d  | e  |Find the place in, and the number of,|
|                      |   |     |    |400 year cycles. 400 years is an     |
|                      |   |     |    |exact number of weeks.               |
|----------------------+---+-----+----+-------------------------------------|
|c                     | 4 |  f  | g  |Find the number of leap years so far |
|                      |   |     |    |this century (ignoring the century   |
|                      |   |     |    |year if it was one), and the number  |
|                      |   |     |    |of ordinary years since.             |
|----------------------+---+-----+----+-------------------------------------|
|8 * b + 13            | 25|  h  |    |Determine the number of days to shift|
|                      |   |     |    |the full moons because of the lunar  |
|                      |   |     |    |correction. The 8 ensures that we get|
|                      |   |     |    |a total of 8 days every 2500 years,  |
|                      |   |     |    |and the 13 ensures that we start at  |
|                      |   |     |    |the right point.                     |
|----------------------+---+-----+----+-------------------------------------|
|19 * a + b - d        | 30|     | j  |j encodes the unadjusted date of the |
| - h + 15             |   |     |    |Paschal full moon. b - d is the      |
|                      |   |     |    |number of century years that are not |
|                      |   |     |    |leap years, and so b - d - h is 7    |
|                      |   |     |    |more than the number found from Table|
|                      |   |     |    |II. Meanwhile 19 * a gives the       |
|                      |   |     |    |position of a specific number (8) in |
|                      |   |     |    |the appropriate column of Table III, |
|                      |   |     |    |and the constant 15 corrects for the |
|                      |   |     |    |two offsets 7 and 8.                 |
|----------------------+---+-----+----+-------------------------------------|
|a + 11 * j            |319|  m  |    |m will be 1 if the full moon needs to|
|                      |   |     |    |be adjusted back one day, and 0      |
|                      |   |     |    |otherwise.                           |
|----------------------+---+-----+----+-------------------------------------|
|2 * e + 2 * f - g     | 7 |     | k  |k + 1 is the number of days from the |
| - j + m + 32         |   |     |    |Paschal full moon to Easter          |
|                      |   |     |    |Sunday. The best way to see how it is|
|                      |   |     |    |derived is to first note that the    |
|                      |   |     |    |days of the week repeat exactly every|
|                      |   |     |    |400 years, and then to rewrite it as:|
|                      |   |     |    |4 - [124 * e + 5 * f + g] - (j - m) +|
|                      |   |     |    |7 * 92                               |
|                      |   |     |    |The term in square brackets          |
|                      |   |     |    |represents the day of the week for   |
|                      |   |     |    |March 21st, the third term advances  |
|                      |   |     |    |this to the Paschal full moon, the   |
|                      |   |     |    |constant 4 represents Saturday, and  |
|                      |   |     |    |the last term makes the result       |
|                      |   |     |    |positive. Then reduce as much as     |
|                      |   |     |    |possible modulo 7.                   |
|----------------------+---+-----+----+-------------------------------------|
|j - m + k + 90        |25 |month|    |We now have j - m + k representing   |
|                      |   |     |    |the date of Easter Sunday, with 0    |
|                      |   |     |    |being March 22nd; the constant 90    |
|                      |   |     |    |derives from 10 representing April   |
|                      |   |     |    |1st.                                 |
|----------------------+---+-----+----+-------------------------------------|
|j - m + k + 19 + month| 32|     |date|And finally we get the date in a     |
|                      |   |     |    |similar way; adding in the month     |
|                      |   |     |    |allows us to skip April 0th          |
|                      |   |     |    |seamlessly.                          |
+---------------------------------------------------------------------------+

If no year is supplied, calculates the date for easter for this year.
"
(let* ((Y (if YEAR YEAR (elt (decode-time) 5)))
	 (a (% Y  19))
	 (b (/ Y 100))
	 (c (% Y 100))
	 (d (/ b   4))
	 (e (% b   4))
	 (f (/ c   4))
	 (g (% c   4))
	 (h (/ (+ (* 8 b) 13) 25))
	 (j (% (- (+ b 15 (* a 19)) d h) 30))
	 (m (/ (+ a (* j 11)) 319))
	 (k (% (- (+ e e f f m 32) g j) 7))
	 (M (/ (- (+ j k 90) m) 25))
	 (D (% (- (+ j k 19 M) m) 32)))
    (encode-time 0 0 0 D M Y)) )

(defun fs-easter (&optional year) 
  (easter-date (if year (symbol-name year))))

(provide 'easter)