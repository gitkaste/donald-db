(require 'time-date)

;; the standard labels we use for the x/y co-ordinates of each cell
(defconst sd-cols   '(a b c d e f g h i))
(defconst sd-rows   '(1 2 3 4 5 6 7 8 9))

;; the column and row groups that go together to make 3x3 boxes
(defconst sd-box    '((a b c) (d e f) (g h i) (1 2 3) (4 5 6) (7 8 9)))

;; the possible values in each cell:
(defconst sd-values '(1 2 3 4 5 6 7 8 9))

;; construct and cache all possible (col . row) coordinates for our grid
(defconst sd-squares
  (apply 'nconc
         (mapcar (lambda (c) 
                   (mapcar (lambda (r)
                             (cons c r)) sd-rows)) sd-cols)))

(defvar sd-inference-start  nil)
(defvar sd-inference-finish nil)
(defvar sd-search-start     nil)
(defvar sd-search-finish    nil)
(defvar sd-search-limit    1200)

;; construct an empty puzzle (all cells completely unconstrained)
(defun sd-empty-puzzle ()
  (mapcar (lambda (s) (cons s (copy-sequence sd-values))) sd-squares))

;; list of cells in a given column
(defun sd-col-unit (col)
  (mapcar (lambda (r) (cons col r)) sd-rows))

;; list of cells in a given row
(defun sd-row-unit (row)
  (mapcar (lambda (c) (cons c row)) sd-cols))

;; list of cells in a 3x3 box, specified by col & row of any cell in said box
(defun sd-box-unit (col row)
  (let (box-row box-col)
    (mapc (lambda (set) (if (memq col set) (setq box-col set))) sd-box)
    (mapc (lambda (set) (if (memq row set) (setq box-row set))) sd-box)
    (apply 'nconc
           (mapcar
            (lambda (c)
              (mapcar (lambda (r) (cons c r)) box-row)) box-col)) ))

;; list cells which are within the constraint-set of a given cell
(defun sd-peers (square)
  (let ((col (car square))
        (row (cdr square)) box-col box-row)
    (delete-dups (delete square (nconc (sd-row-unit row)
                                       (sd-col-unit col)
                                       (sd-box-unit col row)) )) ))

;; is the grid completely defined? (all cells have only 1 candidate)
(defun sd-finished (grid)
  (catch :no
    (mapc (lambda (s) (if (> (length (cdr s)) 1) (throw :no nil))) grid) t))

;; remove a value from the candicate list for each of a cell's peers:
(defun sd-constrain (grid square value)
  (mapc (lambda (s) (sd-eliminate grid s value)) (sd-peers square)))

;; set the possible value list of a cell to a single number,
;; and remove that number from the possible values of all its peers.
(defun sd-set (grid square value)
  (let ((cell (assoc square grid)) values)
    (setq values (cdr cell))
    (setcdr cell (list value))
    (sd-constrain grid square value) ))

;; eliminate value from the possibilities of a given square: grid is
;; the overall data structure, square is just the (c . r) coordinate
;; if there is only 1 candidate left, claim it for this cell alone
;; if there are no possible values left, we've tried an impossible 
;; solution, signal this:
;; if we made a change to the cell, start the inference engine:
(defun sd-eliminate (grid square value)
  (let ((svals (cdr (assoc square grid))))
    (when (memq value svals)
      (setq svals (delq value svals))
      (if (cdr svals)
          (setcdr (assoc square grid) svals)
        (if svals (sd-set grid square (car svals)) 
          (throw :sd-invalid nil))) 
      (sd-infer grid square) )))

;; iterate over each of the row-peer, column-peer and box-peer lists
;; in turn. within each list (aka group), count the number of cells
;; which _could_ contain each possible sudoku value. If only one
;; cell in a group can contain a value (based on our current grid),
;; claim the value for that cell.
(defun sd-infer (grid square)
  ;;(message "sd-infer %S" square)
  (let ((groups (list (sd-row-unit (cdr square))
                      (sd-col-unit (car square))
                      (sd-box-unit (car square) (cdr square)))))
    (mapc
     (lambda (value)
       (mapc
        (lambda (group)
          (let ((count 0) (where nil) possible)
            (mapc (lambda (cell)
                    (setq possible (cdr (assoc cell grid)))
                    (when (memq value possible)
                      (setq count (1+ count) where cell))) group)
            (when (= count 1)
              ;; (message "inference: %d only in %S [ %S ]" value where group)
              (sd-set grid where value)) )) groups)) sd-values)))

(defun sd-parse-buffer-internal ()
  (setq sd-inference-start (current-time))
  (goto-char (point-min))
  (let ((grid (sd-empty-puzzle)) (squares sd-squares) square val)
    (while (re-search-forward "\\([-0-9.]\\)" nil t)
      (setq square  (car squares)
            squares (cdr squares)
            val     (string-to-number (match-string 1)))
      (when (> val 0) (sd-set grid square val)))
    (setq sd-inference-finish (current-time))
    grid))

(defun sd-parse-buffer (buffer)
  (with-current-buffer buffer (sd-parse-buffer-internal)))

(defun sd-parse-string (string)
  (with-temp-buffer (insert string) (sd-parse-buffer-internal)))

(defun sd-most-constrained-square (grid)
  (let ((least (1+ (length sd-values))) square)
    (mapc (lambda (s &optional l)
            (when (and (< (setq l (length (cdr (assoc s grid)))) least) (> l 1))
              (setq square s least l))) sd-squares)
    square))

(defun sd-clone-grid (grid)
  (mapcar (lambda (x) (cons (car x) (copy-sequence (cdr x)))) grid))

(defvar sd-search-squares nil)
(defvar sd-search-opcount 0)

;; take a constrained grid, and for each cell which is unresolved
;; (ie that has multiple candidate values) try each value on a cloned
;; copy of the grid.
;; If a candidate produces a viable grid (it may not be fully constrained,
;; but its values do not contradict the existing constraints), recurse
;; into that grid with the same seach function:
;; if the grid is invalid, reset to the previous level of search and 
;; proceed to the next possible value:
(defun sd-search (grid &optional level)
  (when (not level)
    (setq level             0
          sd-search-start   (current-time)
          sd-search-opcount 0
          sd-search-squares nil))
  (sit-for 0)
  (setq sd-search-finish (current-time))
  (if (sd-finished grid)
      grid
    (let ((target (sd-most-constrained-square grid)) result values)
      (when (not (member target sd-search-squares))
        (setq sd-search-squares (cons target sd-search-squares)))
      (setq values (cdr (assoc target grid)))
      (mapc 
       (lambda (v)
         (let ((probe (sd-clone-grid grid)))
           (setq sd-search-opcount (1+ sd-search-opcount))
           (when (<= sd-search-limit sd-search-opcount)
             (error "Sudoku search depth %d exceeded at %0.2f seconds"
                    sd-search-opcount (sd-elapsed)))
           (setq result (or (catch :sd-invalid
                              (sd-set probe target v)
                              (sd-search probe (1+ level))) result)) )) values)
      result)))

;; take a sudoku grid and apply constraint propagation to it.
;; once the grid is constrained, search it (if necessary) until
;; it is completely solved
(defun sd-solve (src)
  (let (grid buf)
    (setq grid (cond ((setq buf (get-buffer src)) (sd-parse-buffer buf))
                     ((stringp src)               (sd-parse-string src)))
          grid (sd-search grid))
    grid))

(defun sd-elapsed (&optional category)
  (cond ((eq category 'inference) 
         (if (and sd-inference-start sd-inference-finish)
             (float-time (time-subtract sd-inference-finish sd-inference-start))
           0.0))
        ((eq category 'search) 
         (if (and sd-search-start sd-search-finish)
             (float-time (time-subtract sd-search-finish sd-search-start))
           0.0))
        (t (+ (sd-elapsed 'inference) (sd-elapsed 'search))) ))

(defconst sd-line-format
  (concat "%s%s%s %s%s%s %s%s%s" " "
          "%s%s%s %s%s%s %s%s%s" " "
          "%s%s%s %s%s%s %s%s%s" " "
          "%s%s%s %s%s%s %s%s%s" " "
          "%s%s%s %s%s%s %s%s%s" " "
          "%s%s%s %s%s%s %s%s%s" " "
          "%s%s%s %s%s%s %s%s%s" " "
          "%s%s%s %s%s%s %s%s%s" " "
          "%s%s%s %s%s%s %s%s%s" ))

(defconst sd-grid-format
  (concat "%s %s %s | %s %s %s | %s %s %s\n"
          "%s %s %s | %s %s %s | %s %s %s\n"
          "%s %s %s | %s %s %s | %s %s %s\n"
          "------+-------+------\n"
          "%s %s %s | %s %s %s | %s %s %s\n"
          "%s %s %s | %s %s %s | %s %s %s\n"
          "%s %s %s | %s %s %s | %s %s %s\n"
          "------+-------+------\n"
          "%s %s %s | %s %s %s | %s %s %s\n"
          "%s %s %s | %s %s %s | %s %s %s\n"
          "%s %s %s | %s %s %s | %s %s %s\n"))

(defun sd-string (x)
  (if (not (cdr x))
      (format "     %d     " (car x))
    (let (p)
      (setq x (concat "(" (mapconcat 'number-to-string x "") ")")
            p (make-string (/ (- 11 (length x)) 2) ?\ )
            x (concat p x p))
      (format "%11s" x) )))

(defun sd-format (grid &optional brief)
  (let ((ffunc nil))
    (setq ffunc 
          (if (sd-finished grid)
              (lambda (c) (format "%d" (cadr c)))
            (lambda (c) (sd-string (cdr c)))))
    (apply 'format (if brief sd-line-format sd-grid-format)
           (mapcar ffunc grid)) ))

(provide 'sudoku)
