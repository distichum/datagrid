;;; datagrid.el --- Functions for a vector of vectors data structure. -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Joshua Lambert

;; Author: Joshua Lambert <jlambert@missouristate.edu>
;; Maintainer: Joshua Lambert <jlambert@missouristate.edu>
;; Created: 2025-03-31
;; Version: 0.1
;; Keywords: tools, convenience, sorting
;; URL: https://github.com/distichum/datagrid
;; Package-Requires: (csv-mode)

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; datagrid.el is a list of functions to handle table-like structures
;; in Emacs Lisp. The structures must be made of one vector, the
;; parent, where the elements inside of it, the children, are other
;; vectors. The child vectors _must_ be of equal length. Each child
;; vector starts with a header. Think of it as tabular data, a
;; spreadsheet/database table or a simplified dataframe. The purpose
;; of datagrid.el to manipulate and do calculations on small to medium
;; sized data sets.

;;           ╔═════════════════════════════════╗
;;           ║ Data grid: A vector of vectors  ║
;;           ╚═════════════════════════════════╝

;;       ------------------------------------------
;;       |     Vector_1   Vector_2   Vector_3     |
;;       |   ┌──────────┬──────────┬──────────┐   |
;;    V  |   │ Header1  │ Header2  │ Header3  │   |
;;    e  |   │ Element1 │ Element1 │ Element1 │   |
;;    c  |   │ Element2 │ Element2 │ Element2 │   |
;;    t  |   │ Element3 │ Element3 │ Element3 │   |
;;    o  |   │   ...    │   ...    │   ...    │   |
;;    r  |   │ ElementN │ ElementN │ ElementN │   |
;;       |   └──────────┴──────────┴──────────┘   |
;;       ------------------------------------------
;;     Elisp representation:
;;     [["Header1" "Element1" "Element2" "Element3"]
;;      ["Header2" "Element1" "Element2" "Element3"]
;;      ["Header3" "Element1" "Element2" "Element3"]]

;;; Code
;;;; Variables

(defvar datagrid-test-alist '(("date" . ("2025-03-31" "2025-04-01" "2025-04-02" "2025-04-03"))
			      ("location" . ("somewhere" "out" "there" "far"))
			      ("precipitation" . (0.5 0 .25 1))
			      ("high-temp" . (15 20 32 22))))

;;;; Making and changing data grids.
(defun datagrid-from-alist (alist &optional extend-uneven)
  "Create a data grid from an ALIST.
The keys of the alist are the first element of each second
dimension vector. This is usually the place for the heading. The
values are the rest of the column vector. If EXTEND-UNEVEN is t
then vectors will be padded until all are equal in length. If
nil, then only the first X items of the alist will be placed into
a vector. X is the length of the shortest list value."
  (interactive)
  (let* ((values (mapcar #'cdr alist))
         (min-length (if extend-uneven
                         (1+ (apply #'max (mapcar #'length values)))
		       (1+ (apply #'min (mapcar #'length values)))))
         result)
    (cl-loop for item in alist
	     if extend-uneven vconcat
	     (vector (vconcat (append item (make-list (max 0 (- min-length (length item))) nil))))
	     else vconcat
	     (vector (vconcat (seq-take item min-length))))))

(defun datagrid-make (1d-length 2d-length &optional init heading-list)
  "Create a new array with specified dimensions.
Return a newly created vector of vectors where 1D-LENGTH is the
first dimension length, that is the number of columns. The
2D-LENGTH is the second dimension length. HEADING-LIST is a list
of length 1D-LENGTH. Each item in the list is a string. INIT is
the value of each element in each vector, other than the
headings."
  (interactive)
  (let ((headings (or heading-list (make-list 1d-length nil)))
	parent-vector)
    (cl-loop for elt from 1 to 1d-length
	     for init-list = (make-list (1- 2d-length) init)
	     collect (vconcat (append (list (pop headings)) init-list))
	     into parent-vector
	     finally return (vconcat parent-vector))))


(defun datagrid-get-elt (aref-1d aref-2d datagrid)
  "Get a value at a specific column and row.
DATAGRID is the vector of vectors. AREF-1D is the parent vector place
element number. That would be the column number with zero based
counting. AREF-2D is the child vector element. Think of it as the
row number. The first row is row is 0 and holds the heading."
  (interactive)
  (aref (aref datagrid aref-1d) aref-2d))

(defun datagrid-get-column (index datagrid)
  "Extract a column vector from DATAGRID at INDEX.
DATAGRID is a vector of vectors. INDEX is the column to extract,
with zero based counting. Returns a new vector containing all
elements from the specified column, including the heading."
  (interactive)
  (aref datagrid index))

(defun datagrid-get-row (aref-2d datagrid)
  "Extract an entire row from DATAGRID.
DATAGRID is a vector of vectors. ROW-IDX is the index of the row to
extract, with zero based counting. Row 0 contains the headings.
Returns a new vector containing all elements from the specified
row."
  (interactive)
  (vconcat (seq-map (lambda (vec) (aref vec aref-2d)) datagrid)))

(defun datagrid-dimensions (datagrid)
  "Return the two dimensions of DATAGRID.
DATAGRID is a vector of vectors. Returns a cons cell (1D . 2D), where
1D is the first dimension length (number of columns) and 2D is
the second dimension length (number of rows)."
  (interactive)
  (cons (length datagrid) (length (aref datagrid 0))))

(defun datagridp (datagrid)
  "Check if DATAGRID is a valid vector of vectors (datagrid).
Returns non-nil if OBJ is a vector whose elements are all vectors
of equal length. Returns nil otherwise."
  (and (vectorp datagrid)
       (> (length datagrid) 0)
       (vectorp (aref datagrid 0))
       (let ((expected-length (length (aref datagrid 0)))
             (valid t)
             (i 0))
         (while (and valid (< i (length datagrid)))
           (let ((item (aref datagrid i)))
             (unless (and (vectorp item)
                          (= (length item) expected-length))
               (setq valid nil)))
           (setq i (1+ i)))
         valid)))

(defun datagrid-p2 (datagrid)
  "Check if DATAGRID is a valid vector of vectors (datagrid).
Returns non-nil if OBJ is a vector whose elements are all vectors
of equal length. Returns nil otherwise.

TODO: Benchmark this compared to the other one sometime."
  (and (vectorp datagrid)
       (> (length datagrid) 0)
       (vectorp (aref datagrid 0))
       (seq-every-p #'vectorp datagrid)
       (seq-every-p #'= (seq-map #'length datagrid))))

;;;; Manipulation of data grids
(defun datagrid-add-columns (seq datagrid)
  "Add one or more columns.
DATAGRID is the vector of vectors. SEQ is one or more sequences that
can be changed into a vector."
  (interactive)
  (when (and (datagridp datagrid)
	     (= (length (aref datagrid 0)) (length seq)))
    (vconcat datagrid (vector seq))))

(defun datagrid-add-row (seq datagrid)
  "Add one row with values.
DATAGRID is the vector of vectors. SEQ is one or more sequences that
can be changed into a vector."
  (interactive)
  (when (and (datagridp datagrid)
	     (= (length datagrid) (length seq)))
    (cl-loop for vec across datagrid
	     ;; vconcat will guarantee that this is a vector. It uses
	     ;; a little more memory but requires less logic.
	     for elt across (vconcat seq)
	     vconcat (vector (vconcat vec (list elt))))))

(defun datagrid-remove-column (index datagrid)
  "Remove the column at INDEX.
DATAGRID is the vector of vectors. INDEX is the column to remove. It
is zero based counting."
  (interactive)
  (when (datagridp datagrid)
    (vconcat (seq-take datagrid index)
	     (seq-drop datagrid (1+ index)))))

(defun datagrid-remove-row (index datagrid)
  "Remove the row and INDEX.
DATAGRID is the vector of vectors."
  (interactive)
  (cl-loop for elt across datagrid
	   vconcat (vector (vconcat (seq-take elt index)
				    (seq-drop elt (1+ index))))))

(defun datagrid-transpose (datagrid)
  "Transpose rows and columns.
DATAGRID is the vector of vectors."
  (interactive)
  (seq-map #'vector (nreverse datagrid)))

(defun datagrid-sort (index datagrid)
  "Sort a datagrid by a specific column.
DATAGRID is a vector of vectors. INDEX is the zero based index of
the column to sort by. Created with the help of Claude.ai."
  (let* ((sort-column (aref datagrid index))
         (col-length (length sort-column))
         ;; Create indices and pair with values from sort column
         (indexed-vals (vconcat (cl-loop for i from 0 below col-length
					 collect (cons i (aref sort-column i)))))
         ;; Sort the pairs by the values
         (sorted-pairs (seq-sort-by #'cdr #'string< indexed-vals))
         ;; Extract the new order of indices
         (new-indices (vconcat (mapcar #'car sorted-pairs)))
         ;; Create the new datagrid by rearranging according to new indices
         (result (make-vector (length datagrid) nil)))
    ;; Reorder each column according to the sorted indices
    (dotimes (col-idx (length datagrid))
      (let* ((col (aref datagrid col-idx))
             (new-col (make-vector col-length nil)))
        (dotimes (i col-length)
          (setf (aref new-col i) (aref col (aref new-indices i))))
        (setf (aref result col-idx) new-col)))
    result))

;;;; Filters and masks
(defun datagrid-create-mask (pred index datagrid)
  "Create a mask for a DATAGRID column at INDEX.
PRED is a function of one argument. It will operate on the
datagrid column/vector at INDEX. INDEX is zero based.

See the Info Manual for builtin predicates, equality functions,
and other useful functions to make predicate functions:

2.7 Type Predicates
2.8 Equality Predicates
3.3 Type Predicates for Numbers
3.4 Comparison of Numbers
4.2 Predicates for Strings
4.5 Comparison of Characters and Strings
5.2 Predicates on Lists
32.2 Predicates on Markers

Other common predicate function examples using lambdas:

 (lambda (x) (= x 15))
 (lambda (x) (eq x 15))
 (lambda (x) (equal x 15))
 (lambda (x) (string-equal x \"2025-03-31\"))
 (lambda (x) (string-prefix-p x \"2025\"))
 (lambda (x) (string-prefix-p x \"2025\"))
 (lambda (x) (> x \"2025\"))
 (lambda (x) (<= x \"2025\"))"
  (interactive)
  (let* ((vec (elt datagrid index))
	 (mask (and (vectorp datagrid)
		    (> (length datagrid) 0)
		    (vectorp vec)
		    (seq-map pred vec))))
    ;; Discard the first item from mask and replace it with t. It is
    ;; the heading and should always be t.
    (vconcat (cons t (cdr mask)))))

(defun datagrid-filter-vector-by-mask (vector mask)
  "Filter a vector by boolean MASK.
The vector should include the heading and the mask should have a
t in the first place. It returns a filtered vector."
  (let ((index 0))
    (vconcat
     (seq-filter (lambda (elmt)
		   (prog1
		       (and (aref mask index)
			    elmt)
		     (setq index (1+ index))))
		 vector))))

(defun datagrid-filter-by-mask (datagrid mask)
  "Use a boolean MASK to filter DATAGRID.
MASK must be a vector with the same length as datagrid columns
and values must only be t or nil. The first element in the vector
should be t since that is the heading and the mask should never
remove headings. The functions returns a datagrid."
  (vconcat
   (cl-loop for vec across datagrid
	    collect (datagrid-filter-vector-by-mask vec mask))))


(defun datagrid-group-by (datagrid col-values)
  "Group data in DATAGRID according to COL-VALUES.
The resulting structure is a 3D vector. The first dimension is a vector
of groups which were formed by grouping rows of the datagrid by unique
values in COL-VALUES. The 2nd dimension vector contains the original
datagrid vectors filtered for only that group. The 3rd dimension vector
contains the data from one column for one group. REWORD."
  (cl-loop for item in (datagrid-column-unique col-values datagrid)
	   vconcat (vector
		    item
		    (datagrid-filter-by-mask
  		     datagrid
  		     (datagrid-create-mask (lambda (x) (string-equal item x))
					   col-values datagrid)))))

;;;; Creating data grids from other data.
(defun datagrid-from-csv (buff)
  "Return a datagrid from an open csv buffer.
BUFF is the buffer name. If no buffer is given, use the current
buffer.

REQUIRES: csv-mode"
  (interactive)
  (save-current-buffer
    (set-buffer buff)
    (save-excursion
      (let ((data nil))
	(beginning-of-buffer)
	(while (not (eobp))
	  ;; Build a list of lists of row data. Building a list using
	  ;; cons and then reversing it and turning it into a vector is
	  ;; faster than repetitively vconcating vectors, I assume,
	  ;; because cons doesn't have to recreate the whole list each
	  ;; time while vconcat does.
	  (setq data (cons (csv-parse-current-row) data))
	  (forward-line))
	(vconcat (apply #'cl-mapcar #'vector (nreverse data)))))))

;;;; Data analysis
(defun datagrid-reduce-vec (function index datagrid &optional init)
  "Reduce the FUNCTION across DATAGRID at INDEX.
Return the result of calling FUNCTION with INIT and the first
element of the indexed vector in DATAGRID, then calling FUNCTION
with that result and the second element of that vector, then with
that result and the third element of the vector, etc. FUNCTION
will be called with INIT (and then the accumulated value) as the
first argument, and the elements from the vector as the second
argument. If DATAGRID is empty, return INIT and FUNCTION is not
called.

This function and documentation string are derived from
SEQ-REDUCE."
  (interactive)
  (unless (datagridp datagrid)
    (error "Argument must be a datagrid."))
  (let ((vec (seq-subseq (aref datagrid index) 1)))
    (seq-reduce function vec 0)))

(defun datagrid-reduce-coded-vec (function index datagrid &optional init code)
  "Reduce the FUNCTION across DATAGRID at INDEX.
Return the result of calling FUNCTION with INIT and the first
element of the indexed vector in DATAGRID, then calling FUNCTION
with that result and the second element of that vector, then with
that result and the third element of the vector, etc. FUNCTION
will be called with INIT (and then the accumulated value) as the
first argument, and the elements from the vector as the second
argument. If DATAGRID is empty, return INIT and FUNCTION is not
called.

This function and documentation string are derived from
SEQ-REDUCE."
  (interactive)
  (unless (datagridp datagrid)
    (error "Argument must be a datagrid."))
  (let* ((vec (seq-subseq (aref datagrid index) 1))
	 (vec-coded-alist (when (and (listp code)
				     (cl-every #'consp code))
			    (seq-map (lambda (x) (cadr (assoc x code #'string-equal))) vec)))
	 (vec-coded-vec (when (datagridp code)
			  (let ((key (seq-map (lambda (x) (aref code 0))))
				(value (seq-map (lambda (x) (aref code 1))))))
			  (seq-map (lambda (x) (aref value (seq-position key x))) vec)))
	 (coded-vec (delq nil (or vec-coded-alist
				  (append vec-coded-vec nil)))))
    (when coded-vec (seq-reduce function coded-vec 0))))

(defun datagrid-calc-function-wrapper (func-abbrev var1)
  "Call a Calc function with variables.
FUNC-ABBREV is a string for the Calc abbreviation for a function.
VAR1 is a list or a vector. This function only handles
single-variable statistics for vectors.

You can find these functions by doing C-h f and typing calcFunc-
and viewing completions. You can also find the functions in the
Calc documentations in many places. Find the single-variable
statistics at 10.7.1 - Single Variable Statistics. That list
includes:

vcount, vsum, vprod, vmax, vmean, vmeane, vmedian, vhmean,
vgmean, agmean, rms, vsdev, vpsdev, vvar, vpvar, vflat

Also see the Calc function index. Usually you will find the Emacs
lisp function name followed by the abbreviation. For
example (calc-vector-mean) [vmean].

Example: (datagrid-calc-function-wrapper \"vmax\" '(1 2 3 4 5 5000))"
  (let* ((func-name (intern (concat "calcFunc-" func-abbrev)))
	 (result (funcall func-name (cons 'vec var1))))
    (string-to-number (math-format-number result))))

(defun datagrid-reduce-coded-vec-calc (func-abbrev index datagrid &optional code)
  "Reduce the FUNC-ABBREV across DATAGRID at INDEX using CODE.
Return the result of calling the Calc function FUNC-ABBREV with
INIT and the first element of the indexed vector in DATAGRID,
then calling FUNC-ABBREV with that result and the second element
of that vector, then with that result and the third element of
the vector, etc. FUNC-ABBREV will be called with INIT (and then
the accumulated value) as the first argument, and the elements
from the vector as the second argument. If DATAGRID is empty,
return INIT and FUNC-ABBREV is not called.

This function and documentation string are derived from
SEQ-REDUCE."
  (interactive)
  (unless (datagridp datagrid)
    (error "Argument must be a datagrid."))
  (let* ((vec (seq-subseq (aref datagrid index) 1))
	 (vec-coded-alist (when (and (listp code)
				     (cl-every #'consp code))
			    (seq-map (lambda (x) (cadr (assoc x code #'string-equal))) vec)))
	 (vec-coded-vec (when (datagridp code)
			  (let ((key (seq-map (lambda (x) (aref code 0))))
				(value (seq-map (lambda (x) (aref code 1))))))
			  (seq-map (lambda (x) (aref value (seq-position key x))) vec)))
	 (coded-vec (delq nil (or vec-coded-alist
				  (append vec-coded-vec nil)
				  vec))))
    (when coded-vec (datagrid-calc-function-wrapper func-abbrev coded-vec))))

(defun datagrid-code-column (index datagrid code)
  "Change data based on a code alist.
DATAGRID is the vector of vectors. INDEX is the column to code.
It is zero based counting. CODE is an alist where the keys are
what is in datagrid and the values are the values. A Lickert
scale may be coded as follows.

'((\"Strongly Disagree\" 1)
  (\"Disagree\"	2)
  (\"Neither Agree nor Disagree\" 3)
  (\"Agree\" 4)
  (\"Strongly Agree\" 5))

TODO: Provide better feedback regarding data values which are not
found in the coding alist. I'm not sure what kind of feedback at
this point. I should probably create a function that returns the
original data values that return nil."
  (unless (datagridp datagrid)
    (error "Argument must be a datagrid."))
  (let* ((vec (seq-subseq (aref datagrid index) 1))
	 (vec-coded-alist (when (and (listp code)
				     (cl-every #'consp code))
			    (seq-map (lambda (x) (cadr (assoc x code #'string-equal))) vec)))
	 (vec-coded-vec (when (datagridp code)
			  (let ((key (seq-map (lambda (x) (aref code 0))))
				(value (seq-map (lambda (x) (aref code 1))))))
			  (seq-map (lambda (x) (aref value (seq-position key x))) vec))))
    (or vec-coded-alist
	(append vec-coded-vec nil))))

(defun datagrid-column-max (index datagrid)
  "Find the maximum value in a specific column.
DATAGRID is the vector of vectors. INDEX is the zero based column
number."
  (interactive)
  (datagrid-reduce-vec #'max datagrid index))

(defun datagrid-column-min (index datagrid)
  "Basic column statistics"
  (interactive)
  (datagrid-reduce-vec #'min datagrid index))

(defun datagrid-column-sum (index datagrid)
  "Find the sum of a specific column.
DATAGRID is the vector of vectors. INDEX is the zero based column
number."
  (interactive)
  (datagrid-reduce-vec #'+ datagrid index))

(defun datagrid-column-avg (index datagrid)
  "Basic column statistics"
  (interactive)
  (unless (datagridp datagrid)
    (error "Argument must be a datagrid."))
  (let ((vec (seq-subseq (aref datagrid index) 1)))
    (/ (seq-reduce #'+ vec 0) (length vec))))

(defun datagrid-column-frequencies (index datagrid)
  "Find the frequency of elements occuring in a column.
DATAGRID is the vector of vectors. INDEX is the zero based column
number. The elements are compared using equal."
  (unless (datagridp datagrid)
    (error "Argument must be a datagrid."))
  (let ((vec (seq-subseq (aref datagrid index) 1))
	(counts (make-hash-table :test 'equal)))
    (cl-loop for item across vec do
	     (puthash item (1+ (gethash item counts 0)) counts))
    ;; Convert to alist for easier viewing
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) counts)
      result)))

(defun datagrid-column-mode (index datagrid)
  "Find the statistical mode of a column.
DATAGRID is the vector of vectors. INDEX is the zero based column
number. The elements are compared using equal. Therefore, data
does not have to be numeric to find the mode."
  (let ((sorted-frequencies (sort (datagrid-column-frequencies index datagrid)
				  (lambda (a b) (> (cdr a) (cdr b))))))
    (caar sorted-frequencies)))

(defun datagrid-column-unique (index datagrid)
  "Return unique items from a column."
  (interactive)
  (unless (datagridp datagrid)
    (error "Argument must be a datagrid."))
  (let ((vec (seq-subseq (aref datagrid index) 1)))
    (seq-uniq vec)))


;;;; Summary reports
(defun datagrid-report-lickert (datagrid index &optional code)
  "Display statistic related to Lickert scales from a column.
Use this only for ordinal data. DATAGRID is the vector of
vectors. INDEX is the column to analyze. It uses zero based
counting."
  ;; Get rid of the mean and stdev and add quartile related measures.
  ;; TODO: Pass coded data to the mode function.
  (unless (datagridp datagrid)
    (error "Argument must be a datagrid."))
  (let* ((stats-name '("vcount" "vmin" "vmax" "vmedian" "vmean" "vsdev"))
	 (vec (if code
		  (datagrid-code-column index datagrid code)
		(datagrid-get-column index datagrid)))
	 (stats (cl-loop for statn in stats-name
			 ;; TODO: This is redoing the separating of the
			 ;; vector out of the datagrid. Change later.
			 collect (datagrid-reduce-coded-vec-calc statn index datagrid code))))
    (append (cl-mapcar #'cons stats-name stats)
	    (list (cons "mode" (datagrid-column-mode index datagrid))))))

;;;; Misc work
(cl-loop for col from 1 to 44
	 collect (datagrid-report-lickert mysurvey col datagrid-coding-key1))

;; TODO: Create functions to gather quartile information.
