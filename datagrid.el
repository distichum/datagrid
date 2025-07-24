;;; datagrid.el --- Functions for a vector of vectors data structure. -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Joshua Lambert
;; Author: Joshua Lambert <jlambert@missouristate.edu>
;; Maintainer: Joshua Lambert <jlambert@missouristate.edu>
;; Created: 2025-03-31
;; Version: 0.1
;; Keywords: tools, convenience, sorting
;; URL: https://github.com/distichum/datagrid
;; Package-Requires: (seq calc cl-lib)
;; CSV functions require: (csv-mode)

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
;; in Emacs Lisp. A structure is made of one vector whose elements are
;; datagrid-column data structures. Each datagrid-column structure is
;; like a table column. A datagrid-column is defined by cl-defstruct.
;; Each datagrid-column has a data slot that contains a vector. All
;; dagatrid-columns in a datagrid must have an equal number of
;; elements in their datagrid-column-data slot. Think of it as tabular
;; data, a spreadsheet, a database table, or a dataframe. The purpose
;; of datagrid.el is to manipulate data in and do calculations on small to
;; medium sized data sets.

;; datagrid.el is not meant to display data in a buffer or manipulate
;; data that is already in a table. That said, the
;; datagrid-to-org-table function, executed in an Org Mode code block,
;; will produce a typical Org Mode table.

;;; Code:
;;;; Prerequisites
(require 'cl-lib)
(require 'seq)
(require 'calc)
(require 'calc-vec)


;;;; Variables:

(defvar datagrid-test-alist '(("date" . ("2025-03-31" "2025-04-01" "2025-04-02" "2025-04-03"))
			      ("location" . ("somewhere" "out" "there" "far"))
			      ("precipitation" . (0.5 0 .25 1))
			      ("high-temp" . (15 20 32 22))
			      ("rating" . ("Strongly disagree"
					   "Disagree"
					   "Neutral"
					   "Neutral")))
  "A test association list.")

(cl-defstruct (datagrid-column (:constructor datagrid-column-make)
                               (:copier datagrid-column-copy)
                               (:predicate datagrid-column--p))
  "A datagrid column contains a vector and attributes of that
vector. The vector may contain anything an Emacs vector may
contain, but some functions in datagrid.el expect a single
dimensional vector and consistent data types per vector.
The datagrid attributes are heading, data, lom, and code.

The Level Of Measurement (LOM) comes from Stanley Smith Stevens
and may be one of the following:

  nominal
  ordinal
  interval
  ratio
  nil (not from Stevens)

Values of nominal data types may be strings or numbers. Ordinal data in
datagrid.el may be strings or numbers. Interval data must be a number,
either an integer or a float. Ratio data must be a number, either an
integer or a float. Data types of nil may be of any data type. (Weighing
down the data structure with this slot may not be worth it. See if
anyone uses this or not. It can be helpful when automating summary data
presentation.)

CODE is an alist where the keys are one possible interpretation
of the research data and the value is another. For example:

 '(('Strongly disagree' . 1)
   ('Disagree'          . 2)
   ('Neutral'           . 3)
   ('Agree'             . 4)
   ('Strongly agree'    . 5))

Converting such categories to numerical codes better allows them to be
sorted and counted by computer programs."
  (heading nil :type string :documentation "Column heading")
  (data nil :type vector :documentation "The column's data")
  (lom nil :type string :documentation "Level of measurement.")
  (code nil :type list :documentation "An alist used to code or decode values."))

(defvar datagrid-column-example
  (datagrid-column-make :heading "I like Emacs."
			:data [5 5 5 5 5]
			:lom "ordinal"
			:code  '(("Strongly disagree" . 1)
				 ("Disagree"          . 2)
				 ("Neutral"           . 3)
				 ("Agree"             . 4)
				 ("Strongly agree"    . 5)))
  "An example datagrid-column structure.")

(defvar datagrid-example
  (vector (datagrid-column-make
	   :heading "date"
	   :data ["2025-03-31" "2025-04-01" "2025-04-02" "2025-04-03"]
	   :lom nil)
	  (datagrid-column-make
	   :heading "location"
	   :data ["somewhere" "out" "there" "far"]
	   :lom "ordinal"
	   :code  '(("somewhere" . 1)
		    ("out"       . 2)
		    ("there"     . 3)
		    ("far"       . 4)))
	  (datagrid-column-make
	   :heading "precipitation"
	   :data [0.5 0 .25 1]
	   :lom "ratio")
	  (datagrid-column-make
	   :heading "high-temp"
	   :data [15 20 32 22]
	   :lom "interval")
	  (datagrid-column-make
	   :heading "rating"
	   :data ["good" "bad" "ugly" "uglier"]
	   :lom "nominal"))
  "A datagrid is a vector of datagrid-column data structures.
This example can be used for testing.")


;;;; Helper functions:
(defun datagrid-safe-transpose (seq-of-seqs)
  "Transpose a sequence of sequences and pad short rows if needed.
The result is a list of lists."
  (let* ((max-cols (apply #'max (mapcar #'length seq-of-seqs)))
	 (listed (seq-map (lambda (seq) (if (listp seq)
					    seq
					  (append seq nil)))
			  seq-of-seqs))
	 (padded (cl-loop for row in listed
			  collect (append row
					  (make-list (- max-cols (length row))
						     nil)))))
    (apply #'cl-mapcar #'list padded)))

(defun datagrid-unknown-type-to-number (seq)
  "Convert a sequence of unknown types to numbers.
Convert a SEQ of unknown data types to a sequence of
numbers. Leave nil values in place."
  (let ((seqnow (seq-into seq 'list)))
    (vconcat
     (cl-loop for item in seqnow
	      collect (cond ((stringp item) (if (equal item "")
						nil
					      (string-to-number item)))
			    ((numberp item) item)
			    ((null item) item)
			    (t (error "%s cannot be coerced into a number"
				      item)))))))

(defun datagrid-unknown-type-sort (a b)
  "Predicate sort function when type is unknown.
A and B must be the same type but the type may not be known ahead of
time.

Generated by Claude AI and adapted: 2025-06-13."
  (cond
   ((numberp a) (< a b))
   ((stringp a) (string< a b))
   ((symbolp a) (string< (symbol-name a) (symbol-name b)))
   ;; Use string comparison if others don't match.
   (t (string< (format "%s" a) (format "%s" b)))))


;;;; datagrid-column functions
(defun datagrid-column-p (dg-c)
  "Return non-nil if DG-C is a datagrid-column, nil otherwise.
HEADINGS must be strings. DATA must be a vector. LOM must be a string.
CODE must be an alist."
  (and (datagrid-column--p dg-c)
       (or (null (datagrid-column-heading dg-c))
           (stringp (datagrid-column-heading dg-c)))
       (or (null (datagrid-column-data dg-c))
           (vectorp (datagrid-column-data dg-c)))
       (or (null (datagrid-column-lom dg-c))
           (stringp (datagrid-column-lom dg-c)))
       (or (null (datagrid-column-code dg-c))
           (listp (datagrid-column-code dg-c)))))

(defun datagrid-column-set-length (dg-c N)
  "Truncate or expand the data in a datagrid column.
Data may be truncated, extended with nils, or left the same. DG-C is the
datagrid column. N is the length.

This creates a new vector and replaces the old."
  (when-let* ((datagrid-column-p dg-c)
	      (vec (datagrid-column-data dg-c))
	      (len (length (datagrid-column-data dg-c))))
    (if (= len N)
	dg-c
      (datagrid-column-make
       :data (if (< N len)
		 (seq-take vec N)
	       (seq-concatenate 'vector vec (make-vector (- N len) nil)))
       :heading (datagrid-column-heading dg-c)
       :lom (datagrid-column-lom dg-c)
       :code (datagrid-column-code dg-c)))))

(defun datagrid--column-add-data (dg-c seq)
  "Add one or more elements to a datagrid column's data.
DG-C is the datagrid column. SEQ is a sequence. The result of this
function is a datagrid-column.

WARNING. Use only in conjunction with DATAGRID-ADD-ROW which keeps the
datagrid column lengths in sync."
  (when-let* ((datagrid-column-p dg-c)
	      (data (datagrid-column-data dg-c)))
    (datagrid-column-make
     :data (vconcat data seq)
     :heading (datagrid-column-heading dg-c)
     :lom (datagrid-column-lom dg-c)
     :code (datagrid-column-code dg-c))))


;;;; Create datagrid-column methods for seq
;; This ignores datagrid-column slots other than the :data slot.
;; streams.el and ordered-set.el are good examples.
(cl-defmethod seq-elt ((datagrid-column datagrid-column) n)
  "Return the Nth element of the DATAGRID-COLUMN data."
  (elt (datagrid-column-data datagrid-column) n))

(cl-defmethod seq-length ((datagrid-column datagrid-column))
  "Return the length of the DATAGRID-COLUMN data."
  (length (datagrid-column-data datagrid-column)))

(cl-defmethod seq-do (function (datagrid-column datagrid-column))
  "Apply FUNCTION to each element of DATAGRID-COLUMN data.
Return the original DATAGRID-COLUMN. Presumably, FUNCTION has
useful side effects."
  (mapc function (datagrid-column-data datagrid-column))
  datagrid-column)

(cl-defmethod seqp ((datagrid-column datagrid-column))
  "DATAGRID-COLUMNS are sequences."
  t)

(cl-defmethod seq-subseq ((datagrid-column datagrid-column) start &optional end)
  "Return a new DATAGRID-COLUMN containing a range of elements.
START is the first element to select and END is the last. END is
exclusive."
  (datagrid-column-make
   :heading (datagrid-column-heading datagrid-column)
   :data (seq-take (seq-drop (datagrid-column-data datagrid-column) start)
		   (- end start))
   :lom (datagrid-column-lom datagrid-column)
   :code (datagrid-column-code datagrid-column)))

(cl-defmethod seq-into-sequence ((datagrid-column datagrid-column))
  "Return datagrid-column as a sequence."
  ;; Since a datagrid-column is a record data type, this does nothing
  ;; but expose the sequence part of datagrid-column.
  (datagrid-column-data datagrid-column))

(cl-defmethod seq-copy ((datagrid-column datagrid-column))
  "Return a shallow copy of DATAGRID-COLUMN."
  (datagrid-column-copy datagrid-column))

(cl-defmethod seq-into ((datagrid-column datagrid-column) type)
  "Convert DATAGRID-COLUMN into a sequence of type TYPE.
This is lossy because other DATAGRID-COLUMN slots are not copied."
  (seq-into (seq-into-sequence datagrid-column) type))

(cl-defmethod seq-into (sequence (_type (eql datagrid-column)))
  "Convert SEQUENCE into a DATAGRID-COLUMN."
  (datagrid-column-make :data (seq-into sequence 'vector)))



;;;; Making and changing data grids:
(defun datagrid-from-alist (alist &optional headings extend-uneven)
  "Create a datagrid from an ALIST using the datagrid-column struct.
If HEADINGS is nil, the alist has no headings. If HEADINGS is non-nil,
then it does. If EXTEND-UNEVEN is non-nil, then extend lists with nil so
that they are all the same length. If EXTEND-UNEVEN is nil, then take
only the data for a row up to the minimum row length."
  (interactive)
  (let* ((ralist (reverse alist))
         (values (mapcar (if headings #'cdr #'identity) ralist))
         (min-length (apply (if extend-uneven #'max #'min)
                            (mapcar #'length values)))
	 result)
    (dolist (item ralist)
      (let* ((label (if headings (car item) nil))
             (data (if headings (cdr item) item))
             (extended (if extend-uneven
                           (append data (make-list (max 0 (- min-length (length data))) nil))
			 (seq-take data min-length))))
	(push (datagrid-column-make
               :heading label
               :data (vconcat extended))
              result)))
    (vconcat result)))

(defun datagrid-make (col-num row-num &optional init heading-list)
  "Create a new datagrid with specified dimensions.
Return a newly created vector of datagrid-column structs where
COL-NUM is the number of columns. ROW-NUM is the number of rows
in each column. HEADING-LIST is a list whose length must equal
the number of columns. INIT is the value of each element in each
:data slot vector in each datagrid-column struct."
  (interactive)
  (let ((headings (or heading-list (make-list col-num nil))))
    (cl-loop for elt from 1 to col-num
	     ;; Make a list of ROW-NUM items, all of which are INIT.
	     for init-list = (make-list row-num init)
	     ;; Create a datagrid-column struct with init-list as the data.
	     for dg-col = (datagrid-column-make :data init-list)
	     ;; Set the heading of the datagrid-column
	     do (setf (datagrid-column-heading dg-col) (elt headings (1- elt)))
	     collect dg-col
	     ;; It seems like I would not need the into and finally.
	     into parent-vector
	     finally return (vconcat parent-vector))))


(defun datagrid-from-vectors (vec1 &rest other-vectors)
  "Create a datagrid from vectors.
VEC1 is the first vector. OTHER-VECTORS are any other vectors
passed to the function. The first element in each vector is the
heading."
  (interactive)
  (unless (vectorp vec1)
    (error "The first argument must be a vector"))
  (let ((v1 (datagrid-column-make :heading (elt vec1 0)
				  :data (seq-drop vec1 1)))
	(vother (when other-vectors
		  (cl-loop for item in other-vectors
			   collect (datagrid-column-make
				    :heading (elt item 0)
				    :data (seq-drop item 1))))))
    (vconcat (cons v1 vother))))

(defun datagrid-from-csv-buffer (buffer-or-name &optional headings)
  "Return a datagrid from an open csv buffer.
BUFFER-OR-NAME is the buffer name. If HEADINGS is nil, then there
is not a headings row. If t, then there is. The default is nil.

REQUIRES: CSV-MODE"
  (require 'csv-mode)
  (save-current-buffer
    (set-buffer buffer-or-name)
    (save-excursion
      (let ((data nil)
	    (2d-by-column nil))
	(goto-char (point-min))
	(while (not (eobp))
	  (setq data (cons (csv-parse-current-row) data))
	  (forward-line))
	;; Transpose the list of lists to make it a column store.
	(setq 2d-by-column (datagrid-safe-transpose (nreverse data)))
	(vconcat (cl-loop for item in 2d-by-column
                          collect (datagrid-column-make
				   :heading (when headings (elt item 0))
				   :data (vconcat (if headings
						      (seq-drop item 1)
						    item)))))))))

(defun datagrid-from-csv-file (file-path &optional headings)
  "Return a datagrid from a CSV file at FILE-PATH.
If HEADINGS is nil, then there is not a headings row. If t, then
there is. The default is nil.

REQUIRES: CSV-MODE"
  (require 'csv-mode)
  (with-temp-buffer
    (insert-file-contents file-path)
    (csv-mode)
    (let ((data nil)
          (2d-by-column nil))
      (goto-char (point-min))
      (while (not (eobp))
        (setq data (cons (csv-parse-current-row) data))
        (forward-line))
      (setq 2d-by-column (datagrid-safe-transpose (nreverse data)))
      (vconcat (cl-loop for item in 2d-by-column
                        collect (datagrid-column-make
				 :heading (when headings (elt item 0))
				 :data (vconcat (if headings
						    (seq-drop item 1)
						  item))))))))

(defun datagrid-to-vec-of-vec (datagrid)
  "Create a vector of vectors from a DATAGRID.
This is a lossy function because it does not maintain the headings,
code, or lom slot."
  (vconcat (cl-loop for elt across datagrid
		    collect (datagrid-column-data elt))))

(defun datagrid-to-alist (datagrid &optional headings)
  "Create a vector of vectors from a DATAGRID.
If HEADINGS is non-nil, return headings as the first item in each
list. Otherwise return only data."
  (let ((hdngs (and headings
		    (append (datagrid-get-headings datagrid) nil)))
	(data (datagrid-to-vec-of-vec datagrid)))
    (cl-loop for x from 0 below (length hdngs)
	     collect (append (list (elt hdngs x))
			     (append (elt data x) nil)))))

(defun datagrid-to-org-table (datagrid)
  "Return data to create an Org table."
  (datagrid-safe-transpose (datagrid-to-alist datagrid t)))

(defun datagrid-column-add-code (datagrid index code)
  "Add coding data to a datagrid-column.
DATAGRID is the vector of structs. INDEX is a number or a list of
numbers. The function adds CODE in the CODE slot of DATAGRID. If INDEX
is a list, then CODE is applied to all datagrid columns at those
positions.

CODE is an alist where the keys are one possible interpretation
of the research data and the value is another. For example:

 ((\"Strongly disagree\" . 1)
  (\"Disagree\"          . 2)
  (\"Neutral\"           . 3)
  (\"Agree\"             . 4)
  (\"Strongly agree\"    . 5))"
  (if (listp index)
      (cl-loop for elt in index
	       do (setf (datagrid-column-code (aref datagrid elt)) code))
    (setf (datagrid-column-code (aref datagrid index)) code)))


;;;; Datagrid utilities; results are not datagrids:
(defun datagridp (datagrid)
  "Check if DATAGRID is a valid vector of datagrid-columns.
The datagrid-columns must have DATAGRID-COLUMN-DATA fields with equal
length."
  (and (vectorp datagrid)
       (> (length datagrid) 0)
       (seq-every-p #'datagrid-column-p datagrid)
       (apply #'= (seq-map (lambda (x) (length (datagrid-column-data x)))
			   datagrid))))

(defun datagrid-dimensions (datagrid)
  "Return the two dimensions of the DATAGRID's data slot.
DATAGRID is a vector of datagrid-column structs. Returns a cons
cell (columns . rows), where columns is the number of
columns and rows is the number of rows."
  (interactive)
  (let ((dg (aref datagrid 0)))
    (cons (length datagrid) (length (datagrid-column-data dg)))))

(defun datagrid-get-elt (datagrid column-num row-num)
  "Get a value at a specific column and row.
DATAGRID is the vector of datagrid-columns. COLUMN-NUM is the
column number with zero based counting. ROW-NUM is the row number
with zero based counting. Headings are not part of the row
numbering."
  (interactive)
  (let ((dg-col (aref datagrid column-num)))
    (aref (datagrid-column-data dg-col) row-num)))

(defun datagrid-get-col-data (datagrid index)
  "Extract a column vector from DATAGRID at INDEX.
DATAGRID is a vector of a datagrid column structure. INDEX is the column
to extract, with zero based counting. Returns a new vector containing
all elements from the specified column, including the heading."
  (interactive)
  (datagrid-column-data (aref datagrid index)))

(defun datagrid-get-row-data (datagrid row-num)
  "Extract an entire row from DATAGRID.
DATAGRID is a vector of vectors. ROW-NUM is the index of the row to
extract, with zero based counting. Row 0 contains the headings.
Returns a new vector containing all elements from the specified
row."
  (vconcat (seq-map (lambda (vec) (aref (datagrid-column-data vec)
					row-num))
		    datagrid)))

(defun datagrid-get-headings (datagrid)
  "Create a vector from all DATAGRID headings."
  ;; Benchmarking shows similar numbers for cl-loop version.
  (vconcat (seq-map #'datagrid-column-heading datagrid)))

(defun datagrid-col-index-by-header (datagrid header-text)
  "Return the DATAGRID column number with HEADER-TEXT.
Return nil if the header is not found."
  (let ((col 0)
	(head nil))
    (while (and (< col (length datagrid))
		(not head))
      (when (equal (datagrid-column-heading (aref datagrid col)) header-text)
        (setq head t))
      (unless head
	(setq col (1+ col))))
    (when head col)))

(defun datagrid-column-decode (datagrid index)
  "Output a decoded datagrid column as a vector.
DATAGRID is the vector of structs. INDEX is the column number to
code. It is zero based counting. The datagrid-column must have
DATAGRID-COLUMN-CODE to decode the data. If not, then the output
is simply DATAGRID-COLUMN-DATA.

 A Lickert scale DATAGRID-COLUMN-CODE may be coded as follows.

\\='((\"Strongly Disagree\" 1)
     (\"Disagree\"	2)
     (\"Neither Agree nor Disagree\" 3)
     (\"Agree\" 4)
     (\"Strongly Agree\" 5))

The DATAGRID-COLUMN-DATA may look like the following.

 [\"Strongly Disagree\" \"Agree\" \"Disagree\"]

This function will output the following.

 [1 4 2]

TODO: Provide better feedback regarding data values which are not
found in the coding alist. I'm not sure what kind of feedback at
this point. I should probably create a function that returns the
original data values that return nil."
  (unless (datagridp datagrid)
    (error "Argument must be a datagrid"))
  (let* ((vec (datagrid-column-data (aref datagrid index)))
	 (code (datagrid-column-code (aref datagrid index)))
	 (coded-alist (when (and code ;; code must exist
				 (listp code) ;; it must be a list
				 (cl-every #'consp code)) ; it is an alist
			(seq-map (lambda (x)
				   (cdr (assoc x code #'string-equal)))
				 vec))))
    (vconcat coded-alist)))



;;;; Inspection and manipulation of datagrids; return a datagrid:
(defun datagrid-head (datagrid &optional column-num row-num)
  "Return the first ROW-NUM rows and COLUMN-NUM columns of DATAGRID.
COLUMN-NUM is the number of columns (default 5). ROW-NUM is the number
of rows (default 5)."
  (let* ((col-num (or column-num 5))
	 (row-num (or row-num 5)))
    (vconcat
     (cl-loop for x from 0 below col-num
	      collect (datagrid-column-make
		       :heading (datagrid-column-heading
				 (elt datagrid x))
		       :data (seq-take (datagrid-get-col-data datagrid x)
				       row-num)
		       :lom (datagrid-column-lom (elt datagrid x))
		       :code (datagrid-column-code (elt datagrid x)))))))

(defun datagrid-add-column (datagrid &rest datagrid-columns)
  "Add one or more datagrid-column structs to a datagrid.
DATAGRID is a datagrid. DATAGRID-COLUMNS is a list of datagrid-column
structs. The new columns are always expanded or truncated to fit the
length of the existing columns. All datagrid columns must have the same
length."
  (interactive)
  (when-let*
      ((datagridp (datagridp datagrid))
       (datagrid-cols-p (not (member nil (mapcar #'datagrid-column-p
						 datagrid-columns))))
       ;; Get the length of the data in datagrid columns. This
       ;; is the length the new sequences must conform to.
       (col-len (length (datagrid-column-data (aref datagrid 0))))
       ;; Truncate or expand the new data sequence slots.
       (truncd (let ((trunc-all nil))
		 (dolist (dg-c datagrid-columns trunc-all)
		   (setq trunc-all (cons (datagrid-column-set-length
					  dg-c col-len)
					 trunc-all))))))
    (seq-concatenate 'vector datagrid truncd)))

(defun datagrid-add-row (datagrid seq)
  "Add elements to the end of each datagrid-column.
DATAGRID a datagrid structure. SEQ is a list of sequences. Each of the
sequences in SEQ must be of equal length to the number of
datagrid-columns in datagrid."
  (interactive)
  ;; Check common errors first. This one is easy to screw up.
  (unless (datagridp datagrid)
    (error "DATAGRID is not a datagrid structure"))
  (unless (apply #'= (mapcar #'length seq))
    (error "The sequences in SEQ are not of equal length"))
  (unless (= (length datagrid) (length (car seq)))
    (error (concat "The number of datagrid columns is not "
		   "equal to the length of sequences in SEQ.")))
  ;; Transpose the sequences first. With a small number of rows to
  ;; add, this probably isn't worth it, but for many, it probably is.
  ;; TODO: Benchmark.
  (let ((trans-seq (apply #'cl-mapcar #'list seq)))
    (vconcat (cl-loop for struct across datagrid
		      collect (datagrid--column-add-data struct (pop trans-seq))))))

(defun datagrid--add-data-by-column (datagrid seqs)
  "Add data to DATAGRID columns.
SEQS is a sequence of sequences. Each sub-sequence is one column's data.
The sequences of data to add must be in the same order as the
datagrid-columns in DATAGRID. The length of SEQS must be equal to the
length of datagrid. If the sequences added are not of equal length, then
nil will be padded onto other columns to make the data equal."
  (let ((max-len (seq-max (seq-map #'length seqs))))
    (vconcat
     (cl-loop for x from 0 below (length seqs)
	      for new-seq = (seq-concatenate
			     'vector (elt seqs x)
			     (make-list (- max-len (length (elt seqs x))) nil))
	      collect (datagrid--column-add-data (elt datagrid x)
						 new-seq)))))

(defun datagrid-add-data (datagrid seqs &optional horizontal)
  "Add elements to each datagrid-column.
DATAGRID a datagrid structure. SEQS is a sequence of sequences. If
HORIZONTAL is nil, then each sub-sequence is one column's data. If non-nil,
then each sequence is one row's data. The default is nil. The sequences
are extended to keep datagrid-column-data lengths equal.

The sequences of data to add must be in the same order as the
datagrid-columns in DATAGRID."
  (interactive)
  (unless (datagridp datagrid)
    (error "DATAGRID is not a datagrid structure"))
  (if horizontal
      (datagrid--add-data-by-column datagrid (datagrid-safe-transpose seqs))
    (datagrid--add-data-by-column datagrid seqs)))

(defun datagrid-set-headings (datagrid heading-list)
  "Set all DATAGRID headings using HEADING-LIST.
HEADING-LIST is a list of strings where each string is a heading name.
The headings in the list must be in the same order as the
datagrid-columns in the datagrid."
  (cl-loop for x from 0 below (length datagrid)
	   do (setf (datagrid-column-heading (elt datagrid x))
		    (nth x heading-list)))
  datagrid)

(defun datagrid-remove-column (datagrid index)
  "Remove the DATAGRID column at INDEX.
INDEX is the column to remove. It is zero based counting."
  (interactive)
  (when (datagridp datagrid)
    (vconcat (seq-take datagrid index)
	     (seq-drop datagrid (1+ index)))))

(defun datagrid-remove-row (datagrid index)
  "Remove the DATAGRID row at INDEX.
This function provides a new datagrid with the INDEX row eliminated."
  (interactive)
  (vconcat
   (cl-loop for elt across datagrid
	    collect (let ((new-col-data (datagrid-column-copy
					 datagrid)))
		      (setf (datagrid-column-data new-col-data)
			    (vconcat (seq-take (datagrid-column-data elt)
					       index)
				     (seq-drop (datagrid-column-data elt)
					       (1+ index))))
		      new-col-data))))

(defun datagrid-sort (datagrid index)
  "Sort a datagrid by a specific column.
DATAGRID is a vector of vectors. INDEX is the zero based index of the
column to sort by. This function assumes that elements in one column are
of like data type and will cause errors if they are not. Created with
the help of Claude.ai."
  ;; TODO: Recreate this with an indirect sorting method as suggested at
  ;; https://www.reddit.com/r/emacs/comments/1lv24a7/comment/n22kkbp/?context=3
  (let* ((new-dg (copy-sequence datagrid))
	 ;; Get the data from the column to sort by
	 (sort-col-data (datagrid-column-data (aref new-dg index)))
	 (sort-col-length (length sort-col-data))
	 ;; Create indices and pair with values from sort column
	 (indexed-vals (cl-loop for i from 0 below sort-col-length
				collect (cons i (aref sort-col-data i))))
	 ;; Sort the pairs by the values
	 (sorted-pairs
	  (seq-sort-by #'cdr #'datagrid-unknown-type-sort indexed-vals))
	 ;; Extract the new order of indices
	 (new-indices (mapcar #'car sorted-pairs)))
    ;; Reorder each column according to the sorted indices
    (dotimes (col-idx (1- (length new-dg)))
      (let* ((col-data (datagrid-column-data (aref new-dg col-idx)))
             (new-col-data (make-vector sort-col-length nil)))
	(dotimes (i sort-col-length)
          (setf (aref new-col-data i) (aref col-data (nth i new-indices))))
	(setf (datagrid-column-data (aref new-dg col-idx)) new-col-data)))
    new-dg))



(defun datagrid-join (datagrid1 join-on-1 datagrid2 join-on-2 dg2-col)
  "Join DATAGRID1 and DATAGRID2 to create a new datagrid column.
Create a new datagrid with a new DATAGRID-COLUMN by doing a left
outter join where DATAGRID1 is left and DATAGRID2 is right. Join
on column index JOIN-ON-1 and JOIN-ON-2, which are simply index
numbers indicating a column from each datagrid. DG2-col is the
column from datagrid two that contains the new vector's data.

Unlike SQL, it is only possible to specify one column of data to
collect."
  (let* ((dg1-col (datagrid-column-data
		   (elt datagrid1 join-on-1)))
	 (dg2-col-a (datagrid-column-data
		     (elt datagrid2 join-on-2)))
	 (dg2-col-b (datagrid-column-data
		     (elt datagrid2 dg2-col)))
	 ;; Using cl-loop instead of seq-map or cl-flet provides much
	 ;; better performance here.
	 (new-list (cl-loop
		    for elt across dg1-col
		    collect (funcall (lambda (lelt)
				       (elt dg2-col-b
					    (seq-position dg2-col-a lelt)))
				     elt))))
    (datagrid-add-column
     datagrid-example (datagrid-column-make :data (vconcat new-list)))))

;;;; Filters and masks:
(defun datagrid-create-mask (datagrid pred index)
  "Create a mask for a DATAGRID column at INDEX.
PRED is a function of one argument. It will operate on the
datagrid column at INDEX. INDEX is zero based.

This simply returns a vector

See the Info Manual for built-in predicates, equality functions,
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
  (let* ((struct (elt datagrid index))
	 (vec (datagrid-column-data struct))
	 (mask (and (vectorp datagrid)
		    (> (length datagrid) 0)
		    (vectorp vec)
		    (seq-map pred vec))))
    (vconcat mask)))


(defun datagrid-filter-vector-by-mask (column-struct mask)
  "Filter a datagrid-column struct by a boolean MASK.
COLUMN-STRUCT is a datagrid-column structure. MASK must be a sequence
with the same length as datagrid columns and the values must only be t
or nil. It is typically created by datagrid-create-mask-s. This is a
helper function for DATAGRID-FILTER-BY-MASK. It returns a
datagrid-column structure that copies the original but with the data
slot filtered."
  (let* ((mask2 (vconcat mask))
	 (vec (datagrid-column-data column-struct))
	 (vec2 (cl-loop for x from 0 below (length vec)
			collect (and (aref mask2 x)
				     (aref vec x)))))
    (datagrid-column-make :heading (datagrid-column-heading column-struct)
			  :data (vconcat (delq nil vec2))
			  :lom (datagrid-column-lom column-struct)
			  :code (datagrid-column-code column-struct))))

(defun datagrid-filter-by-mask (datagrid mask)
  "Use a boolean MASK to filter a DATAGRID.
MASK must be a vector with the same length as datagrid columns
and the values must only be t or nil. The function returns a
DATAGRID. You can use DATAGRID-CREATE-MASK to create a mask.

The following code show an example workflow when using masks.

 (let* ((mygrid (datagrid-from-csv-buffer \"name-of-file.csv\" t))
        (index (datagrid-col-index-by-header mygrid \"heading-name\"))
        (func (lambda (x) (string-equal x \"filter-string\")))
        (mask (datagrid-create-mask mygrid func index)))
   (datagrid-filter-by-mask mygrid mask))"
  (vconcat
   (cl-loop for vec across datagrid
	    collect (datagrid-filter-vector-by-mask vec mask))))

(defun datagrid-group-by (datagrid index)
  "Group data in DATAGRID according to INDEX.
INDEX is a column number. The resulting structure is a 3D vector. The
first dimension is a vector of groups which were formed by grouping rows
of the datagrid by unique values in COL-VALUES. The 2nd dimension vector
contains the original datagrid vectors filtered for only that group. The
3rd dimension vector contains the data from one column for one group.
REWORD.

This function is slow and inefficient."
  ;; TODO: Profile this and see where it is slow.
  (vconcat
   (cl-loop for item in (datagrid-column-unique datagrid index)
	    collect (list item (datagrid-to-vec-of-vec
				(datagrid-filter-by-mask
				 datagrid
  				 (datagrid-create-mask
				  datagrid
				  (lambda (x) (string-equal item x))
				  index)))))))


;;;; Data analysis:
(defun datagrid-reduce-vec (datagrid function index &optional code convert)
  "Reduce a FUNCTION across DATAGRID data at INDEX.
Return the result of calling FUNCTION on the data vector at INDEX
from a datagrid column. If CODE is nil, then column data is not
decoded. If CODE is non-nil then decode data using the alist in
the code slot of the datagrid column structure. If CONVERT is t,
loop over the data to convert strings to numbers as needed.

Examples:
\ (datagrid-reduce-vec datagrid-example #`+ 2)
\ (datagrid-reduce-vec datagrid #`max index code convert)
\ (datagrid-reduce-vec datagrid #`+ index code convert)

This function and documentation string are derived from
SEQ-REDUCE."
  (interactive)
  (unless (datagridp datagrid)
    (error "Argument must be a datagrid"))
  (let* ((vec (if code
		  (datagrid-column-decode datagrid index)
		(datagrid-column-data (aref datagrid index))))
	 (vec (if convert
		  (datagrid-unknown-type-to-number vec)
		vec)))
    (seq-reduce function vec 0)))

(defun datagrid-prep-for-calc (item)
  "Prepare the ITEM for use in a Calc function.
ITEM should be a string or number. The following steps will be
taken in order: all data values will be converted to a number or
return an error; floating point numbers will be converted to a
format Calc functions can understand.

Use the following to prepare a sequence of items.

Example: (seq-keep #\\='datagrid-prep-for-calc your-seq)"
  (let* ((it (cond ((stringp item) (if (equal item "")
				       nil
				     (string-to-number item)))
		   ((numberp item) item)
		   ((null item) item)
		   (t (error "%s cannot be coerced into a number"
			     item)))))
    (if (floatp it)
	(math-read-number (number-to-string it))
      it)))

(defun datagrid-calc-function-wrapper (func-abbrev lst)
  "Call a Calc vector function for a list.
FUNC-ABBREV is a string which comes from a Calc function.
LST is a list. This function only handles single-variable vector
statistics.

Examples:
\ (datagrid-calc-function-wrapper \"vmax\" `(1 2 3 4 5 5000))
\ (datagrid-calc-function-wrapper \"vflat\" `(1 2 3 4 5 5000))
\ (datagrid-calc-function-wrapper \"vmedian\" `(1 2 3 4 5 5000))

Lists must include only numbers, those numbers must be in Calc
understandable form, and not include nil. Calc cannot handle
decimal numbers for division. Use the following to prepare a
list.

  (seq-keep #`datagrid-prep-for-calc your-list)

You can find Calc functions by doing \\[describe-function] (C-h
f) and typing calcFunc- and viewing completions. You can also
find the functions in the Calc documentation in many places. Find
the single-variable statistics at 10.7.1 - Single Variable
Statistics. That list includes:

vcount, vsum, vprod, vmax, vmean, vmeane, vmedian, vhmean,
vgmean, agmean, rms, vsdev, vpsdev, vvar, vpvar, vflat

Also see the Calc function index. Usually you will find the Emacs
Lisp function name followed by the abbreviation. For
example (calc-vector-mean) [vmean]."
  (let* ((func-name (intern (concat "calcFunc-" func-abbrev)))
	 (result (funcall func-name (cons 'vec lst))))
    (string-to-number (math-format-number result))))

(defun datagrid-reduce-vec-calc (datagrid func-abbrev index &optional code convert)
  "Reduce an Emacs Calc function, FUNC-ABBREV, across DATAGRID data.
Return the result of calling the Calc function FUNC-ABBREV on the
data in a datagrid column at INDEX. If CODE is nil, then column
data is not decoded. If CODE is non-nil then decode data using
the alist in the code slot of the datagrid column structure. If
CONVERT is t, loop over the data to convert strings to numbers as
needed.

FUNC-ABBREV is a string for the Calc abbreviation for a function.
You can find these functions by doing \\[describe-function] (C-h f) and typing calcFunc-
and viewing completions. You can also find the functions in the
Calc documentation in many places. Find the single-variable
statistics at 10.7.1 - Single Variable Statistics. That list
includes:

vcount, vsum, vprod, vmax, vmean, vmeane, vmedian, vhmean,
vgmean, agmean, rms, vsdev, vpsdev, vvar, vpvar, vflat

Also see the Calc function index. Usually you will find the Emacs
Lisp function name followed by the abbreviation. For
example (calc-vector-mean) [vmean].

Nil data values are discarded before the calculation.
  (interactive)"
  (unless (datagridp datagrid)
    (error "Argument must be a datagrid"))
  (let* ((vec (if code
		  (datagrid-column-decode datagrid index)
		(datagrid-column-data (aref datagrid index))))
	 (vec (if convert
		  (datagrid-unknown-type-to-number vec)
		vec))
	 ;; Calc cannot handle nil values. Remove them. This also
	 ;; converts vec to a list. Because mathematicians call them
	 ;; vectors, that is what Calc calls them even if they arrive
	 ;; to a Calc function in a list.
	 (vec (delq nil (append vec nil))))
    (when vec (datagrid-calc-function-wrapper func-abbrev vec))))


;;;; Statistical functions
(defun datagrid-column-frequencies (datagrid index &optional code)
  "Find the frequency of elements occuring in a column.
DATAGRID is the vector of structs. INDEX is the zero based column
number. If CODE is t, then decode data first. If nil, take code
as is. This works for strings or numbers."
  (unless (datagridp datagrid)
    (error "Argument must be a datagrid"))
  (let* ((vec (if code
		  (datagrid-column-decode datagrid index)
		(datagrid-column-data (aref datagrid index))))
	 (counts (make-hash-table :test 'equal)))
    ;; this loop changes the variable counts
    (cl-loop for item across vec
	     do (puthash item (1+ (gethash item counts 0)) counts))
    ;; Convert to alist for easier viewing
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) counts)
      (nreverse (seq-sort-by #'cdr #'datagrid-unknown-type-sort result)))))

(defun datagrid-column-quartiles (datagrid index &optional code)
  "Find the first, second, and third quartile of a column.
DATAGRID is the vector of structs. INDEX is the zero based column
number. If CODE is t, then decode data first. If nil, take code as is."
  (if-let* ((vec (if code
		     (datagrid-column-decode datagrid index)
		   (datagrid-column-data (aref datagrid index))))
	    ;; Are there more efficient ways to do this?
	    (vec (seq-into (seq-filter #'identity vec) 'vector))
	    (vec (seq-sort #'< vec))
	    (len (length vec))
	    (1Q (aref vec (/ (+ len 1) 4)))
	    (2Q (aref vec (/ (+ len 1) 2)))
	    (3Q (aref vec (/ (* 3 (+ len 1)) 4))))
      `(("1Q" . ,1Q)
	("2Q" . ,2Q)
	("3Q" . ,3Q)
	("IQR" . ,(- 3Q 1Q)))))

(defun datagrid-column-mode (datagrid index &optional code)
  "Find the mode, most often occurring item, of a column.
DATAGRID is a datagrid struct. INDEX is the zero based column
number. CODE is nil if the data is not coded and non-nil if it
is. The output is a list of one or more mode values. This works
for strings or numbers."
  (let ((frequencies (datagrid-column-frequencies datagrid index code)))
    (if (null frequencies)
        nil
      (let* ((sorted-frequencies (sort frequencies
                                       (lambda (a b) (> (cdr a) (cdr b)))))
             (high-freq (cdar sorted-frequencies)))
        (mapcar #'car
                (seq-filter (lambda (x) (= high-freq (cdr x)))
                            sorted-frequencies))))))

(defun datagrid-column-unique (datagrid index &optional code)
  "Return unique items from a column.
DATAGRID is the vector of structs. INDEX is the zero based column
number. If CODE is t, then decode data first. If nil, take code
as is."
  (interactive)
  (unless (datagridp datagrid)
    (error "Argument must be a datagrid"))
  (let ((vec (if code (datagrid-column-decode datagrid index)
	       (datagrid-column-data (aref datagrid index)))))
    (seq-uniq vec)))

(defun datagrid-column-mad (datagrid index &optional code)
  "Calculate the median absolute deviation.
DATAGRID is the vector of structs. INDEX is the zero based column
number. If CODE is t, then decode data first. If nil, take code
as is."
  (let* ((lst (delq nil (append (if code
				    (datagrid-column-decode datagrid index)
				  (datagrid-column-data (aref datagrid index)))
				nil)))
	 (lst-calc (seq-keep #'datagrid-prep-for-calc lst))
	 (median1 (datagrid-calc-function-wrapper "vmedian" lst-calc))
	 ;; It would be nice to do this in Calc formulas so the prep
	 ;; for Calc function doesn't have to done again.
	 (lst2 (mapcar (lambda (x) (abs (- x median1))) lst)))
    (datagrid-calc-function-wrapper
     "vmedian" (seq-keep #'datagrid-prep-for-calc lst2))))


;;;; Summary reports
(defun datagrid-report-nominal (datagrid index)
  "Display column statistics for nominal data.
DATAGRID is a vector of datagrid structures. INDEX is the column
to analyze. It uses zero based counting."
  (unless (datagridp datagrid)
    (error "Argument must be a datagrid"))
  (let ((freq (seq-take (datagrid-column-frequencies datagrid index) 5)))
    (append
     (list (datagrid-column-heading (elt datagrid index)))
     (list (cons "cardinality" (length freq)))
     (list (cons "mode" (datagrid-column-mode datagrid index)))
     (list (cons "frequency - five or fewer" freq)))))

(defun datagrid-report-ordinal (datagrid index &optional code convert)
  "Display column statistics for ordinal data.
DATAGRID is a vector of datagrid structures. INDEX is the column
to analyze. It uses zero based counting. If CODE is t then decode
the data before using the data. If CODE is nil take the data as
is. CONVERT tells the function to convert strings to numbers.

Mean and standard deviation are not included because this is
ordinal data."
  (unless (datagridp datagrid)
    (error "Argument must be a datagrid"))
  (let* ((stats-name '("vcount" "vmin" "vmax" "vmedian"))
	 (vec (if code
		  (datagrid-column-decode datagrid index)
		(datagrid-column-data (aref datagrid index))))
	 ;; Convert string to numbers and decimal numbers to Calc
	 ;; understandable number. Calc cannot handle nil values. It
	 ;; cannot divide by decimal numbers, instead it requires
	 ;; (float 25 -2) or something like that. seq-keep removes
	 ;; them too. This also converts vec to a list.
	 (lst (if convert
		  (seq-keep #'datagrid-prep-for-calc vec)
		(seq-keep #'identity vec)))
	 (stats (cl-loop for statn in stats-name
			 collect (datagrid-calc-function-wrapper statn lst)))
	 ;; Non-calc functions cannot use Calc format so reuse vec
	 ;; above plus convert strings to numbers.
	 (new-dg (vector (datagrid-column-make
			  :data (datagrid-unknown-type-to-number vec)))))
    (append
     (list (datagrid-column-heading (elt datagrid index)))
     (cl-mapcar #'cons stats-name stats)
     ;; Using vec rather than datagrid so 0 index.
     (list (cons "mode" (datagrid-column-mode new-dg 0)))
     (list (cons "quartiles" (datagrid-column-quartiles new-dg 0)))
     (list (cons "mean absolute deviation"
		 (datagrid-column-mad new-dg 0))))))

(defun datagrid-report-interval (datagrid index &optional code convert)
  "Display column statistics for interval data.
DATAGRID is a vector of datagrid structures. INDEX is the column
to analyze. It uses zero based counting. If CODE is t then decode
the data before using the data. If CODE is nil take the data as
is. CONVERT tells the function to convert strings to numbers."
  (unless (datagridp datagrid)
    (error "Argument must be a datagrid"))
  (let* ((stats-name '("vcount" "vmin" "vmax" "vmedian" "vmean"
		       "vsdev"))
	 (vec (if code
		  (datagrid-column-decode datagrid index)
		(datagrid-column-data (aref datagrid index))))
	 (lst (if convert
		  (seq-keep #'datagrid-prep-for-calc vec)
		(seq-keep #'identity vec)))
	 (stats (cl-loop for statn in stats-name
			 collect (datagrid-calc-function-wrapper statn lst)))
	 (new-dg (vector (datagrid-column-make
			  :data (datagrid-unknown-type-to-number vec)))))
    (append
     (list (datagrid-column-heading (elt datagrid index)))
     (cl-mapcar #'cons stats-name stats)
     ;; Using vec rather than datagrid so 0 index.
     (list (cons "mode" (datagrid-column-mode new-dg 0)))
     (list (cons "quartiles" (datagrid-column-quartiles new-dg 0)))
     )))

(defun datagrid-report-ratio (datagrid index &optional code convert)
  "Display column statistics for ratio data.
DATAGRID is a vector of datagrid structures. INDEX is the column
to analyze. It uses zero based counting. If CODE is t then decode
the data before using the data. If CODE is nil take the data as
is. CONVERT tells the function to convert strings to numbers.

RMS stands for root-mean-square or coefficient of variation."
  (unless (datagridp datagrid)
    (error "Argument must be a datagrid"))
  (let* ((stats-name '("vcount" "vmin" "vmax" "vmedian" "vmean" "vgmean"
		       "vsdev" "rms"))
	 (vec (if code
		  (datagrid-column-decode datagrid index)
		(datagrid-column-data (aref datagrid index))))
	 (lst (if convert
		  (seq-keep #'datagrid-prep-for-calc vec)
		(seq-keep #'identity vec)))
	 (stats (cl-loop for statn in stats-name
			 collect (datagrid-calc-function-wrapper statn lst)))
	 (new-dg (vector (datagrid-column-make
			  :data (datagrid-unknown-type-to-number vec)))))
    (append
     (list (datagrid-column-heading (elt datagrid index)))
     (cl-mapcar #'cons stats-name stats)
     ;; Using vec rather than datagrid so 0 index.
     (list (cons "mode" (datagrid-column-mode new-dg 0)))
     (list (cons "quartiles" (datagrid-column-quartiles new-dg 0)))
     )))

(defun datagrid-report-all-lom (datagrid)
  "Report on each column of data based on the level of measurement.
DATAGRID is a vector of datagrid structures.

This function will always decode a value if there is a code slot
provided for a datagrid-column struct. It will always convert strings to
Calc understandable numbers. This function will be slow for large data
sets.

This returns a cons structure. All columns with no specified level of
measurement are treated as nominal data."
  (cl-loop for x from 0 below (length datagrid)
	   collect (let ((code (if (datagrid-column-code (elt datagrid x))
				   t
				 nil))
			 (lom (datagrid-column-lom (elt datagrid x))))
		     (cons
		      (datagrid-column-heading (elt datagrid x))
		      (cond ((equal "ordinal" lom)
			     (datagrid-report-ordinal datagrid x code t))
			    ((equal "interval" lom)
			     (datagrid-report-interval datagrid x code t))
			    ((equal "ratio" lom)
			     (datagrid-report-ratio datagrid x code t))
			    (t
			     (datagrid-report-nominal datagrid x)))))))


;;; Provide
(provide 'datagrid)
;;; datagrid.el ends here
