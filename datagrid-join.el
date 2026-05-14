;;; datagrid-join.el --- Relational joins for datagrid -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Joshua Lambert
;; Author: Joshua Lambert <jlambert@missouristate.edu>
;; Maintainer: Joshua Lambert <jlambert@missouristate.edu>
;; Keywords: tools, convenience
;; Package-Requires: (cl-lib datagrid)

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; A dplyr-style join family for datagrid:
;;
;;   `datagrid-left-join'  - keep all rows of DG1; attach matching DG2 columns
;;   `datagrid-inner-join' - keep only rows of DG1 that match in DG2
;;   `datagrid-full-join'  - keep all rows from both grids
;;   `datagrid-anti-join'  - keep rows of DG1 with no match in DG2
;;   `datagrid-semi-join'  - keep rows of DG1 with a match in DG2 (no projection)
;;
;; All functions share the signature
;;
;;   (datagrid-X-join DG1 DG2 &key ON COLS SUFFIX)
;;
;; where ON identifies the join key, COLS selects which DG2 columns to
;; project (default: all DG2 columns except the key), and SUFFIX
;; disambiguates heading collisions. Anti and semi joins ignore COLS
;; and SUFFIX since they project no DG2 columns.
;;
;; ON accepts:
;;   - an integer: same column index in both grids
;;   - a string: heading present in both grids
;;   - a cons (X . Y): per-grid index or heading
;;
;; Keys are compared with `equal' (type-strict): "1" and 1 do not
;; match. Empty keys (nil or empty string) on either side never match.
;; When DG2 has duplicate keys, the first occurrence wins.

;;; Code:
(require 'cl-lib)
(require 'datagrid)

;;;; Internal helpers

(defun datagrid-join--empty-p (v)
  "Return non-nil if V should be treated as a missing key or value."
  (or (null v) (and (stringp v) (string-empty-p v))))

(defun datagrid-join--resolve-key (dg spec)
  "Resolve SPEC to a column index in DG.
SPEC is an integer (used as is) or a string (matched against
DG's headings)."
  (cond ((integerp spec) spec)
	((stringp spec)
	 (or (cl-position spec (datagrid-get-headings dg) :test #'equal)
	     (error "No column with heading %S in datagrid" spec)))
	(t (error "Bad join column spec: %S" spec))))

(defun datagrid-join--normalize-on (on dg1 dg2)
  "Resolve the :on argument ON into a cons (IDX1 . IDX2)."
  (unless on (error ":on is required"))
  (pcase on
    ((or (pred integerp) (pred stringp))
     (cons (datagrid-join--resolve-key dg1 on)
	   (datagrid-join--resolve-key dg2 on)))
    (`(,a . ,b)
     (cons (datagrid-join--resolve-key dg1 a)
	   (datagrid-join--resolve-key dg2 b)))
    (_ (error "Bad :on value: %S" on))))

(defun datagrid-join--build-index (vec)
  "Return hashtable mapping VEC values to first-occurrence index.
Empty values and later duplicates are skipped."
  (let ((ht (make-hash-table :test #'equal)))
    (cl-loop for v across vec for i from 0
	     unless (or (datagrid-join--empty-p v) (gethash v ht))
	     do (puthash v i ht))
    ht))

(defun datagrid-join--row-map (keys1 lookup)
  "Vector of dg2-index-or-nil for each key in KEYS1 via LOOKUP."
  (cl-map 'vector
	  (lambda (k)
	    (and (not (datagrid-join--empty-p k)) (gethash k lookup)))
	  keys1))

(defun datagrid-join--uniquify (heading used suffix)
  "Return HEADING modified so it does not collide with USED.
On first collision, append SUFFIX. On further collisions,
escalate with `_2', `_3', ... after SUFFIX."
  (cond ((null heading) heading)
	((not (member heading used)) heading)
	(t (let ((base (concat heading suffix)))
	     (if (not (member base used))
		 base
	       (cl-loop for n from 2
			for c = (format "%s_%d" base n)
			unless (member c used) return c))))))

(defun datagrid-join--default-cols (dg2 on2)
  "Logical column indices of DG2 except the key column ON2."
  (cl-loop for i from 0 below (datagrid--ncols dg2)
	   unless (= i on2) collect i))

(defun datagrid-join--project (dg-base dg2 cols row-map suffix)
  "Append projected DG2 COLS to DG-BASE, aligned via ROW-MAP.
COLS is a list of logical DG2 column indices. ROW-MAP is a vector
whose length is the number of logical rows in DG-BASE, each
element a logical dg2 row index or nil. Returns a new datagrid
with DG2's selected columns appended (heading, lom, code
preserved; headings uniquified using SUFFIX)."
  (let* ((used (cl-loop for h across (datagrid-get-headings dg-base)
                        when h collect h))
	 (result dg-base))
    (dolist (idx cols result)
      (let* ((src (aref (datagrid-columns dg2) (datagrid--col-at dg2 idx)))
	     (data (datagrid-pull dg2 idx))
	     (heading (datagrid-join--uniquify
		       (datagrid-column-heading src) used suffix)))
	(when heading (push heading used))
	(setq result
	      (datagrid-add-column
	       result
	       (datagrid-column-make
		:heading heading
		:lom (datagrid-column-lom src)
		:code (datagrid-column-code src)
		:data (cl-map 'vector
			      (lambda (i) (and i (aref data i)))
			      row-map))))))))

(defun datagrid-join--filter-rows (dg1 dg2 on keep-matched)
  "Filter DG1 rows by whether their key matches in DG2.
ON is the :on argument; KEEP-MATCHED non-nil keeps matched rows
(semi-join), nil keeps unmatched rows (anti-join)."
  (let* ((on (datagrid-join--normalize-on on dg1 dg2))
	 (on1 (car on)) (on2 (cdr on))
	 (lookup (datagrid-join--build-index (datagrid-pull dg2 on2)))
	 (mask (cl-map 'vector
		       (lambda (k)
			 (let ((m (and (not (datagrid-join--empty-p k))
				       (gethash k lookup))))
			   (if keep-matched m (not m))))
		       (datagrid-pull dg1 on1))))
    (datagrid-filter-by-mask dg1 mask)))

;;;; Public API

(cl-defun datagrid-left-join (dg1 dg2 &key on cols suffix)
  "Return DG1 with matching DG2 columns appended.
All rows of DG1 are kept. For each row, the projected DG2
columns hold the value from the matching DG2 row, or nil if no
match.

ON identifies the join key. It accepts an integer (same column
index in both grids), a string (heading present in both grids),
or a cons (X . Y) for per-grid index or heading. Keys are
compared with `equal'; empty keys (nil or empty string) never
match. When DG2 has duplicate keys, the first occurrence wins.

COLS is a list of DG2 column indices or headings to project. It
defaults to every DG2 column except the join key. SUFFIX
disambiguates DG2 headings that collide with DG1 headings and
defaults to \"_2\"."
  (let* ((suffix (or suffix "_2"))
	 (on (datagrid-join--normalize-on on dg1 dg2))
	 (on1 (car on)) (on2 (cdr on))
	 (cols (cl-loop for c in (or cols (datagrid-join--default-cols dg2 on2))
			collect (datagrid-join--resolve-key dg2 c)))
	 (lookup (datagrid-join--build-index (datagrid-pull dg2 on2)))
	 (row-map (datagrid-join--row-map (datagrid-pull dg1 on1) lookup)))
    (datagrid-join--project dg1 dg2 cols row-map suffix)))

(cl-defun datagrid-inner-join (dg1 dg2 &key on cols suffix)
  "Return only the rows of DG1 that have a match in DG2, with DG2 columns appended.

ON identifies the join key: an integer (same column index in
both grids), a string (heading present in both), or a cons
(X . Y) for per-grid index or heading. Keys are compared with
`equal'; empty keys (nil or empty string) never match. When DG2
has duplicate keys, the first occurrence wins.

COLS is a list of DG2 column indices or headings to project,
defaulting to every DG2 column except the join key. SUFFIX
disambiguates colliding headings and defaults to \"_2\"."
  (let* ((suffix (or suffix "_2"))
	 (on (datagrid-join--normalize-on on dg1 dg2))
	 (on1 (car on)) (on2 (cdr on))
	 (cols (cl-loop for c in (or cols (datagrid-join--default-cols dg2 on2))
			collect (datagrid-join--resolve-key dg2 c)))
	 (lookup (datagrid-join--build-index (datagrid-pull dg2 on2)))
	 (row-map (datagrid-join--row-map (datagrid-pull dg1 on1) lookup))
	 (mask (cl-map 'vector (lambda (i) (if i t nil)) row-map))
	 (dg1-f (datagrid-filter-by-mask dg1 mask))
	 (rm-f (vconcat (cl-loop for i across row-map when i collect i))))
    (datagrid-join--project dg1-f dg2 cols rm-f suffix)))

(cl-defun datagrid-full-join (dg1 dg2 &key on cols suffix)
  "Return the union of DG1 and DG2 by key.
All rows of DG1 are kept (with matching DG2 columns appended,
nil for unmatched rows); then rows of DG2 whose key is absent
from DG1 are appended at the bottom. For these extra rows,
DG1's join column receives the DG2 key, DG1's other columns
receive nil, and the projected DG2 columns receive their actual
DG2 values.

ON identifies the join key: an integer (same column index in
both grids), a string (heading present in both), or a cons
(X . Y) for per-grid index or heading. Keys are compared with
`equal'; empty keys (nil or empty string) never match. When DG2
has duplicate keys, the first occurrence wins.

COLS is a list of DG2 column indices or headings to project,
defaulting to every DG2 column except the join key. SUFFIX
disambiguates colliding headings and defaults to \"_2\"."
  (let* ((suffix (or suffix "_2"))
	 (on (datagrid-join--normalize-on on dg1 dg2))
	 (on1 (car on)) (on2 (cdr on))
	 (cols (cl-loop for c in (or cols (datagrid-join--default-cols dg2 on2))
			collect (datagrid-join--resolve-key dg2 c)))
	 (keys2 (datagrid-pull dg2 on2))
	 (keys1 (datagrid-pull dg1 on1))
	 (lookup2 (datagrid-join--build-index keys2))
	 (lookup1 (datagrid-join--build-index keys1))
	 (row-map (datagrid-join--row-map keys1 lookup2))
	 (left (datagrid-join--project dg1 dg2 cols row-map suffix))
	 (extras (cl-loop for k across keys2 for i from 0
			  unless (or (datagrid-join--empty-p k)
				     (gethash k lookup1))
			  collect i))
	 (n-extra (length extras)))
    (if (zerop n-extra)
	left
      (let* ((extras-vec (vconcat extras))
             (left-cols (datagrid-columns left))
	     (n-dg1 (datagrid--ncols dg1))
	     (n-cols (length left-cols))
	     (result (make-vector n-cols nil)))
	(dotimes (col-idx n-cols)
	  (let* ((col (aref left-cols col-idx))
		 (orig-data (datagrid-column-data col))
		 (extension
		  (cond
		   ((= col-idx on1)
		    (cl-map 'vector (lambda (i) (aref keys2 i)) extras-vec))
		   ((< col-idx n-dg1)
		    (make-vector n-extra nil))
		   (t
		    (let* ((proj-idx (- col-idx n-dg1))
			   (src-idx (nth proj-idx cols))
			   (src-data (datagrid-pull dg2 src-idx)))
		      (cl-map 'vector (lambda (i) (aref src-data i))
			      extras-vec)))))
		 (new-col (datagrid-column-copy col)))
	    (setf (datagrid-column-data new-col)
		  (vconcat orig-data extension))
	    (setf (aref result col-idx) new-col)))
	(datagrid-make :columns result)))))

(cl-defun datagrid-anti-join (dg1 dg2 &key on)
  "Return rows of DG1 whose join key is absent from DG2.
Empty keys in DG1 (nil or empty string) are kept, since they
cannot match anything.

ON identifies the join key: an integer (same column index in
both grids), a string (heading present in both), or a cons
(X . Y) for per-grid index or heading. Keys are compared with
`equal'."
  (datagrid-join--filter-rows dg1 dg2 on nil))

(cl-defun datagrid-semi-join (dg1 dg2 &key on)
  "Return rows of DG1 whose join key is present in DG2.
No DG2 columns are projected.

ON identifies the join key: an integer (same column index in
both grids), a string (heading present in both), or a cons
(X . Y) for per-grid index or heading. Keys are compared with
`equal'; empty keys (nil or empty string) never match."
  (datagrid-join--filter-rows dg1 dg2 on t))

;;;; Deprecated

(defun datagrid-join (datagrid1 join-on-1 datagrid2 join-on-2 &rest dg2-indices)
  "Deprecated. Use `datagrid-left-join' instead."
  (declare (obsolete datagrid-left-join "1.0"))
  (datagrid-left-join datagrid1 datagrid2
		      :on (cons join-on-1 join-on-2)
		      :cols dg2-indices))

(provide 'datagrid-join)
;;; datagrid-join.el ends here
