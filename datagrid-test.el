;;; datagrid-test.el --- ERT tests for datagrid.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Joshua Lambert

;;; Code:
(require 'ert)
(add-to-list 'load-path
	     (file-name-directory (or load-file-name buffer-file-name)))
(require 'datagrid)


;;;; Fixtures

(defun dg-test--simple ()
  "Return a 3-column, 4-row datagrid."
  (vector
   (datagrid-column-make :heading "name"
			 :data ["Alice" "Bob" "Carol" "Dave"]
			 :lom "nominal")
   (datagrid-column-make :heading "score"
			 :data [10 20 30 40]
			 :lom "ratio")
   (datagrid-column-make :heading "group"
			 :data ["x" "y" "x" "y"]
			 :lom "nominal")))

(defun dg-test--coded ()
  "Return a single-column datagrid with a code alist."
  (vector
   (datagrid-column-make
    :heading "rating"
    :data ["Agree" "Disagree" "Neutral" "Agree"]
    :lom "ordinal"
    :code '(("Strongly disagree" . 1)
	    ("Disagree"          . 2)
	    ("Neutral"           . 3)
	    ("Agree"             . 4)
	    ("Strongly agree"    . 5)))))


;;;; datagrid-safe-transpose

(ert-deftest datagrid-test-safe-transpose-square ()
  (should (equal (datagrid-safe-transpose '((1 2) (3 4)))
		 '((1 3) (2 4)))))

(ert-deftest datagrid-test-safe-transpose-uneven-rows ()
  (should (equal (datagrid-safe-transpose '((1 2 3) (4 5)))
		 '((1 4) (2 5) (3 nil)))))

(ert-deftest datagrid-test-safe-transpose-vectors ()
  (should (equal (datagrid-safe-transpose (list [1 2] [3 4]))
		 '((1 3) (2 4)))))


;;;; datagrid-unknown-type-to-number

(ert-deftest datagrid-test-unknown-type-to-number-strings ()
  (should (equal (datagrid-unknown-type-to-number ["1" "2" "3"])
		 [1 2 3])))

(ert-deftest datagrid-test-unknown-type-to-number-mixed ()
  (should (equal (datagrid-unknown-type-to-number [1 "2" nil])
		 [1 2 nil])))

(ert-deftest datagrid-test-unknown-type-to-number-empty-string ()
  (should (equal (datagrid-unknown-type-to-number [""])
		 [nil])))


;;;; datagrid-unknown-type-sort

(ert-deftest datagrid-test-unknown-type-sort-numbers ()
  (should (datagrid-unknown-type-sort 1 2))
  (should-not (datagrid-unknown-type-sort 2 1)))

(ert-deftest datagrid-test-unknown-type-sort-strings ()
  (should (datagrid-unknown-type-sort "apple" "banana"))
  (should-not (datagrid-unknown-type-sort "banana" "apple")))


;;;; datagrid-column-p

(ert-deftest datagrid-test-column-p-valid-full ()
  (should (datagrid-column-p
	   (datagrid-column-make :heading "h" :data [1 2] :lom "ratio"
				 :code '(("a" . 1))))))

(ert-deftest datagrid-test-column-p-valid-minimal ()
  (should (datagrid-column-p (datagrid-column-make :data [1]))))

(ert-deftest datagrid-test-column-p-rejects-plain-vector ()
  (should-not (datagrid-column-p [1 2 3])))

(ert-deftest datagrid-test-column-p-rejects-non-struct ()
  (should-not (datagrid-column-p "not a column"))
  (should-not (datagrid-column-p nil)))


;;;; datagrid-column-set-length

(ert-deftest datagrid-test-column-set-length-truncate ()
  (let* ((col (datagrid-column-make :heading "x" :data [1 2 3 4 5]))
	 (result (datagrid-column-set-length col 3)))
    (should (equal (datagrid-column-data result) [1 2 3]))))

(ert-deftest datagrid-test-column-set-length-extend ()
  (let* ((col (datagrid-column-make :heading "x" :data [1 2]))
	 (result (datagrid-column-set-length col 4)))
    (should (equal (datagrid-column-data result) [1 2 nil nil]))))

(ert-deftest datagrid-test-column-set-length-same-returns-original ()
  (let* ((col (datagrid-column-make :heading "x" :data [1 2 3]))
	 (result (datagrid-column-set-length col 3)))
    (should (eq result col))))

(ert-deftest datagrid-test-column-set-length-preserves-metadata ()
  (let* ((col (datagrid-column-make :heading "h" :data [1 2 3]
				    :lom "ratio" :code '(("a" . 1))))
	 (result (datagrid-column-set-length col 2)))
    (should (equal (datagrid-column-heading result) "h"))
    (should (equal (datagrid-column-lom result) "ratio"))
    (should (equal (datagrid-column-code result) '(("a" . 1))))))


;;;; datagridp

(ert-deftest datagrid-test-datagridp-valid ()
  (should (datagridp (dg-test--simple))))

(ert-deftest datagrid-test-datagridp-empty-vector ()
  (should-not (datagridp [])))

(ert-deftest datagrid-test-datagridp-unequal-column-lengths ()
  (should-not (datagridp
	       (vector (datagrid-column-make :data [1 2 3])
		       (datagrid-column-make :data [1 2])))))

(ert-deftest datagrid-test-datagridp-plain-vector ()
  (should-not (datagridp [1 2 3])))

(ert-deftest datagrid-test-datagridp-nil ()
  (should-not (datagridp nil)))


;;;; datagrid-from-alist

(ert-deftest datagrid-test-from-alist-with-headings ()
  (let* ((alist '(("x" . (1 2 3)) ("y" . (4 5 6))))
	 (dg (datagrid-from-alist alist t)))
    (should (datagridp dg))
    (should (equal (datagrid-column-heading (aref dg 0)) "x"))
    (should (equal (datagrid-column-data (aref dg 0)) [1 2 3]))))

(ert-deftest datagrid-test-from-alist-no-headings ()
  (let* ((alist '((1 2 3) (4 5 6)))
	 (dg (datagrid-from-alist alist nil)))
    (should (datagridp dg))
    (should (null (datagrid-column-heading (aref dg 0))))))

(ert-deftest datagrid-test-from-alist-extend-uneven ()
  (let* ((alist '(("a" . (1 2 3)) ("b" . (4 5))))
	 (dg (datagrid-from-alist alist t t)))
    (should (= (length (datagrid-column-data (aref dg 1))) 3))
    (should (null (aref (datagrid-column-data (aref dg 1)) 2)))))

(ert-deftest datagrid-test-from-alist-truncate-uneven ()
  (let* ((alist '(("a" . (1 2 3)) ("b" . (4 5))))
	 (dg (datagrid-from-alist alist t nil)))
    (should (= (length (datagrid-column-data (aref dg 0))) 2))
    (should (= (length (datagrid-column-data (aref dg 1))) 2))))


;;;; datagrid-make

(ert-deftest datagrid-test-make-dimensions ()
  (let ((dg (datagrid-make 3 4)))
    (should (= (length dg) 3))
    (should (= (length (datagrid-column-data (aref dg 0))) 4))))

(ert-deftest datagrid-test-make-init-value ()
  (let ((dg (datagrid-make 2 3 0)))
    (should (equal (datagrid-column-data (aref dg 0)) [0 0 0]))
    (should (equal (datagrid-column-data (aref dg 1)) [0 0 0]))))

(ert-deftest datagrid-test-make-with-headings ()
  (let ((dg (datagrid-make 2 3 nil '("col1" "col2"))))
    (should (equal (datagrid-column-heading (aref dg 0)) "col1"))
    (should (equal (datagrid-column-heading (aref dg 1)) "col2"))))

(ert-deftest datagrid-test-make-nil-headings-by-default ()
  (let ((dg (datagrid-make 2 2)))
    (should (null (datagrid-column-heading (aref dg 0))))))


;;;; datagrid-from-vectors

(ert-deftest datagrid-test-from-vectors-single ()
  (let ((dg (datagrid-from-vectors ["heading" 1 2 3])))
    (should (datagridp dg))
    (should (equal (datagrid-column-heading (aref dg 0)) "heading"))
    (should (equal (datagrid-column-data (aref dg 0)) [1 2 3]))))

(ert-deftest datagrid-test-from-vectors-multiple ()
  (let ((dg (datagrid-from-vectors ["a" 1 2] ["b" 3 4])))
    (should (= (length dg) 2))
    (should (equal (datagrid-column-heading (aref dg 0)) "a"))
    (should (equal (datagrid-column-heading (aref dg 1)) "b"))
    (should (equal (datagrid-column-data (aref dg 1)) [3 4]))))

(ert-deftest datagrid-test-from-vectors-errors-on-list ()
  (should-error (datagrid-from-vectors '("heading" 1 2 3))))


;;;; datagrid-dimensions

(ert-deftest datagrid-test-dimensions-basic ()
  (should (equal (datagrid-dimensions (dg-test--simple)) '(3 . 4))))

(ert-deftest datagrid-test-dimensions-single-column ()
  (let ((dg (vector (datagrid-column-make :data [1 2 3 4 5]))))
    (should (equal (datagrid-dimensions dg) '(1 . 5)))))


;;;; datagrid-get-elt

(ert-deftest datagrid-test-get-elt-various-positions ()
  (let ((dg (dg-test--simple)))
    (should (equal (datagrid-get-elt dg 0 0) "Alice"))
    (should (equal (datagrid-get-elt dg 1 2) 30))
    (should (equal (datagrid-get-elt dg 2 3) "y"))))


;;;; datagrid-get-col-data

(ert-deftest datagrid-test-get-col-data-basic ()
  (should (equal (datagrid-get-col-data (dg-test--simple) 1) [10 20 30 40])))


;;;; datagrid-get-row-data

(ert-deftest datagrid-test-get-row-data-first-row ()
  (should (equal (datagrid-get-row-data (dg-test--simple) 0)
		 ["Alice" 10 "x"])))

(ert-deftest datagrid-test-get-row-data-last-row ()
  (should (equal (datagrid-get-row-data (dg-test--simple) 3)
		 ["Dave" 40 "y"])))


;;;; datagrid-get-headings

(ert-deftest datagrid-test-get-headings-basic ()
  (should (equal (datagrid-get-headings (dg-test--simple))
		 ["name" "score" "group"])))

(ert-deftest datagrid-test-get-headings-nil-headings ()
  (let ((dg (vector (datagrid-column-make :data [1 2])
		    (datagrid-column-make :data [3 4]))))
    (should (equal (datagrid-get-headings dg) [nil nil]))))


;;;; datagrid-col-index-by-header

(ert-deftest datagrid-test-col-index-by-header-first ()
  (should (= (datagrid-col-index-by-header (dg-test--simple) "name") 0)))

(ert-deftest datagrid-test-col-index-by-header-middle ()
  (should (= (datagrid-col-index-by-header (dg-test--simple) "score") 1)))

(ert-deftest datagrid-test-col-index-by-header-not-found ()
  (should (null (datagrid-col-index-by-header (dg-test--simple) "missing"))))


;;;; datagrid-col-data-by-header

(ert-deftest datagrid-test-col-data-by-header-basic ()
  (should (equal (datagrid-col-data-by-header (dg-test--simple) "score")
		 [10 20 30 40])))


;;;; datagrid-column-decode

(ert-deftest datagrid-test-column-decode-with-code ()
  ;; "Agree"=4, "Disagree"=2, "Neutral"=3, "Agree"=4
  (should (equal (datagrid-column-decode (dg-test--coded) 0) [4 2 3 4])))

(ert-deftest datagrid-test-column-decode-missing-value-is-nil ()
  (let ((dg (vector (datagrid-column-make
		     :heading "x"
		     :data ["Agree" "Unknown"]
		     :code '(("Agree" . 4))))))
    (should (equal (datagrid-column-decode dg 0) [4 nil]))))

(ert-deftest datagrid-test-column-decode-no-code-returns-data ()
  (let ((dg (vector (datagrid-column-make :data [1 2 3]))))
    (should (equal (datagrid-column-decode dg 0) [1 2 3]))))


;;;; datagrid-column-add-code

(ert-deftest datagrid-test-column-add-code-single-index ()
  (let* ((dg (dg-test--simple))
	 (code '(("x" . 1) ("y" . 2))))
    (datagrid-column-add-code dg 2 code)
    (should (equal (datagrid-column-code (aref dg 2)) code))))

(ert-deftest datagrid-test-column-add-code-multiple-indices ()
  (let* ((dg (dg-test--simple))
	 (code '(("x" . 1) ("y" . 2))))
    (datagrid-column-add-code dg '(0 2) code)
    (should (equal (datagrid-column-code (aref dg 0)) code))
    (should (equal (datagrid-column-code (aref dg 2)) code))
    (should (null (datagrid-column-code (aref dg 1))))))


;;;; datagrid-head

(ert-deftest datagrid-test-head-defaults-to-5x5 ()
  (let* ((dg (datagrid-make 6 10 0))
	 (h (datagrid-head dg)))
    (should (= (length h) 5))
    (should (= (length (datagrid-column-data (aref h 0))) 5))))

(ert-deftest datagrid-test-head-limit-rows ()
  (let* ((h (datagrid-head (dg-test--simple) nil 2)))
    (should (= (length (datagrid-column-data (aref h 0))) 2))
    (should (equal (datagrid-column-data (aref h 0)) ["Alice" "Bob"]))))

(ert-deftest datagrid-test-head-limit-cols ()
  (let* ((h (datagrid-head (dg-test--simple) 2 nil)))
    (should (= (length h) 2))
    (should (equal (datagrid-column-heading (aref h 1)) "score"))))

(ert-deftest datagrid-test-head-fewer-cols-than-default ()
  "Should not error when datagrid has fewer than 5 columns."
  (let* ((dg (vector (datagrid-column-make :heading "a" :data [1 2])))
	 (h (datagrid-head dg)))
    (should (= (length h) 1))))

(ert-deftest datagrid-test-head-preserves-metadata ()
  (let* ((dg (dg-test--coded))
	 (h (datagrid-head dg)))
    (should (equal (datagrid-column-heading (aref h 0)) "rating"))
    (should (equal (datagrid-column-lom (aref h 0)) "ordinal"))
    (should (equal (datagrid-column-code (aref h 0))
		   (datagrid-column-code (aref dg 0))))))


;;;; datagrid-add-column

(ert-deftest datagrid-test-add-column-appends ()
  (let* ((dg (dg-test--simple))
	 (new-col (datagrid-column-make :heading "new" :data [1 2 3 4]))
	 (result (datagrid-add-column dg new-col)))
    (should (= (length result) 4))
    (should (equal (datagrid-column-heading (aref result 3)) "new"))
    (should (equal (datagrid-column-data (aref result 3)) [1 2 3 4]))))

(ert-deftest datagrid-test-add-column-truncates-long-data ()
  (let* ((dg (dg-test--simple))
	 (new-col (datagrid-column-make :heading "new" :data [1 2 3 4 5 6]))
	 (result (datagrid-add-column dg new-col)))
    (should (= (length (datagrid-column-data (aref result 3))) 4))))

(ert-deftest datagrid-test-add-column-extends-short-data ()
  (let* ((dg (dg-test--simple))
	 (new-col (datagrid-column-make :heading "new" :data [1 2]))
	 (result (datagrid-add-column dg new-col)))
    (should (= (length (datagrid-column-data (aref result 3))) 4))
    (should (null (aref (datagrid-column-data (aref result 3)) 3)))))

(ert-deftest datagrid-test-add-column-two-columns-order ()
  "Two added columns should appear in the order given.
NOTE: this test exposes a bug — the dolist/cons loop in
datagrid-add-column builds truncd in reverse, so two or more
added columns are appended in the wrong order."
  (let* ((dg (vector (datagrid-column-make :heading "a" :data [1 2])))
	 (col1 (datagrid-column-make :heading "b" :data [3 4]))
	 (col2 (datagrid-column-make :heading "c" :data [5 6]))
	 (result (datagrid-add-column dg col1 col2)))
    (should (equal (datagrid-column-heading (aref result 1)) "b"))
    (should (equal (datagrid-column-heading (aref result 2)) "c"))))


;;;; datagrid-add-row

(ert-deftest datagrid-test-add-row-basic ()
  (let* ((dg (vector
	      (datagrid-column-make :heading "a" :data [1 2])
	      (datagrid-column-make :heading "b" :data [3 4])))
	 (result (datagrid-add-row dg '((5 6) (7 8)))))
    ;; Row (5 6): a=5, b=6. Row (7 8): a=7, b=8.
    (should (= (length (datagrid-column-data (aref result 0))) 4))
    (should (equal (datagrid-column-data (aref result 0)) [1 2 5 7]))
    (should (equal (datagrid-column-data (aref result 1)) [3 4 6 8]))))

(ert-deftest datagrid-test-add-row-errors-on-wrong-column-count ()
  (let ((dg (dg-test--simple)))
    (should-error (datagrid-add-row dg '((1 2))))))


;;;; datagrid-set-headings

(ert-deftest datagrid-test-set-headings-all ()
  (let* ((dg (datagrid-make 3 2 nil '("a" "b" "c")))
	 (result (datagrid-set-headings dg '("x" "y" "z"))))
    (should (equal (datagrid-column-heading (aref result 0)) "x"))
    (should (equal (datagrid-column-heading (aref result 1)) "y"))
    (should (equal (datagrid-column-heading (aref result 2)) "z"))))


;;;; datagrid-remove-column

(ert-deftest datagrid-test-remove-column-first ()
  (let* ((result (datagrid-remove-column (dg-test--simple) 0)))
    (should (= (length result) 2))
    (should (equal (datagrid-column-heading (aref result 0)) "score"))))

(ert-deftest datagrid-test-remove-column-middle ()
  (let* ((result (datagrid-remove-column (dg-test--simple) 1)))
    (should (= (length result) 2))
    (should (equal (datagrid-column-heading (aref result 0)) "name"))
    (should (equal (datagrid-column-heading (aref result 1)) "group"))))

(ert-deftest datagrid-test-remove-column-last ()
  (let* ((result (datagrid-remove-column (dg-test--simple) 2)))
    (should (= (length result) 2))
    (should (equal (datagrid-column-heading (aref result 1)) "score"))))


;;;; datagrid-sort

(ert-deftest datagrid-test-sort-numeric-reorders-all-columns ()
  (let* ((dg (vector
	      (datagrid-column-make :heading "n" :data [3 1 4 1 5])
	      (datagrid-column-make :heading "s" :data ["c" "a" "d" "b" "e"])))
	 (result (datagrid-sort dg 0)))
    (should (equal (datagrid-column-data (aref result 0)) [1 1 3 4 5]))
    (should (equal (datagrid-column-data (aref result 1)) ["a" "b" "c" "d" "e"]))))

(ert-deftest datagrid-test-sort-string-column ()
  (let* ((dg (vector
	      (datagrid-column-make :heading "s" :data ["banana" "apple" "cherry"])
	      (datagrid-column-make :heading "n" :data [2 1 3])))
	 (result (datagrid-sort dg 0)))
    (should (equal (datagrid-column-data (aref result 0))
		   ["apple" "banana" "cherry"]))
    (should (equal (datagrid-column-data (aref result 1)) [1 2 3]))))

(ert-deftest datagrid-test-sort-does-not-mutate-original ()
  (let* ((dg (vector (datagrid-column-make :data [3 1 2])))
	 (original-data (copy-sequence (datagrid-column-data (aref dg 0))))
	 (_ (datagrid-sort dg 0)))
    (should (equal (datagrid-column-data (aref dg 0)) original-data))))


;;;; datagrid-join

(ert-deftest datagrid-test-join-basic-left-outer ()
  (let* ((dg1 (vector
	       (datagrid-column-make :heading "id" :data ["a" "b" "c"])
	       (datagrid-column-make :heading "val" :data [1 2 3])))
	 (dg2 (vector
	       (datagrid-column-make :heading "id" :data ["b" "c" "a"])
	       (datagrid-column-make :heading "label" :data ["bee" "cee" "ay"])))
	 (result (datagrid-join dg1 0 dg2 0 1)))
    (should (= (length result) 3))
    (should (equal (datagrid-column-data (aref result 2))
		   ["ay" "bee" "cee"]))))

(ert-deftest datagrid-test-join-unmatched-row-is-nil ()
  (let* ((dg1 (vector
	       (datagrid-column-make :heading "id" :data ["a" "x" "c"])))
	 (dg2 (vector
	       (datagrid-column-make :heading "id" :data ["a" "c"])
	       (datagrid-column-make :heading "v" :data [10 30])))
	 (result (datagrid-join dg1 0 dg2 0 1)))
    (should (null (aref (datagrid-column-data (aref result 1)) 1)))))


;;;; datagrid-create-mask

(ert-deftest datagrid-test-create-mask-numeric-predicate ()
  (let* ((dg (dg-test--simple))
	 (mask (datagrid-create-mask dg (lambda (x) (> x 15)) 1)))
    (should (equal mask [nil t t t]))))

(ert-deftest datagrid-test-create-mask-string-predicate ()
  (let* ((dg (dg-test--simple))
	 (mask (datagrid-create-mask dg (lambda (x) (string-equal x "x")) 2)))
    (should (equal mask [t nil t nil]))))


;;;; datagrid-filter-by-mask

(ert-deftest datagrid-test-filter-by-mask-basic ()
  (let* ((dg (dg-test--simple))
	 (result (datagrid-filter-by-mask dg [t nil t nil])))
    (should (datagridp result))
    (should (= (length (datagrid-column-data (aref result 0))) 2))
    (should (equal (datagrid-column-data (aref result 0)) ["Alice" "Carol"]))
    (should (equal (datagrid-column-data (aref result 1)) [10 30]))))

(ert-deftest datagrid-test-filter-by-mask-all-false ()
  (let* ((result (datagrid-filter-by-mask (dg-test--simple) [nil nil nil nil])))
    (should (= (length (datagrid-column-data (aref result 0))) 0))))

(ert-deftest datagrid-test-filter-by-mask-all-true ()
  (let* ((dg (dg-test--simple))
	 (result (datagrid-filter-by-mask dg [t t t t])))
    (should (equal (datagrid-column-data (aref result 0))
		   (datagrid-column-data (aref dg 0))))))

(ert-deftest datagrid-test-filter-by-mask-accepts-list-mask ()
  "Mask passed as a list should be converted to vector automatically."
  (let* ((result (datagrid-filter-by-mask (dg-test--simple) '(t nil t nil))))
    (should (= (length (datagrid-column-data (aref result 0))) 2))))

(ert-deftest datagrid-test-filter-by-mask-preserves-metadata ()
  (let* ((dg (dg-test--coded))
	 (result (datagrid-filter-by-mask dg [t nil t nil])))
    (should (equal (datagrid-column-heading (aref result 0)) "rating"))
    (should (equal (datagrid-column-lom (aref result 0)) "ordinal"))))


;;;; datagrid-group-by

(ert-deftest datagrid-test-group-by-two-groups ()
  (let* ((dg (dg-test--simple))
	 (result (datagrid-group-by dg 2)))
    (should (= (length result) 2))
    (let ((group-x (cl-find "x" result :key #'car :test #'equal))
	  (group-y (cl-find "y" result :key #'car :test #'equal)))
      (should group-x)
      (should group-y)
      (should (equal (aref (cadr group-x) 0) ["Alice" "Carol"]))
      (should (equal (aref (cadr group-y) 0) ["Bob" "Dave"])))))

(ert-deftest datagrid-test-group-by-single-group ()
  (let* ((dg (vector
	      (datagrid-column-make :heading "g" :data ["a" "a" "a"])
	      (datagrid-column-make :heading "v" :data [1 2 3])))
	 (result (datagrid-group-by dg 0)))
    (should (= (length result) 1))
    (should (equal (car (aref result 0)) "a"))
    (should (equal (aref (cadr (aref result 0)) 1) [1 2 3]))))

(ert-deftest datagrid-test-group-by-preserves-row-order ()
  (let* ((dg (vector
	      (datagrid-column-make :heading "g" :data ["a" "b" "a" "b"])
	      (datagrid-column-make :heading "v" :data [1 2 3 4])))
	 (result (datagrid-group-by dg 0))
	 (group-a (cl-find "a" result :key #'car :test #'equal)))
    (should (equal (aref (cadr group-a) 1) [1 3]))))


;;;; datagrid-reduce-vec

(ert-deftest datagrid-test-reduce-vec-sum ()
  (should (= (datagrid-reduce-vec (dg-test--simple) #'+ 1) 100)))

(ert-deftest datagrid-test-reduce-vec-with-code ()
  ;; "Agree"=4 "Disagree"=2 "Neutral"=3 "Agree"=4 → sum=13
  (should (= (datagrid-reduce-vec (dg-test--coded) #'+ 0 t) 13)))

(ert-deftest datagrid-test-reduce-vec-with-convert ()
  (let ((dg (vector (datagrid-column-make :data ["10" "20" "30"]))))
    (should (= (datagrid-reduce-vec dg #'+ 0 nil t) 60))))


;;;; datagrid-prep-for-calc

(ert-deftest datagrid-test-prep-for-calc-integer ()
  (should (= (datagrid-prep-for-calc 42) 42)))

(ert-deftest datagrid-test-prep-for-calc-string-number ()
  (should (= (datagrid-prep-for-calc "42") 42)))

(ert-deftest datagrid-test-prep-for-calc-nil-returns-nil ()
  (should (null (datagrid-prep-for-calc nil))))

(ert-deftest datagrid-test-prep-for-calc-empty-string-returns-nil ()
  (should (null (datagrid-prep-for-calc ""))))

(ert-deftest datagrid-test-prep-for-calc-float-returns-calc-format ()
  (let ((result (datagrid-prep-for-calc 0.25)))
    (should (listp result))
    (should (eq (car result) 'float))))


;;;; datagrid-calc-function-wrapper

(ert-deftest datagrid-test-calc-function-wrapper-sum ()
  (should (= (datagrid-calc-function-wrapper "vsum" '(1 2 3 4 5)) 15.0)))

(ert-deftest datagrid-test-calc-function-wrapper-mean ()
  (should (= (datagrid-calc-function-wrapper "vmean" '(1 2 3 4 5)) 3.0)))

(ert-deftest datagrid-test-calc-function-wrapper-median-odd ()
  (should (= (datagrid-calc-function-wrapper "vmedian" '(1 2 3 4 5)) 3.0)))

(ert-deftest datagrid-test-calc-function-wrapper-median-even ()
  "Even-length data produces a fractional median — tests the math-float fix."
  (should (= (datagrid-calc-function-wrapper "vmedian" '(1 2 3 4)) 2.5)))

(ert-deftest datagrid-test-calc-function-wrapper-max ()
  (should (= (datagrid-calc-function-wrapper "vmax" '(3 1 4 1 5 9)) 9.0)))

(ert-deftest datagrid-test-calc-function-wrapper-count ()
  (should (= (datagrid-calc-function-wrapper "vcount" '(1 2 3)) 3.0)))


;;;; datagrid-reduce-vec-calc

(ert-deftest datagrid-test-reduce-vec-calc-mean ()
  (should (= (datagrid-reduce-vec-calc (dg-test--simple) "vmean" 1) 25.0)))

(ert-deftest datagrid-test-reduce-vec-calc-with-code ()
  ;; Decoded: [4 2 3 4], mean = 13/4 = 3.25
  (should (= (datagrid-reduce-vec-calc (dg-test--coded) "vmean" 0 t) 3.25)))

(ert-deftest datagrid-test-reduce-vec-calc-nil-returns-nil-on-empty ()
  (let ((dg (vector (datagrid-column-make :data [nil nil nil]))))
    (should (null (datagrid-reduce-vec-calc dg "vmean" 0 nil t)))))


;;;; datagrid-column-frequencies

(ert-deftest datagrid-test-column-frequencies-basic ()
  (let* ((dg (vector (datagrid-column-make :data ["a" "b" "a" "c" "a"])))
	 (freq (datagrid-column-frequencies dg 0)))
    (should (= (cdr (assoc "a" freq)) 3))
    (should (= (cdr (assoc "b" freq)) 1))
    (should (= (cdr (assoc "c" freq)) 1))))

(ert-deftest datagrid-test-column-frequencies-sorted-descending ()
  (let* ((dg (vector (datagrid-column-make :data ["a" "b" "a" "b" "b"])))
	 (freq (datagrid-column-frequencies dg 0)))
    (should (= (cdar freq) 3))
    (should (>= (cdr (nth 0 freq)) (cdr (nth 1 freq))))))

(ert-deftest datagrid-test-column-frequencies-with-code ()
  (let* ((freq (datagrid-column-frequencies (dg-test--coded) 0 t)))
    (should (= (cdr (assoc 4 freq)) 2))
    (should (= (cdr (assoc 2 freq)) 1))
    (should (= (cdr (assoc 3 freq)) 1))))


;;;; datagrid-column-quartiles

(ert-deftest datagrid-test-column-quartiles-returns-four-keys ()
  (let* ((dg (vector (datagrid-column-make :data [1 3 5 7 9 11 13 15])))
	 (q (datagrid-column-quartiles dg 0)))
    (should (assoc "1Q" q))
    (should (assoc "2Q" q))
    (should (assoc "3Q" q))
    (should (assoc "IQR" q))))

(ert-deftest datagrid-test-column-quartiles-iqr-consistent ()
  "IQR must equal 3Q minus 1Q."
  (let* ((dg (vector (datagrid-column-make :data [1 3 5 7 9 11 13 15])))
	 (q (datagrid-column-quartiles dg 0)))
    (should (= (cdr (assoc "IQR" q))
	       (- (cdr (assoc "3Q" q)) (cdr (assoc "1Q" q)))))))

(ert-deftest datagrid-test-column-quartiles-known-values ()
  ;; [1 3 5 7 9 11 13 15] sorted, len=8
  ;; Q1: aref[(8+1)/4]=aref[2]=5, Q2: aref[4]=9, Q3: aref[6]=13
  (let* ((dg (vector (datagrid-column-make :data [15 1 9 3 13 5 11 7])))
	 (q (datagrid-column-quartiles dg 0)))
    (should (= (cdr (assoc "1Q" q)) 5))
    (should (= (cdr (assoc "2Q" q)) 9))
    (should (= (cdr (assoc "3Q" q)) 13))))

(ert-deftest datagrid-test-column-quartiles-all-nil-returns-nil ()
  (let* ((dg (vector (datagrid-column-make :data [nil nil nil]))))
    (should (null (datagrid-column-quartiles dg 0)))))

(ert-deftest datagrid-test-column-quartiles-small-column-no-error ()
  "Q3 index must be clamped so columns with len < 4 do not error."
  (let* ((dg (vector (datagrid-column-make :data [1 2 3]))))
    (should (datagrid-column-quartiles dg 0)))
  (let* ((dg (vector (datagrid-column-make :data [42]))))
    (should (datagrid-column-quartiles dg 0))))

(ert-deftest datagrid-test-column-quartiles-ignores-nil-values ()
  ;; Use 8 non-nil values so Q3 index stays in bounds.
  (let* ((dg (vector (datagrid-column-make :data [1 nil 3 5 nil 7 9 11 nil 13 15])))
	 (q (datagrid-column-quartiles dg 0)))
    (should q)))


;;;; datagrid-column-mode

(ert-deftest datagrid-test-column-mode-unimodal-string ()
  (let* ((dg (vector (datagrid-column-make :data ["a" "b" "a" "c" "a"]))))
    (should (equal (datagrid-column-mode dg 0) '("a")))))

(ert-deftest datagrid-test-column-mode-unimodal-numeric ()
  (let* ((dg (vector (datagrid-column-make :data [1 2 2 3 3 3]))))
    (should (equal (datagrid-column-mode dg 0) '(3)))))

(ert-deftest datagrid-test-column-mode-empty-returns-nil ()
  (let* ((dg (vector (datagrid-column-make :data []))))
    (should (null (datagrid-column-mode dg 0)))))

(ert-deftest datagrid-test-column-mode-no-redundant-sort ()
  "datagrid-column-frequencies already sorts; mode should not re-sort."
  (let* ((dg (vector (datagrid-column-make :data ["z" "z" "a" "a" "a"])))
	 (mode (datagrid-column-mode dg 0)))
    (should (equal mode '("a")))))


;;;; datagrid-column-unique

(ert-deftest datagrid-test-column-unique-removes-duplicates ()
  (let* ((dg (vector (datagrid-column-make :data ["a" "b" "a" "c"])))
	 (uniq (datagrid-column-unique dg 0)))
    (should (= (length uniq) 3))
    (should (seq-contains-p uniq "a" #'equal))
    (should (seq-contains-p uniq "b" #'equal))
    (should (seq-contains-p uniq "c" #'equal))))

(ert-deftest datagrid-test-column-unique-no-duplicates-unchanged ()
  (let* ((dg (vector (datagrid-column-make :data [1 2 3 4])))
	 (uniq (datagrid-column-unique dg 0)))
    (should (= (length uniq) 4))))

(ert-deftest datagrid-test-column-unique-with-code ()
  (let* ((uniq (datagrid-column-unique (dg-test--coded) 0 t)))
    ;; Decoded: [4 2 3 4] → unique: 4, 2, 3
    (should (= (length uniq) 3))))


;;;; datagrid-column-mad

(ert-deftest datagrid-test-column-mad-basic ()
  ;; [1 2 3 4 5]: median=3, deviations=[2 1 0 1 2], MAD=1
  (let* ((dg (vector (datagrid-column-make :data [1 2 3 4 5]))))
    (should (= (datagrid-column-mad dg 0) 1.0))))

(ert-deftest datagrid-test-column-mad-string-data ()
  "String data should be converted to numbers without error."
  (let* ((dg (vector (datagrid-column-make :data ["1" "2" "3" "4" "5"]))))
    (should (= (datagrid-column-mad dg 0) 1.0))))


;;;; datagrid-to-vec-of-vec

(ert-deftest datagrid-test-to-vec-of-vec-structure ()
  (let* ((dg (dg-test--simple))
	 (vov (datagrid-to-vec-of-vec dg)))
    (should (vectorp vov))
    (should (= (length vov) 3))
    (should (equal (aref vov 0) ["Alice" "Bob" "Carol" "Dave"]))
    (should (equal (aref vov 1) [10 20 30 40]))))


;;;; datagrid-to-alist

(ert-deftest datagrid-test-to-alist-without-headings ()
  (let* ((dg (vector (datagrid-column-make :heading "a" :data [1 2])
		     (datagrid-column-make :heading "b" :data [3 4])))
	 (result (datagrid-to-alist dg)))
    (should (equal result '((1 2) (3 4))))))

(ert-deftest datagrid-test-to-alist-with-headings ()
  (let* ((dg (vector (datagrid-column-make :heading "a" :data [1 2])
		     (datagrid-column-make :heading "b" :data [3 4])))
	 (result (datagrid-to-alist dg t)))
    (should (equal result '(("a" 1 2) ("b" 3 4))))))


;;;; Report functions
;; These pin the current Calc-backed behavior so refactors can be verified.

(defun dg-test--approx= (a b &optional tol)
  "Return non-nil if A and B differ by less than TOL (default 1e-6)."
  (< (abs (- a b)) (or tol 1e-6)))

(defun dg-test--ratio-dg ()
  "Single ratio column with known values [1 2 3 4 5]."
  (vector (datagrid-column-make :heading "r"
				:data [1 2 3 4 5]
				:lom "ratio")))

(defun dg-test--interval-dg ()
  "Single interval column with known values [10 20 30 40]."
  (vector (datagrid-column-make :heading "i"
				:data [10 20 30 40]
				:lom "interval")))

(defun dg-test--ordinal-dg ()
  "Single ordinal column with known values [1 2 2 3 4 5]."
  (vector (datagrid-column-make :heading "o"
				:data [1 2 2 3 4 5]
				:lom "ordinal")))


;;;; datagrid-report-nominal

(ert-deftest datagrid-test-report-nominal-heading-first ()
  (let ((report (datagrid-report-nominal (dg-test--simple) 0)))
    (should (equal (car report) "name"))))

(ert-deftest datagrid-test-report-nominal-cardinality ()
  (let ((report (datagrid-report-nominal (dg-test--simple) 2)))
    ;; "x" "y" "x" "y" → 2 unique
    (should (= (cdr (assoc "cardinality" report)) 2))))

(ert-deftest datagrid-test-report-nominal-mode ()
  (let* ((dg (vector (datagrid-column-make :heading "h"
					   :data ["a" "b" "a" "c" "a"]
					   :lom "nominal")))
	 (report (datagrid-report-nominal dg 0)))
    (should (equal (cdr (assoc "mode" report)) '("a")))))

(ert-deftest datagrid-test-report-nominal-frequency-key-present ()
  (let ((report (datagrid-report-nominal (dg-test--simple) 2)))
    (should (assoc "frequency - five or fewer" report))))


;;;; datagrid-report-ordinal

(ert-deftest datagrid-test-report-ordinal-heading-first ()
  (let ((report (datagrid-report-ordinal (dg-test--ordinal-dg) 0)))
    (should (equal (car report) "o"))))

(ert-deftest datagrid-test-report-ordinal-stats-keys ()
  (let ((report (datagrid-report-ordinal (dg-test--ordinal-dg) 0)))
    (dolist (k '("vcount" "vmin" "vmax" "vmedian" "mode" "quartiles"
		 "mean absolute deviation"))
      (should (assoc k report)))))

(ert-deftest datagrid-test-report-ordinal-stat-values ()
  (let ((report (datagrid-report-ordinal (dg-test--ordinal-dg) 0)))
    (should (= (cdr (assoc "vcount" report)) 6.0))
    (should (= (cdr (assoc "vmin" report)) 1.0))
    (should (= (cdr (assoc "vmax" report)) 5.0))
    (should (= (cdr (assoc "vmedian" report)) 2.5))))

(ert-deftest datagrid-test-report-ordinal-mode-value ()
  (let ((report (datagrid-report-ordinal (dg-test--ordinal-dg) 0)))
    (should (equal (cdr (assoc "mode" report)) '(2)))))

(ert-deftest datagrid-test-report-ordinal-no-mean-or-sdev ()
  "Ordinal report should NOT include vmean or vsdev."
  (let ((report (datagrid-report-ordinal (dg-test--ordinal-dg) 0)))
    (should-not (assoc "vmean" report))
    (should-not (assoc "vsdev" report))))

(ert-deftest datagrid-test-report-ordinal-with-code ()
  ;; Decoded: [4 2 3 4]
  (let ((report (datagrid-report-ordinal (dg-test--coded) 0 t)))
    (should (= (cdr (assoc "vcount" report)) 4.0))
    (should (= (cdr (assoc "vmin" report)) 2.0))
    (should (= (cdr (assoc "vmax" report)) 4.0))))

(ert-deftest datagrid-test-report-ordinal-with-convert ()
  (let* ((dg (vector (datagrid-column-make :heading "s"
					   :data ["1" "2" "3" "4" "5"]
					   :lom "ordinal")))
	 (report (datagrid-report-ordinal dg 0 nil t)))
    (should (= (cdr (assoc "vcount" report)) 5.0))
    (should (= (cdr (assoc "vmedian" report)) 3.0))))


;;;; datagrid-report-interval

(ert-deftest datagrid-test-report-interval-heading-first ()
  (let ((report (datagrid-report-interval (dg-test--interval-dg) 0)))
    (should (equal (car report) "i"))))

(ert-deftest datagrid-test-report-interval-stats-keys ()
  (let ((report (datagrid-report-interval (dg-test--interval-dg) 0)))
    (dolist (k '("vcount" "vmin" "vmax" "vmedian" "vmean" "vsdev"
		 "mode" "quartiles"))
      (should (assoc k report)))))

(ert-deftest datagrid-test-report-interval-stat-values ()
  (let ((report (datagrid-report-interval (dg-test--interval-dg) 0)))
    (should (= (cdr (assoc "vcount" report)) 4.0))
    (should (= (cdr (assoc "vmin" report)) 10.0))
    (should (= (cdr (assoc "vmax" report)) 40.0))
    (should (= (cdr (assoc "vmedian" report)) 25.0))
    (should (= (cdr (assoc "vmean" report)) 25.0))
    ;; sample sdev of [10 20 30 40]: sqrt(500/3) ≈ 12.9099
    (should (dg-test--approx= (cdr (assoc "vsdev" report)) 12.909944487 1e-4))))

(ert-deftest datagrid-test-report-interval-no-gmean-or-rms ()
  "Interval report should NOT include vgmean or rms."
  (let ((report (datagrid-report-interval (dg-test--interval-dg) 0)))
    (should-not (assoc "vgmean" report))
    (should-not (assoc "rms" report))))


;;;; datagrid-report-ratio

(ert-deftest datagrid-test-report-ratio-heading-first ()
  (let ((report (datagrid-report-ratio (dg-test--ratio-dg) 0)))
    (should (equal (car report) "r"))))

(ert-deftest datagrid-test-report-ratio-stats-keys ()
  (let ((report (datagrid-report-ratio (dg-test--ratio-dg) 0)))
    (dolist (k '("vcount" "vmin" "vmax" "vmedian" "vmean" "vgmean"
		 "vsdev" "rms" "mode" "quartiles"))
      (should (assoc k report)))))

(ert-deftest datagrid-test-report-ratio-stat-values ()
  (let ((report (datagrid-report-ratio (dg-test--ratio-dg) 0)))
    (should (= (cdr (assoc "vcount" report)) 5.0))
    (should (= (cdr (assoc "vmin" report)) 1.0))
    (should (= (cdr (assoc "vmax" report)) 5.0))
    (should (= (cdr (assoc "vmedian" report)) 3.0))
    (should (= (cdr (assoc "vmean" report)) 3.0))
    ;; sample sdev of [1..5] = sqrt(2.5) ≈ 1.5811388
    (should (dg-test--approx= (cdr (assoc "vsdev" report)) 1.5811388 1e-4))
    ;; gmean of [1..5] = 120^(1/5) ≈ 2.6051710
    (should (dg-test--approx= (cdr (assoc "vgmean" report)) 2.6051710 1e-4))
    ;; rms of [1..5] = sqrt(55/5) = sqrt(11) ≈ 3.3166247
    (should (dg-test--approx= (cdr (assoc "rms" report)) 3.3166247 1e-4))))

(ert-deftest datagrid-test-report-ratio-with-convert ()
  (let* ((dg (vector (datagrid-column-make :heading "s"
					   :data ["1" "2" "3" "4" "5"]
					   :lom "ratio")))
	 (report (datagrid-report-ratio dg 0 nil t)))
    (should (= (cdr (assoc "vmean" report)) 3.0))))


;;;; datagrid-report-all-lom dispatch

(ert-deftest datagrid-test-report-all-lom-dispatches-by-lom ()
  (let* ((dg (vector
	      (datagrid-column-make :heading "n"
				    :data ["a" "b" "a" "c" "b"]
				    :lom "nominal")
	      (datagrid-column-make :heading "o" :data [1 2 3 4 5]
				    :lom "ordinal")
	      (datagrid-column-make :heading "i" :data [10 20 30 40 50]
				    :lom "interval")
	      (datagrid-column-make :heading "r" :data [1.0 2.0 3.0 4.0 5.0]
				    :lom "ratio")))
	 (reports (datagrid-report-all-lom dg)))
    ;; nominal: has "cardinality"
    (should (assoc "cardinality" (cdr (nth 0 reports))))
    ;; ordinal: has "mean absolute deviation", no vmean
    (should (assoc "mean absolute deviation" (cdr (nth 1 reports))))
    (should-not (assoc "vmean" (cdr (nth 1 reports))))
    ;; interval: has vmean+vsdev, no vgmean
    (should (assoc "vmean" (cdr (nth 2 reports))))
    (should-not (assoc "vgmean" (cdr (nth 2 reports))))
    ;; ratio: has vgmean+rms
    (should (assoc "vgmean" (cdr (nth 3 reports))))
    (should (assoc "rms" (cdr (nth 3 reports))))))

(ert-deftest datagrid-test-report-all-lom-nil-treated-as-nominal ()
  (let* ((dg (vector (datagrid-column-make :heading "x"
					   :data ["a" "b" "a"]
					   :lom nil)))
	 (reports (datagrid-report-all-lom dg)))
    (should (assoc "cardinality" (cdr (nth 0 reports))))))


(provide 'datagrid-test)
;;; datagrid-test.el ends here
