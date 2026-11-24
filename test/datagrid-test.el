;;; datagrid-test.el --- ERT tests for datagrid.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Joshua Lambert

;;; Code:
(require 'ert)
(add-to-list 'load-path
             (file-name-directory (or load-file-name buffer-file-name)))
(require 'datagrid)
(require 'datagrid-join)
;; Load Calc so its display variables are special when the tests bind them.
(require 'calc)


;;;; Fixtures

(defun datagrid-test-mk (&rest cols)
  "Wrap COLS in a datagrid struct."
  (datagrid-make :columns (vconcat cols)))

(defun datagrid-test-simple ()
  "Return a 3-column, 4-row datagrid."
  (datagrid-test-mk
   (datagrid-column-make :heading "name"
                         :data ["Alice" "Bob" "Carol" "Dave"]
                         :lom "nominal")
   (datagrid-column-make :heading "score"
                         :data [10 20 30 40]
                         :lom "ratio")
   (datagrid-column-make :heading "group"
                         :data ["x" "y" "x" "y"]
                         :lom "nominal")))

(defun datagrid-test-coded ()
  "Return a single-column datagrid with a code alist."
  (datagrid-test-mk
   (datagrid-column-make
    :heading "rating"
    :data ["Agree" "Disagree" "Neutral" "Agree"]
    :lom "ordinal"
    :code '(("Strongly disagree" . 1)
            ("Disagree"          . 2)
            ("Neutral"           . 3)
            ("Agree"             . 4)
            ("Strongly agree"    . 5)))))

(defsubst datagrid-test-col (dg n)
  "Physical column N of datagrid DG."
  (aref (datagrid-columns dg) n))

(defsubst datagrid-test-ncols (dg)
  "Physical column count of datagrid DG."
  (length (datagrid-columns dg)))


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

(ert-deftest datagrid-test-safe-transpose-empty ()
  (should (null (datagrid-safe-transpose nil)))
  (should (null (datagrid-safe-transpose []))))


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

(ert-deftest datagrid-test-column-set-length-rejects-invalid ()
  ;; Regression: the `datagrid-column-p' check was a variable binding.
  (should-error (datagrid-column-set-length
                 (datagrid-column-make :heading 5 :data [1 2]) 3))
  (should-error (datagrid-column-set-length
                 (datagrid-column-make :data [1 2] :lom 'ratio) 3))
  (should-error (datagrid-column-set-length nil 3))
  (should-error (datagrid-column-set-length [1 2] 3)))

(ert-deftest datagrid-test-column-add-data-rejects-invalid ()
  (should-error (datagrid--column-add-data
                 (datagrid-column-make :heading 5 :data [1]) [2]))
  (should-error (datagrid--column-add-data nil [2]))
  (let ((out (datagrid--column-add-data
              (datagrid-column-make :heading "h" :data [1]) [2 3])))
    (should (equal (datagrid-column-data out) [1 2 3]))))

(ert-deftest datagrid-test-add-column-still-validates ()
  (should-error (datagrid-add-column (datagrid-test-simple)
                                     (datagrid-column-make :heading 5 :data [1]))))

(ert-deftest datagrid-test-column-set-length-preserves-metadata ()
  (let* ((col (datagrid-column-make :heading "h" :data [1 2 3]
                                    :lom "ratio" :code '(("a" . 1))))
         (result (datagrid-column-set-length col 2)))
    (should (equal (datagrid-column-heading result) "h"))
    (should (equal (datagrid-column-lom result) "ratio"))
    (should (equal (datagrid-column-code result) '(("a" . 1))))))

(ert-deftest datagrid-test-column-set-length-nil-data ()
  ;; Regression: a column with no data returned nil instead of a column.
  (let ((result (datagrid-column-set-length
                 (datagrid-column-make :heading "x") 3)))
    (should (datagrid-column-p result))
    (should (equal (datagrid-column-data result) [nil nil nil]))))

(ert-deftest datagrid-test-column-add-data-nil-data ()
  ;; Regression: a column with no data returned nil instead of a column.
  (let ((out (datagrid--column-add-data
              (datagrid-column-make :heading "h") [1 2])))
    (should (datagrid-column-p out))
    (should (equal (datagrid-column-data out) [1 2]))))

(ert-deftest datagrid-test-column-seq-subseq ()
  ;; Regression: omitting END signaled an error.
  (let ((col (datagrid-column-make :heading "h" :data [1 2 3 4])))
    (should (equal (datagrid-column-data (seq-subseq col 2)) [3 4]))
    (should (equal (datagrid-column-data (seq-subseq col 1 3)) [2 3]))
    (should (equal (datagrid-column-data (seq-subseq col -2)) [3 4]))
    (should (equal (datagrid-column-heading (seq-subseq col 1)) "h"))))


;;;; datagridp

(ert-deftest datagrid-test-datagridp-valid ()
  (should (datagridp (datagrid-test-simple))))

(ert-deftest datagrid-test-datagridp-empty-vector ()
  (should-not (datagridp [])))

(ert-deftest datagrid-test-datagridp-plain-vector ()
  (should-not (datagridp [1 2 3])))

(ert-deftest datagrid-test-datagridp-nil ()
  (should-not (datagridp nil)))


;;;; datagrid-from-alist

(ert-deftest datagrid-test-from-alist-with-headings ()
  (let* ((alist '(("x" . (1 2 3)) ("y" . (4 5 6))))
         (dg (datagrid-from-alist alist t)))
    (should (datagridp dg))
    (should (equal (datagrid-column-heading (datagrid-test-col dg 0)) "x"))
    (should (equal (datagrid-column-data (datagrid-test-col dg 0)) [1 2 3]))))

(ert-deftest datagrid-test-from-alist-no-headings ()
  (let* ((alist '((1 2 3) (4 5 6)))
         (dg (datagrid-from-alist alist nil)))
    (should (datagridp dg))
    (should (null (datagrid-column-heading (datagrid-test-col dg 0))))))

(ert-deftest datagrid-test-from-alist-extend-uneven ()
  (let* ((alist '(("a" . (1 2 3)) ("b" . (4 5))))
         (dg (datagrid-from-alist alist t t)))
    (should (= (length (datagrid-column-data (datagrid-test-col dg 1))) 3))
    (should (null (aref (datagrid-column-data (datagrid-test-col dg 1)) 2)))))

(ert-deftest datagrid-test-from-alist-unequal-lengths-error ()
  ;; Columns are never cut to the shortest. The error names the
  ;; lengths, by heading when there are headings.
  (let ((err (should-error (datagrid-from-alist '(("a" 1 2 3) ("b" 4 5)) t))))
    (should (string-match-p "((\"a\" . 3) (\"b\" . 2))"
                            (error-message-string err))))
  (should-error (datagrid-from-alist '((1 2 3) (4 5))))
  (should-error (datagrid-from-alist '((1 2 3) (4 5)) nil nil)))

(ert-deftest datagrid-test-from-alist-extend-uneven-no-headings ()
  (should (equal (datagrid-to-alist (datagrid-from-alist '((1 2 3) (4)) nil t))
                 '((1 2 3) (4 nil nil)))))

(ert-deftest datagrid-test-from-alist-keeps-column-order ()
  (should (equal (datagrid-get-headings
                  (datagrid-from-alist '(("x" 1) ("y" 2) ("z" 3)) t))
                 ["x" "y" "z"])))

(ert-deftest datagrid-test-from-alist-empty ()
  (dolist (headings '(nil t))
    (dolist (extend '(nil t))
      (let ((dg (datagrid-from-alist nil headings extend)))
        (should (datagridp dg))
        (should (equal (datagrid-dimensions dg) '(0 . 0)))))))


;;;; CSV readers

;; These need csv-mode or pcsv and are skipped when it is missing.

(declare-function csv-mode "csv-mode")

(defmacro datagrid-test-with-csv-buffer (text &rest body)
  "Run BODY in a temporary `csv-mode' buffer holding TEXT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (csv-mode)
     ,@body))

(defmacro datagrid-test-with-csv-file (var text &rest body)
  "Run BODY with VAR bound to a temporary CSV file holding TEXT."
  (declare (indent 2))
  `(let ((,var (make-temp-file "datagrid-test" nil ".csv" ,text)))
     (unwind-protect (progn ,@body)
       (delete-file ,var))))

(ert-deftest datagrid-test-from-csv-buffer-with-headings ()
  (skip-unless (require 'csv-mode nil t))
  (datagrid-test-with-csv-buffer "a,b\n1,2\n3,4\n"
    (let ((dg (datagrid-from-csv-buffer (current-buffer) t)))
      (should (equal (datagrid-get-headings dg) ["a" "b"]))
      (should (equal (datagrid-to-alist dg) '(("1" "3") ("2" "4")))))))

(ert-deftest datagrid-test-from-csv-buffer-without-headings ()
  (skip-unless (require 'csv-mode nil t))
  (datagrid-test-with-csv-buffer "a,b\n1,2\n"
    (let ((dg (datagrid-from-csv-buffer (current-buffer))))
      (should (equal (datagrid-get-headings dg) [nil nil]))
      (should (equal (datagrid-to-alist dg) '(("a" "1") ("b" "2")))))))

(ert-deftest datagrid-test-from-csv-buffer-pads-short-rows ()
  (skip-unless (require 'csv-mode nil t))
  (datagrid-test-with-csv-buffer "a,b,c\n1,2,3\n4\n"
    (let ((dg (datagrid-from-csv-buffer (current-buffer) t)))
      (should (equal (datagrid-to-alist dg t)
                     '(("a" "1" "4") ("b" "2" nil) ("c" "3" nil)))))))

(ert-deftest datagrid-test-from-csv-buffer-quoted-fields ()
  (skip-unless (require 'csv-mode nil t))
  (datagrid-test-with-csv-buffer "name,note\nx,\"a, b\"\n"
    (should (equal (datagrid-pull (datagrid-from-csv-buffer (current-buffer) t)
                                  "note")
                   ["a, b"]))))

(ert-deftest datagrid-test-from-csv-buffer-empty ()
  (skip-unless (require 'csv-mode nil t))
  (datagrid-test-with-csv-buffer ""
    (let ((dg (datagrid-from-csv-buffer (current-buffer) t)))
      (should (datagridp dg))
      (should (equal (datagrid-dimensions dg) '(0 . 0))))))

(ert-deftest datagrid-test-from-csv-file-csv-mode ()
  (skip-unless (require 'csv-mode nil t))
  (datagrid-test-with-csv-file f "a,b\r\n1,2\r\n"
    (let ((dg (datagrid-from-csv-file f t)))
      (should (equal (datagrid-get-headings dg) ["a" "b"]))
      (should (equal (datagrid-to-alist dg) '(("1") ("2")))))))

(ert-deftest datagrid-test-from-csv-file-pcsv ()
  (skip-unless (require 'pcsv nil t))
  (datagrid-test-with-csv-file f "a,b\n1,2\n"
    (let ((dg (datagrid-from-csv-file f t 'pcsv)))
      (should (equal (datagrid-get-headings dg) ["a" "b"]))
      (should (equal (datagrid-to-alist dg) '(("1") ("2")))))))

(ert-deftest datagrid-test-from-csv-file-pcsv-without-headings ()
  (skip-unless (require 'pcsv nil t))
  (datagrid-test-with-csv-file f "a,b\n1,2\n"
    (let ((dg (datagrid-from-csv-file f nil 'pcsv)))
      (should (equal (datagrid-get-headings dg) [nil nil]))
      (should (equal (datagrid-to-alist dg) '(("a" "1") ("b" "2")))))))

(ert-deftest datagrid-test-from-csv-file-pcsv-pads-short-rows ()
  (skip-unless (require 'pcsv nil t))
  (datagrid-test-with-csv-file f "a,b,c\n1,2,3\n4\n"
    (should (equal (datagrid-to-alist (datagrid-from-csv-file f t 'pcsv) t)
                   '(("a" "1" "4") ("b" "2" nil) ("c" "3" nil))))))

(ert-deftest datagrid-test-from-csv-file-pcsv-quoted-fields ()
  ;; A quoted field may hold a comma, a doubled quote, or a newline.
  (skip-unless (require 'pcsv nil t))
  (datagrid-test-with-csv-file f
      "name,note\nx,\"a, b\"\ny,\"say \"\"hi\"\"\"\nz,\"line 1\nline 2\"\n"
    (let ((dg (datagrid-from-csv-file f t 'pcsv)))
      (should (equal (datagrid-pull dg "name") ["x" "y" "z"]))
      (should (equal (datagrid-pull dg "note")
                     ["a, b" "say \"hi\"" "line 1\nline 2"])))))

(ert-deftest datagrid-test-from-csv-buffer-long-row-errors ()
  ;; The error names the first row that is too long.
  (skip-unless (require 'csv-mode nil t))
  (datagrid-test-with-csv-buffer "a,b\n1,2\n3,4,5\n6,7,8,9\n"
    (let ((err (should-error (datagrid-from-csv-buffer (current-buffer) t))))
      (should (string-match-p "row 3 has 3 fields, but the first row has 2"
                              (error-message-string err))))))

(ert-deftest datagrid-test-from-csv-buffer-long-row-without-headings ()
  (skip-unless (require 'csv-mode nil t))
  (datagrid-test-with-csv-buffer "1\n2,3\n"
    (should-error (datagrid-from-csv-buffer (current-buffer)))))

(ert-deftest datagrid-test-from-csv-file-long-row-errors ()
  (datagrid-test-with-csv-file f "a,b\n1,2,3\n"
    (skip-unless (or (locate-library "csv-mode") (locate-library "pcsv")))
    (when (require 'csv-mode nil t)
      (should-error (datagrid-from-csv-file f t)))
    (when (require 'pcsv nil t)
      (should-error (datagrid-from-csv-file f t 'pcsv)))))

(ert-deftest datagrid-test-from-csv-file-extend-uneven-obsolete ()
  ;; The fourth argument is left out of the advertised calling
  ;; convention, so the byte-compiler warns about it, and it is
  ;; ignored.
  (should (equal (get-advertised-calling-convention
                  (symbol-function 'datagrid-from-csv-file))
                 '(file-path &optional headings parser)))
  (skip-unless (require 'pcsv nil t))
  (datagrid-test-with-csv-file f "a,b\n1,2\n3\n"
    (let ((expected '(("a" "1" "3") ("b" "2" nil))))
      (with-no-warnings
        (dolist (ext '(nil t))
          (should (equal (datagrid-to-alist
                          (datagrid-from-csv-file f t 'pcsv ext) t)
                         expected)))))))

(ert-deftest datagrid-test-from-csv-file-empty ()
  ;; Both parsers agree on an empty file.
  (skip-unless (or (locate-library "csv-mode") (locate-library "pcsv")))
  (datagrid-test-with-csv-file f ""
    (when (require 'csv-mode nil t)
      (should (equal (datagrid-dimensions (datagrid-from-csv-file f t))
                     '(0 . 0))))
    (when (require 'pcsv nil t)
      (should (equal (datagrid-dimensions (datagrid-from-csv-file f t 'pcsv))
                     '(0 . 0))))))


;;;; datagrid-make-empty

(ert-deftest datagrid-test-make-dimensions ()
  (let ((dg (datagrid-make-empty 3 4)))
    (should (= (datagrid-test-ncols dg) 3))
    (should (= (length (datagrid-column-data (datagrid-test-col dg 0))) 4))))

(ert-deftest datagrid-test-make-init-value ()
  (let ((dg (datagrid-make-empty 2 3 0)))
    (should (equal (datagrid-column-data (datagrid-test-col dg 0)) [0 0 0]))
    (should (equal (datagrid-column-data (datagrid-test-col dg 1)) [0 0 0]))))

(ert-deftest datagrid-test-make-with-headings ()
  (let ((dg (datagrid-make-empty 2 3 nil '("col1" "col2"))))
    (should (equal (datagrid-column-heading (datagrid-test-col dg 0)) "col1"))
    (should (equal (datagrid-column-heading (datagrid-test-col dg 1)) "col2"))))

(ert-deftest datagrid-test-make-nil-headings-by-default ()
  (let ((dg (datagrid-make-empty 2 2)))
    (should (null (datagrid-column-heading (datagrid-test-col dg 0))))))


;;;; datagrid-from-vectors

(ert-deftest datagrid-test-from-vectors-single ()
  (let ((dg (datagrid-from-vectors ["heading" 1 2 3])))
    (should (datagridp dg))
    (should (equal (datagrid-column-heading (datagrid-test-col dg 0)) "heading"))
    (should (equal (datagrid-column-data (datagrid-test-col dg 0)) [1 2 3]))))

(ert-deftest datagrid-test-from-vectors-multiple ()
  (let ((dg (datagrid-from-vectors ["a" 1 2] ["b" 3 4])))
    (should (= (datagrid-test-ncols dg) 2))
    (should (equal (datagrid-column-heading (datagrid-test-col dg 0)) "a"))
    (should (equal (datagrid-column-heading (datagrid-test-col dg 1)) "b"))
    (should (equal (datagrid-column-data (datagrid-test-col dg 1)) [3 4]))))

(ert-deftest datagrid-test-from-vectors-errors-on-list ()
  (should-error (datagrid-from-vectors '("heading" 1 2 3))))

(ert-deftest datagrid-test-from-vectors-bad-arguments ()
  (should-error (datagrid-from-vectors))
  (should-error (datagrid-from-vectors []))
  (should-error (datagrid-from-vectors :extend-uneven t))
  (should-error (datagrid-from-vectors ["a" 1] :extend-uneven t)))

(ert-deftest datagrid-test-from-vectors-unequal-lengths-error ()
  (should-error (datagrid-from-vectors ["a" 1 2] ["b" 3]))
  (should-error (datagrid-from-vectors :extend-uneven nil ["a" 1 2] ["b" 3])))

(ert-deftest datagrid-test-from-vectors-extend-uneven ()
  (let ((dg (datagrid-from-vectors :extend-uneven t ["a" 1 2] ["b" 3] ["c"])))
    (should (equal (datagrid-to-alist dg t)
                   '(("a" 1 2) ("b" 3 nil) ("c" nil nil))))))

(ert-deftest datagrid-test-from-vectors-matches-from-alist ()
  (should (equal (datagrid-to-alist
                  (datagrid-from-vectors :extend-uneven t ["a" 1 2] ["b" 3])
                  t)
                 (datagrid-to-alist
                  (datagrid-from-alist '(("a" 1 2) ("b" 3)) t t)
                  t))))


;;;; datagrid-dimensions

(ert-deftest datagrid-test-dimensions-basic ()
  (should (equal (datagrid-dimensions (datagrid-test-simple)) '(3 . 4))))

(ert-deftest datagrid-test-dimensions-single-column ()
  (let ((dg (datagrid-test-mk (datagrid-column-make :data [1 2 3 4 5]))))
    (should (equal (datagrid-dimensions dg) '(1 . 5)))))


;;;; datagrid-get-elt

(ert-deftest datagrid-test-get-elt-various-positions ()
  (let ((dg (datagrid-test-simple)))
    (should (equal (datagrid-get-elt dg 0 0) "Alice"))
    (should (equal (datagrid-get-elt dg 1 2) 30))
    (should (equal (datagrid-get-elt dg 2 3) "y"))))


;;;; datagrid-set-elt

(ert-deftest datagrid-test-set-elt-functional ()
  (let* ((dg (datagrid-test-simple))
         (result (datagrid-set-elt dg 1 2 99)))
    (should (equal (datagrid-get-elt result 1 2) 99))
    ;; source datagrid is not modified
    (should (equal (datagrid-get-elt dg 1 2) 30))))

(ert-deftest datagrid-test-set-elt-by-heading ()
  (let* ((dg (datagrid-test-simple))
         (result (datagrid-set-elt dg "group" 0 "z")))
    (should (equal (datagrid-get-elt result 2 0) "z"))
    (should (equal (datagrid-get-elt dg 2 0) "x"))))

(ert-deftest datagrid-test-set-elt-copies-only-target-column ()
  (let* ((dg (datagrid-test-simple))
         (result (datagrid-set-elt dg 1 2 99)))
    ;; the target column's data vector is a fresh copy
    (should-not (eq (datagrid-column-data (datagrid-test-col result 1))
                    (datagrid-column-data (datagrid-test-col dg 1))))
    ;; every other column shares its data vector with the source
    (should (eq (datagrid-column-data (datagrid-test-col result 0))
                (datagrid-column-data (datagrid-test-col dg 0))))
    (should (eq (datagrid-column-data (datagrid-test-col result 2))
                (datagrid-column-data (datagrid-test-col dg 2))))))

(ert-deftest datagrid-test-set-elt-respects-row-order ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "n" :data [3 1 2])))
         (sorted (datagrid-sort dg 0))          ; logical order [1 2 3]
         (result (datagrid-set-elt sorted 0 0 99)))
    (should (equal (datagrid-pull sorted 0) [1 2 3]))
    ;; writes the logical-row-0 slot, not physical index 0
    (should (equal (datagrid-get-elt result 0 0) 99))
    (should (equal (datagrid-pull result 0) [99 2 3]))
    ;; the sorted source view is untouched
    (should (equal (datagrid-pull sorted 0) [1 2 3]))))

(ert-deftest datagrid-test-set-elt-inplace-mutates-and-returns-same ()
  ;; Use a freshly allocated vector, not a literal: in-place mutation
  ;; would otherwise corrupt the shared constant across test runs.
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "n" :data (vector 10 20 30 40))))
         (ret (datagrid-set-elt dg 0 2 99 t)))
    ;; in-place returns the same object, mutated
    (should (eq ret dg))
    (should (equal (datagrid-get-elt dg 0 2) 99))))


;;;; datagrid-pull (col data)

(ert-deftest datagrid-test-get-col-data-basic ()
  (should (equal (datagrid-pull (datagrid-test-simple) 1) [10 20 30 40])))


;;;; datagrid-get-row-data

(ert-deftest datagrid-test-get-row-data-first-row ()
  (should (equal (datagrid-get-row-data (datagrid-test-simple) 0)
                 ["Alice" 10 "x"])))

(ert-deftest datagrid-test-get-row-data-last-row ()
  (should (equal (datagrid-get-row-data (datagrid-test-simple) 3)
                 ["Dave" 40 "y"])))


;;;; datagrid-get-headings

(ert-deftest datagrid-test-get-headings-basic ()
  (should (equal (datagrid-get-headings (datagrid-test-simple))
                 ["name" "score" "group"])))

(ert-deftest datagrid-test-get-headings-nil-headings ()
  (let ((dg (datagrid-test-mk (datagrid-column-make :data [1 2])
                         (datagrid-column-make :data [3 4]))))
    (should (equal (datagrid-get-headings dg) [nil nil]))))


;;;; datagrid-to-org-table

(defun datagrid-test-org-grid ()
  "Return a 2-column, 2-row datagrid for Org table tests."
  (datagrid-test-mk (datagrid-column-make :heading "a" :data ["x" "y"])
               (datagrid-column-make :heading "b" :data [1.2345 2])))

(ert-deftest datagrid-test-to-org-table-default-headings-and-hline ()
  (should (equal (datagrid-to-org-table (datagrid-test-org-grid))
                 '(("a" "b") hline ("x" 1.2345) ("y" 2)))))

(ert-deftest datagrid-test-to-org-table-hline-nil ()
  (should (equal (datagrid-to-org-table (datagrid-test-org-grid) :hline nil)
                 '(("a" "b") ("x" 1.2345) ("y" 2)))))

(ert-deftest datagrid-test-to-org-table-headings-nil ()
  (should (equal (datagrid-to-org-table (datagrid-test-org-grid) :headings nil)
                 '(("x" 1.2345) ("y" 2)))))

(ert-deftest datagrid-test-to-org-table-hline-ignored-without-headings ()
  (should (equal (datagrid-to-org-table (datagrid-test-org-grid)
                                        :headings nil :hline t)
                 '(("x" 1.2345) ("y" 2)))))

(ert-deftest datagrid-test-to-org-table-digits-rounds-floats-only ()
  (should (equal (datagrid-to-org-table (datagrid-test-org-grid) :digits 2)
                 '(("a" "b") hline ("x" 1.23) ("y" 2))))
  (should (equal (cadr (nth 2 (datagrid-to-org-table
                               (datagrid-test-org-grid) :digits 0)))
                 1)))

(ert-deftest datagrid-test-to-org-table-nil-becomes-empty ()
  (let ((dg (datagrid-test-mk (datagrid-column-make :heading "a"
                                               :data [nil 1]))))
    (should (equal (datagrid-to-org-table dg)
                   '(("a") hline ("") (1))))))

(ert-deftest datagrid-test-to-org-table-escapes-vertical-bar ()
  (let ((dg (datagrid-test-mk (datagrid-column-make :heading "a|b"
                                               :data ["c|d"]))))
    (should (equal (datagrid-to-org-table dg)
                   '(("a\\vert{}b") hline ("c\\vert{}d"))))))

(ert-deftest datagrid-test-to-org-table-no-rows ()
  (let ((dg (datagrid-test-mk (datagrid-column-make :heading "a" :data []))))
    (should (equal (datagrid-to-org-table dg) '(("a") hline)))))

(defun datagrid-test-money-grid ()
  "Return a datagrid with a string, a count, and two money columns."
  (datagrid-from-vectors ["code" "01" "22" nil]
                         ["n" 3 1 2]
                         ["cost" 100.5 0 -3]
                         ["price" 1.005 nil 2.25]))

(ert-deftest datagrid-test-to-org-table-format-string-pads-all-numbers ()
  ;; Integers are formatted too; strings, nil, and headings are not.
  (should (equal (datagrid-to-org-table (datagrid-test-money-grid)
                                        :format "%.2f")
                 '(("code" "n" "cost" "price") hline
                   ("01" "3.00" "100.50" "1.00")
                   ("22" "1.00" "0.00" "")
                   ("" "2.00" "-3.00" "2.25")))))

(ert-deftest datagrid-test-to-org-table-format-function-sees-numbers-only ()
  (let ((seen nil))
    (datagrid-to-org-table (datagrid-test-money-grid)
                           :format (lambda (x) (push x seen) x))
    (should (cl-every #'numberp seen))
    (should (= (length seen) 8))))

(ert-deftest datagrid-test-to-org-table-format-function-symbol ()
  (should (equal (datagrid-to-org-table (datagrid-test-org-grid)
                                        :format #'number-to-string
                                        :headings nil)
                 '(("x" "1.2345") ("y" "2")))))

(ert-deftest datagrid-test-to-org-table-format-alist-per-column ()
  ;; Columns not named are left as they are.
  (should (equal (datagrid-to-org-table
                  (datagrid-test-money-grid)
                  :headings nil
                  :format '(("n" . "%d") (("cost" 3) . "%.2f")))
                 '(("01" "3" "100.50" "1.00")
                   ("22" "1" "0.00" "")
                   ("" "2" "-3.00" "2.25"))))
  (should (equal (datagrid-to-org-table (datagrid-test-money-grid)
                                        :headings nil
                                        :format '(("cost" . "%.1f")))
                 '(("01" 3 "100.5" 1.005)
                   ("22" 1 "0.0" "")
                   ("" 2 "-3.0" 2.25)))))

(ert-deftest datagrid-test-to-org-table-format-alist-function ()
  (should (equal (cadr (datagrid-to-org-table
                        (datagrid-test-org-grid)
                        :headings nil
                        :format `(("b" . ,(lambda (x) (* 10 x))))))
                 '("y" 20))))

(ert-deftest datagrid-test-to-org-table-format-alist-follows-col-order ()
  (let ((dg (datagrid-select (datagrid-test-money-grid) "cost" "n")))
    (should (equal (car (datagrid-to-org-table dg :headings nil
                                               :format '((1 . "%03d"))))
                   '(100.5 "003")))))

(ert-deftest datagrid-test-to-org-table-format-escapes-vertical-bar ()
  (should (equal (cadr (datagrid-to-org-table
                        (datagrid-test-org-grid)
                        :headings nil
                        :format (lambda (x) (format "%s|" x))))
                 '("y" "2\\vert{}"))))

(ert-deftest datagrid-test-to-org-table-format-with-digits-errors ()
  (should-error (datagrid-to-org-table (datagrid-test-org-grid)
                                       :digits 2 :format "%.2f")))

(ert-deftest datagrid-test-to-org-table-format-bad-values-error ()
  (let ((dg (datagrid-test-money-grid)))
    ;; Not a string, function, or alist.
    (should-error (datagrid-to-org-table dg :format 5))
    ;; An entry that is not a cons, or whose FMT is not usable.
    (should-error (datagrid-to-org-table dg :format '("n")))
    (should-error (datagrid-to-org-table dg :format '(("n" . 5))))
    ;; A column that is not there.
    (should-error (datagrid-to-org-table dg :format '(("nope" . "%d"))))
    (should-error (datagrid-to-org-table dg :format '((9 . "%d"))))
    ;; The same column twice, even under two refs.
    (should-error (datagrid-to-org-table
                   dg :format '(("n" . "%d") (1 . "%.2f"))))))

(ert-deftest datagrid-test-to-org-table-format-no-rows ()
  (let ((dg (datagrid-test-mk (datagrid-column-make :heading "a" :data []))))
    (should (equal (datagrid-to-org-table dg :format "%.2f")
                   '(("a") hline)))))

(ert-deftest datagrid-test-to-org-table-footer-label-and-defaults ()
  ;; Every all-number column is summed, nils skipped. The label goes
  ;; in the first column that is not summed.
  (should (equal (datagrid-to-org-table (datagrid-test-money-grid)
                                        :footer "Total")
                 '(("code" "n" "cost" "price") hline
                   ("01" 3 100.5 1.005)
                   ("22" 1 0 "")
                   ("" 2 -3 2.25)
                   hline
                   ("Total" 6 97.5 3.255)))))

(ert-deftest datagrid-test-to-org-table-footer-does-not-change-datagrid ()
  (let ((dg (datagrid-test-money-grid)))
    (datagrid-to-org-table dg :footer "Total")
    (should (equal (datagrid-dimensions dg) '(4 . 3)))))

(ert-deftest datagrid-test-to-org-table-footer-uses-format ()
  (should (equal (last (datagrid-to-org-table
                        (datagrid-test-money-grid)
                        :footer "Total"
                        :format '(("n" . "%d") (("cost" "price") . "%.2f")))
                       2)
                 '(hline ("Total" "6" "97.50" "3.25")))))

(ert-deftest datagrid-test-to-org-table-footer-uses-digits ()
  (should (equal (car (last (datagrid-to-org-table (datagrid-test-money-grid)
                                                   :footer "T" :digits 1)))
                 '("T" 6 97.5 3.3))))

(ert-deftest datagrid-test-to-org-table-footer-plist ()
  (should (equal (car (last (datagrid-to-org-table
                             (datagrid-test-money-grid)
                             :footer '(:label "Top" :fn max
                                              :cols ("cost" 1)))))
                 '("Top" 3 100.5 ""))))

(ert-deftest datagrid-test-to-org-table-footer-plist-without-label ()
  (should (equal (car (last (datagrid-to-org-table
                             (datagrid-test-money-grid)
                             :footer '(:fn min))))
                 '("" 1 -3 1.005))))

(ert-deftest datagrid-test-to-org-table-footer-explicit-cols-only ()
  ;; With :cols, other number columns are left empty.
  (should (equal (car (last (datagrid-to-org-table
                             (datagrid-test-money-grid)
                             :footer '(:label "Total" :cols ("n")))))
                 '("Total" 6 "" ""))))

(ert-deftest datagrid-test-to-org-table-footer-label-skips-summed-cols ()
  ;; The first column is summed, so the label goes in the second.
  (let ((dg (datagrid-select (datagrid-test-money-grid) "n" "code" "cost")))
    (should (equal (car (last (datagrid-to-org-table dg :footer "Total")))
                   '(6 "Total" 97.5)))))

(ert-deftest datagrid-test-to-org-table-footer-follows-row-and-col-order ()
  (let ((dg (thread-first (datagrid-test-money-grid)
                          (datagrid-filter (lambda (n) (> n 1)) "n")
                          (datagrid-select "cost" "code"))))
    (should (equal (car (last (datagrid-to-org-table dg :footer "Total")))
                   '(97.5 "Total")))))

(ert-deftest datagrid-test-to-org-table-footer-without-headings ()
  (should (equal (datagrid-to-org-table (datagrid-test-org-grid)
                                        :headings nil :footer "Total")
                 `(("x" 1.2345) ("y" 2) hline ("Total" ,(+ 1.2345 2))))))

(ert-deftest datagrid-test-to-org-table-footer-hline-nil ()
  ;; HLINE controls only the rule under the headings.
  (should (equal (datagrid-to-org-table (datagrid-test-org-grid)
                                        :hline nil :footer "Total")
                 `(("a" "b") ("x" 1.2345) ("y" 2) hline
                   ("Total" ,(+ 1.2345 2))))))

(ert-deftest datagrid-test-to-org-table-footer-no-rows ()
  ;; No doubled rule under the headings. An empty sum is 0, but a
  ;; column with no numbers is not summed by default.
  (let ((dg (datagrid-from-vectors ["a"] ["b"])))
    (should (equal (datagrid-to-org-table dg :footer "Total")
                   '(("a" "b") hline ("Total" ""))))
    (should (equal (datagrid-to-org-table
                    dg :footer '(:label "Total" :cols ("b")))
                   '(("a" "b") hline ("Total" 0))))
    (should (equal (datagrid-to-org-table
                    dg :headings nil :footer '(:label "Total" :cols ("b")))
                   '(("Total" 0))))))

(ert-deftest datagrid-test-to-org-table-footer-all-nil-column ()
  ;; A column of nils only is not a number column. Asked for, its
  ;; sum is 0, and max gives an empty cell.
  (let ((dg (datagrid-from-vectors ["a" "x" "y"] ["b" nil nil])))
    (should (equal (car (last (datagrid-to-org-table dg :footer "T")))
                   '("T" "")))
    (should (equal (car (last (datagrid-to-org-table
                               dg :footer '(:label "T" :cols ("b")))))
                   '("T" 0)))
    (should (equal (car (last (datagrid-to-org-table
                               dg :footer '(:label "T" :fn max
                                                   :cols ("b")))))
                   '("T" "")))))

(ert-deftest datagrid-test-to-org-table-footer-errors ()
  (let ((dg (datagrid-test-money-grid)))
    ;; Not a string or a plist.
    (should-error (datagrid-to-org-table dg :footer 5))
    (should-error (datagrid-to-org-table dg :footer '("Total")))
    ;; Unknown keyword, or FN not a function.
    (should-error (datagrid-to-org-table dg :footer '(:lable "Total")))
    (should-error (datagrid-to-org-table dg :footer '(:fn 5)))
    ;; A column that is not there.
    (should-error (datagrid-to-org-table dg :footer '(:cols ("nope"))))
    (should-error (datagrid-to-org-table dg :footer '(:cols (9))))
    ;; No column left for the label.
    (should-error (datagrid-to-org-table
                   (datagrid-select dg "n" "cost") :footer "Total"))))


;;;; datagrid-col-index-by-heading

(ert-deftest datagrid-test-col-index-by-heading-first ()
  (should (= (datagrid-col-index-by-heading (datagrid-test-simple) "name") 0)))

(ert-deftest datagrid-test-col-index-by-heading-middle ()
  (should (= (datagrid-col-index-by-heading (datagrid-test-simple) "score") 1)))

(ert-deftest datagrid-test-col-index-by-heading-not-found ()
  (should (null (datagrid-col-index-by-heading (datagrid-test-simple) "missing"))))

(ert-deftest datagrid-test-col-index-by-heading-logical-order ()
  ;; The index is the logical position, not the stored one.
  (let ((dg (datagrid-select (datagrid-test-simple) "score" "name")))
    (should (= (datagrid-col-index-by-heading dg "name") 1))
    (should (= (datagrid-col-index-by-heading dg "score") 0))
    (should (null (datagrid-col-index-by-heading dg "group")))))

(ert-deftest datagrid-test-col-index-by-heading-first-duplicate ()
  (let ((dg (datagrid-from-vectors ["a" 1] ["b" 2] ["a" 3])))
    (should (= (datagrid-col-index-by-heading dg "a") 0))))


;;;; datagrid-col-data-by-header

(ert-deftest datagrid-test-col-data-by-header-basic ()
  (should (equal (datagrid-pull (datagrid-test-simple) "score")
                 [10 20 30 40])))


;;;; datagrid-column-decode

(ert-deftest datagrid-test-column-decode-with-code ()
  ;; "Agree"=4, "Disagree"=2, "Neutral"=3, "Agree"=4
  (should (equal (datagrid-column-decode (datagrid-test-coded) 0) [4 2 3 4])))

(ert-deftest datagrid-test-column-decode-missing-value-is-nil ()
  (let ((dg (datagrid-test-mk (datagrid-column-make
                          :heading "x"
                          :data ["Agree" "Unknown"]
                          :code '(("Agree" . 4))))))
    (should (equal (datagrid-column-decode dg 0) [4 nil]))))

(ert-deftest datagrid-test-column-decode-no-code-returns-data ()
  (let ((dg (datagrid-test-mk (datagrid-column-make :data [1 2 3]))))
    (should (equal (datagrid-column-decode dg 0) [1 2 3]))))

(ert-deftest datagrid-test-column-decode-numeric-keys ()
  ;; Regression: numeric data signaled an error from `string-equal'.
  (let ((dg (datagrid-test-mk (datagrid-column-make
                          :heading "x"
                          :data [1 2 3]
                          :code '((1 . "Disagree") (2 . "Agree"))))))
    (should (equal (datagrid-column-decode dg 0) ["Disagree" "Agree" nil]))))

(ert-deftest datagrid-test-column-decode-numbers-against-string-keys ()
  (let ((dg (datagrid-test-mk (datagrid-column-make
                          :heading "I like Emacs."
                          :data [5 5 5 5 5]
                          :code '(("Strongly disagree" . 1)
                                  ("Agree" . 4)
                                  ("Strongly agree" . 5))))))
    (should (equal (datagrid-column-decode dg 0 'unmapped)
                   [unmapped unmapped unmapped unmapped unmapped]))))

(ert-deftest datagrid-test-column-decode-symbol-matches-string-key ()
  (let ((dg (datagrid-test-mk (datagrid-column-make
                          :heading "x"
                          :data (vector 'Agree "Agree")
                          :code '(("Agree" . 4))))))
    (should (equal (datagrid-column-decode dg 0) [4 4]))))

(ert-deftest datagrid-test-to-vtable ()
  ;; Regression: `make-vtable' is not autoloaded, so this failed unless
  ;; vtable was already loaded.
  (skip-unless (locate-library "vtable"))
  (let ((name (generate-new-buffer-name " *datagrid-vtable-test*")))
    (unwind-protect
        (progn
          (datagrid-to-vtable (datagrid-test-simple) name t)
          (with-current-buffer name
            (should (string-match-p "Alice" (buffer-string)))))
      (when (get-buffer name) (kill-buffer name)))))


;;;; datagrid-column-add-code

(ert-deftest datagrid-test-column-add-code-single-index ()
  (let* ((dg (datagrid-test-simple))
         (code '(("x" . 1) ("y" . 2)))
         (result (datagrid-column-add-code dg 2 code)))
    (should (equal (datagrid-column-code (datagrid-test-col result 2)) code))))

(ert-deftest datagrid-test-column-add-code-multiple-indices ()
  (let* ((dg (datagrid-test-simple))
         (code '(("x" . 1) ("y" . 2)))
         (result (datagrid-column-add-code dg '(0 2) code)))
    (should (equal (datagrid-column-code (datagrid-test-col result 0)) code))
    (should (equal (datagrid-column-code (datagrid-test-col result 2)) code))
    (should (null (datagrid-column-code (datagrid-test-col result 1))))))


;;;; datagrid-head

(ert-deftest datagrid-test-head-defaults-to-5x5 ()
  (let* ((dg (datagrid-make-empty 6 10 0))
         (h (datagrid-head dg)))
    (should (= (datagrid-test-ncols h) 5))
    (should (= (length (datagrid-column-data (datagrid-test-col h 0))) 5))))

(ert-deftest datagrid-test-head-limit-rows ()
  (let* ((h (datagrid-head (datagrid-test-simple) nil 2)))
    (should (= (length (datagrid-column-data (datagrid-test-col h 0))) 2))
    (should (equal (datagrid-column-data (datagrid-test-col h 0)) ["Alice" "Bob"]))))

(ert-deftest datagrid-test-head-limit-cols ()
  (let* ((h (datagrid-head (datagrid-test-simple) 2 nil)))
    (should (= (datagrid-test-ncols h) 2))
    (should (equal (datagrid-column-heading (datagrid-test-col h 1)) "score"))))

(ert-deftest datagrid-test-head-fewer-cols-than-default ()
  "Should not error when datagrid has fewer than 5 columns."
  (let* ((dg (datagrid-test-mk (datagrid-column-make :heading "a" :data [1 2])))
         (h (datagrid-head dg)))
    (should (= (datagrid-test-ncols h) 1))))

(ert-deftest datagrid-test-head-preserves-metadata ()
  (let* ((dg (datagrid-test-coded))
         (h (datagrid-head dg)))
    (should (equal (datagrid-column-heading (datagrid-test-col h 0)) "rating"))
    (should (equal (datagrid-column-lom (datagrid-test-col h 0)) "ordinal"))
    (should (equal (datagrid-column-code (datagrid-test-col h 0))
                   (datagrid-column-code (datagrid-test-col dg 0))))))


;;;; datagrid-add-column

(ert-deftest datagrid-test-add-column-appends ()
  (let* ((dg (datagrid-test-simple))
         (new-col (datagrid-column-make :heading "new" :data [1 2 3 4]))
         (result (datagrid-add-column dg new-col)))
    (should (= (datagrid-test-ncols result) 4))
    (should (equal (datagrid-column-heading (datagrid-test-col result 3)) "new"))
    (should (equal (datagrid-column-data (datagrid-test-col result 3)) [1 2 3 4]))))

(ert-deftest datagrid-test-add-column-truncates-long-data ()
  (let* ((dg (datagrid-test-simple))
         (new-col (datagrid-column-make :heading "new" :data [1 2 3 4 5 6]))
         (result (datagrid-add-column dg new-col)))
    (should (= (length (datagrid-column-data (datagrid-test-col result 3))) 4))))

(ert-deftest datagrid-test-add-column-extends-short-data ()
  (let* ((dg (datagrid-test-simple))
         (new-col (datagrid-column-make :heading "new" :data [1 2]))
         (result (datagrid-add-column dg new-col)))
    (should (= (length (datagrid-column-data (datagrid-test-col result 3))) 4))
    (should (null (aref (datagrid-column-data (datagrid-test-col result 3)) 3)))))

(ert-deftest datagrid-test-add-column-two-columns-order ()
  (let* ((dg (datagrid-test-mk (datagrid-column-make :heading "a" :data [1 2])))
         (col1 (datagrid-column-make :heading "b" :data [3 4]))
         (col2 (datagrid-column-make :heading "c" :data [5 6]))
         (result (datagrid-add-column dg col1 col2)))
    (should (equal (datagrid-column-heading (datagrid-test-col result 1)) "b"))
    (should (equal (datagrid-column-heading (datagrid-test-col result 2)) "c"))))


;;;; datagrid-add-row (via datagrid-add-data)

(ert-deftest datagrid-test-add-row-basic ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "a" :data [1 2])
              (datagrid-column-make :heading "b" :data [3 4])))
         (result (datagrid-add-data dg '((5 6) (7 8)) t)))
    (should (= (length (datagrid-column-data (datagrid-test-col result 0))) 4))
    (should (equal (datagrid-column-data (datagrid-test-col result 0)) [1 2 5 7]))
    (should (equal (datagrid-column-data (datagrid-test-col result 1)) [3 4 6 8]))))


;;;; datagrid-bind-rows

(ert-deftest datagrid-test-bind-rows-basic ()
  (let* ((dg1 (datagrid-test-mk
               (datagrid-column-make :heading "a" :data [1 2])
               (datagrid-column-make :heading "b" :data [3 4])))
         (dg2 (datagrid-test-mk
               (datagrid-column-make :heading "a" :data [5 6])
               (datagrid-column-make :heading "b" :data [7 8])))
         (result (datagrid-bind-rows dg1 dg2)))
    (should (= (datagrid-test-ncols result) 2))
    (should (equal (datagrid-get-headings result) ["a" "b"]))
    (should (equal (datagrid-column-data (datagrid-test-col result 0)) [1 2 5 6]))
    (should (equal (datagrid-column-data (datagrid-test-col result 1)) [3 4 7 8]))))

(ert-deftest datagrid-test-bind-rows-aligns-by-heading ()
  (let* ((dg1 (datagrid-test-mk
               (datagrid-column-make :heading "a" :data [1 2])
               (datagrid-column-make :heading "b" :data [3 4])))
         (dg2 (datagrid-test-mk
               (datagrid-column-make :heading "b" :data [7 8])
               (datagrid-column-make :heading "a" :data [5 6])))
         (result (datagrid-bind-rows dg1 dg2)))
    (should (equal (datagrid-column-data (datagrid-test-col result 0)) [1 2 5 6]))
    (should (equal (datagrid-column-data (datagrid-test-col result 1)) [3 4 7 8]))))

(ert-deftest datagrid-test-bind-rows-fills-missing-columns ()
  (let* ((dg1 (datagrid-test-mk
               (datagrid-column-make :heading "a" :data [1 2])
               (datagrid-column-make :heading "b" :data [3 4])))
         (dg2 (datagrid-test-mk
               (datagrid-column-make :heading "a" :data [5 6])
               (datagrid-column-make :heading "c" :data [9 10])))
         (result (datagrid-bind-rows dg1 dg2)))
    (should (equal (datagrid-get-headings result) ["a" "b" "c"]))
    (should (equal (datagrid-column-data (datagrid-test-col result 0)) [1 2 5 6]))
    (should (equal (datagrid-column-data (datagrid-test-col result 1)) [3 4 nil nil]))
    (should (equal (datagrid-column-data (datagrid-test-col result 2)) [nil nil 9 10]))))

(ert-deftest datagrid-test-bind-rows-preserves-metadata-from-first ()
  (let* ((dg1 (datagrid-test-mk
               (datagrid-column-make :heading "rating"
                                     :data ["Agree"]
                                     :lom "ordinal"
                                     :code '(("Agree" . 4)))))
         (dg2 (datagrid-test-mk
               (datagrid-column-make :heading "rating"
                                     :data ["Disagree"]
                                     :lom "nominal"
                                     :code nil)))
         (result (datagrid-bind-rows dg1 dg2)))
    (should (equal (datagrid-column-lom (datagrid-test-col result 0)) "ordinal"))
    (should (equal (datagrid-column-code (datagrid-test-col result 0))
                   '(("Agree" . 4))))))

(ert-deftest datagrid-test-bind-rows-single-grid ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "a" :data [1 2 3])))
         (result (datagrid-bind-rows dg)))
    (should (equal (datagrid-column-data (datagrid-test-col result 0)) [1 2 3]))))

(ert-deftest datagrid-test-bind-rows-skips-nil-args ()
  (let* ((dg1 (datagrid-test-mk (datagrid-column-make :heading "a" :data [1])))
         (dg2 (datagrid-test-mk (datagrid-column-make :heading "a" :data [2])))
         (result (datagrid-bind-rows nil dg1 nil dg2 nil)))
    (should (equal (datagrid-column-data (datagrid-test-col result 0)) [1 2]))))

(ert-deftest datagrid-test-bind-rows-errors-on-no-args ()
  (should-error (datagrid-bind-rows))
  (should-error (datagrid-bind-rows nil nil)))

(ert-deftest datagrid-test-bind-rows-errors-on-non-datagrid ()
  (let ((dg (datagrid-test-mk (datagrid-column-make :heading "a" :data [1]))))
    (should-error (datagrid-bind-rows dg "not a datagrid"))))

(ert-deftest datagrid-test-bind-rows-respects-row-order ()
  (let* ((dg1 (datagrid-test-mk
               (datagrid-column-make :heading "a" :data [10 20 30])))
         (sorted (datagrid-sort dg1 "a"))
         (reversed (datagrid-make
                    :columns (datagrid-columns sorted)
                    :row-order (vector 2 1 0)))
         (dg2 (datagrid-test-mk
               (datagrid-column-make :heading "a" :data [40 50])))
         (result (datagrid-bind-rows reversed dg2)))
    (should (equal (datagrid-column-data (datagrid-test-col result 0))
                   [30 20 10 40 50]))))

(ert-deftest datagrid-test-bind-rows-respects-col-order ()
  (let* ((dg1 (datagrid-test-mk
               (datagrid-column-make :heading "a" :data [1 2])
               (datagrid-column-make :heading "b" :data [3 4])))
         (reordered (datagrid-select dg1 "b" "a"))
         (dg2 (datagrid-test-mk
               (datagrid-column-make :heading "a" :data [5])
               (datagrid-column-make :heading "b" :data [6])))
         (result (datagrid-bind-rows reordered dg2)))
    (should (equal (datagrid-get-headings result) ["b" "a"]))
    (should (equal (datagrid-column-data (datagrid-test-col result 0)) [3 4 6]))
    (should (equal (datagrid-column-data (datagrid-test-col result 1)) [1 2 5]))))


;;;; datagrid-set-headings

(ert-deftest datagrid-test-set-headings-all ()
  (let* ((dg (datagrid-make-empty 3 2 nil '("a" "b" "c")))
         (result (datagrid-set-headings dg '("x" "y" "z"))))
    (should (equal (datagrid-column-heading (datagrid-test-col result 0)) "x"))
    (should (equal (datagrid-column-heading (datagrid-test-col result 1)) "y"))
    (should (equal (datagrid-column-heading (datagrid-test-col result 2)) "z"))))


;;;; datagrid-remove-column

(ert-deftest datagrid-test-remove-column-first ()
  (let* ((result (datagrid-remove-column (datagrid-test-simple) 0)))
    (should (= (datagrid-test-ncols result) 2))
    (should (equal (datagrid-column-heading (datagrid-test-col result 0)) "score"))))

(ert-deftest datagrid-test-remove-column-middle ()
  (let* ((result (datagrid-remove-column (datagrid-test-simple) 1)))
    (should (= (datagrid-test-ncols result) 2))
    (should (equal (datagrid-column-heading (datagrid-test-col result 0)) "name"))
    (should (equal (datagrid-column-heading (datagrid-test-col result 1)) "group"))))

(ert-deftest datagrid-test-remove-column-last ()
  (let* ((result (datagrid-remove-column (datagrid-test-simple) 2)))
    (should (= (datagrid-test-ncols result) 2))
    (should (equal (datagrid-column-heading (datagrid-test-col result 1)) "score"))))


;;;; datagrid-sort

(ert-deftest datagrid-test-sort-numeric-reorders-all-columns ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "n" :data [3 1 4 1 5])
              (datagrid-column-make :heading "s" :data ["c" "a" "d" "b" "e"])))
         (result (datagrid-sort dg 0)))
    (should (equal (datagrid-pull result 0) [1 1 3 4 5]))
    (should (equal (datagrid-pull result 1) ["a" "b" "c" "d" "e"]))))

(ert-deftest datagrid-test-sort-string-column ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "s" :data ["banana" "apple" "cherry"])
              (datagrid-column-make :heading "n" :data [2 1 3])))
         (result (datagrid-sort dg 0)))
    (should (equal (datagrid-pull result 0)
                   ["apple" "banana" "cherry"]))
    (should (equal (datagrid-pull result 1) [1 2 3]))))

(ert-deftest datagrid-test-sort-does-not-mutate-original ()
  (let* ((dg (datagrid-test-mk (datagrid-column-make :data [3 1 2])))
         (original-data (copy-sequence
                         (datagrid-column-data (datagrid-test-col dg 0)))))
    (datagrid-sort dg 0)
    (should (equal (datagrid-column-data (datagrid-test-col dg 0))
                   original-data))))

(ert-deftest datagrid-test-sort-spec-reverse ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "n" :data [3 1 4 1 5])
              (datagrid-column-make :heading "s" :data ["c" "a" "d" "b" "e"])))
         (result (datagrid-sort dg '(:col 0 :reverse t))))
    (should (equal (datagrid-pull result 0) [5 4 3 1 1]))
    (should (equal (datagrid-pull result 1) ["e" "d" "c" "a" "b"]))))

(ert-deftest datagrid-test-sort-spec-key-transform ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "s"
                                    :data ["ccc" "a" "bb" "dddd"])))
         (result (datagrid-sort dg '(:col 0 :key length))))
    (should (equal (datagrid-pull result 0) ["a" "bb" "ccc" "dddd"]))))

(ert-deftest datagrid-test-sort-spec-custom-lessp ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "n" :data [3 1 2])))
         (result (datagrid-sort dg `(:col 0 :lessp ,(lambda (a b) (> a b))))))
    (should (equal (datagrid-pull result 0) [3 2 1]))))

(ert-deftest datagrid-test-sort-multi-key-precedence ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "dept"
                                    :data ["b" "a" "b" "a"])
              (datagrid-column-make :heading "name"
                                    :data ["xx" "yyy" "z" "w"])))
         ;; dept ascending, then name length ascending within dept.
         (result (datagrid-sort dg "dept" '(:col "name" :key length))))
    (should (equal (datagrid-pull result 0) ["a" "a" "b" "b"]))
    (should (equal (datagrid-pull result 1) ["w" "yyy" "z" "xx"]))))

(ert-deftest datagrid-test-sort-multi-key-reverse-secondary ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "status"
                                    :data ["ok" "ok" "bad" "ok"])
              (datagrid-column-make :heading "profit"
                                    :data [10 -30 5 20])))
         ;; status ascending, then absolute profit descending.
         (result (datagrid-sort dg "status"
                                '(:col "profit" :key abs :reverse t))))
    (should (equal (datagrid-pull result 0) ["bad" "ok" "ok" "ok"]))
    (should (equal (datagrid-pull result 1) [5 -30 20 10]))))

(ert-deftest datagrid-test-sort-multi-key-is-stable ()
  ;; Equal primary keys must preserve original row order (a stable
  ;; tie-break), so the trailing column stays in its input sequence.
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "k" :data ["a" "a" "a"])
              (datagrid-column-make :heading "tag" :data [1 2 3])))
         (result (datagrid-sort dg "k")))
    (should (equal (datagrid-pull result 1) [1 2 3]))))

(ert-deftest datagrid-test-sort-no-keys-returns-input ()
  (let* ((dg (datagrid-test-mk (datagrid-column-make :data [3 1 2]))))
    (should (equal (datagrid-pull (datagrid-sort dg) 0) [3 1 2]))))

(ert-deftest datagrid-test-sort-bad-spec-signals ()
  (let ((dg (datagrid-test-mk (datagrid-column-make :data [1 2]))))
    (should-error (datagrid-sort dg '(:key length)))))


;;;; datagrid-join (deprecated -> datagrid-left-join)

(ert-deftest datagrid-test-join-basic-left-outer ()
  (let* ((dg1 (datagrid-test-mk
               (datagrid-column-make :heading "id" :data ["a" "b" "c"])
               (datagrid-column-make :heading "val" :data [1 2 3])))
         (dg2 (datagrid-test-mk
               (datagrid-column-make :heading "id" :data ["b" "c" "a"])
               (datagrid-column-make :heading "label" :data ["bee" "cee" "ay"])))
         (result (with-no-warnings (datagrid-join dg1 0 dg2 0 1))))
    (should (= (datagrid-test-ncols result) 3))
    (should (equal (datagrid-pull result 2) ["ay" "bee" "cee"]))))

(ert-deftest datagrid-test-join-unmatched-row-is-nil ()
  (let* ((dg1 (datagrid-test-mk
               (datagrid-column-make :heading "id" :data ["a" "x" "c"])))
         (dg2 (datagrid-test-mk
               (datagrid-column-make :heading "id" :data ["a" "c"])
               (datagrid-column-make :heading "v" :data [10 30])))
         (result (with-no-warnings (datagrid-join dg1 0 dg2 0 1))))
    (should (null (aref (datagrid-pull result 1) 1)))))


;;;; datagrid-create-mask

(ert-deftest datagrid-test-create-mask-numeric-predicate ()
  (let* ((dg (datagrid-test-simple))
         (mask (datagrid-create-mask dg (lambda (x) (> x 15)) 1)))
    (should (equal mask [nil t t t]))))

(ert-deftest datagrid-test-create-mask-string-predicate ()
  (let* ((dg (datagrid-test-simple))
         (mask (datagrid-create-mask dg (lambda (x) (string-equal x "x")) 2)))
    (should (equal mask [t nil t nil]))))


;;;; datagrid-filter-by-mask

(ert-deftest datagrid-test-filter-by-mask-basic ()
  (let* ((dg (datagrid-test-simple))
         (result (datagrid-filter-by-mask dg [t nil t nil])))
    (should (datagridp result))
    (should (= (length (datagrid-pull result 0)) 2))
    (should (equal (datagrid-pull result 0) ["Alice" "Carol"]))
    (should (equal (datagrid-pull result 1) [10 30]))))

(ert-deftest datagrid-test-filter-by-mask-all-false ()
  (let* ((result (datagrid-filter-by-mask (datagrid-test-simple) [nil nil nil nil])))
    (should (= (length (datagrid-pull result 0)) 0))))

(ert-deftest datagrid-test-filter-by-mask-all-true ()
  (let* ((dg (datagrid-test-simple))
         (result (datagrid-filter-by-mask dg [t t t t])))
    (should (equal (datagrid-pull result 0)
                   (datagrid-pull dg 0)))))

(ert-deftest datagrid-test-filter-by-mask-accepts-list-mask ()
  (let* ((result (datagrid-filter-by-mask (datagrid-test-simple) '(t nil t nil))))
    (should (= (length (datagrid-pull result 0)) 2))))

(ert-deftest datagrid-test-filter-by-mask-preserves-metadata ()
  (let* ((dg (datagrid-test-coded))
         (result (datagrid-filter-by-mask dg [t nil t nil])))
    (should (equal (datagrid-column-heading (datagrid-test-col result 0)) "rating"))
    (should (equal (datagrid-column-lom (datagrid-test-col result 0)) "ordinal"))))


;;;; datagrid-filter-vector-by-mask

(ert-deftest datagrid-test-filter-vector-by-mask-is-obsolete ()
  (should (get 'datagrid-filter-vector-by-mask 'byte-obsolete-info)))

(ert-deftest datagrid-test-filter-vector-by-mask-still-works ()
  ;; Obsolete functions keep working until they are removed.
  (let* ((col (datagrid-column-make :heading "n" :data [3 1 2]))
         (result (with-no-warnings
                   (datagrid-filter-vector-by-mask col [t nil t]))))
    (should (equal (datagrid-column-data result) [3 2]))
    (should (equal (datagrid-column-heading result) "n"))))


;;;; datagrid-filter

(ert-deftest datagrid-test-filter-one-column ()
  (let ((result (datagrid-filter (datagrid-test-simple)
                                 (lambda (s) (> s 15)) "score")))
    (should (equal (datagrid-pull result "name") ["Bob" "Carol" "Dave"]))
    (should (equal (datagrid-pull result "score") [20 30 40]))))

(ert-deftest datagrid-test-filter-several-columns-in-order ()
  ;; PRED gets one value per column, in the order of COLS.
  (let ((result (datagrid-filter (datagrid-test-simple)
                                 (lambda (g s) (and (equal g "y") (< s 30)))
                                 "group" "score")))
    (should (equal (datagrid-pull result "name") ["Bob"]))))

(ert-deftest datagrid-test-filter-integer-refs ()
  (let ((result (datagrid-filter (datagrid-test-simple)
                                 (lambda (g) (equal g "x")) 2)))
    (should (equal (datagrid-pull result 0) ["Alice" "Carol"]))))

(ert-deftest datagrid-test-filter-any-non-nil-keeps-row ()
  (let ((result (datagrid-filter (datagrid-test-simple)
                                 (lambda (n) (string-search "a" n))
                                 "name")))
    (should (equal (datagrid-pull result "name") ["Carol" "Dave"]))))

(ert-deftest datagrid-test-filter-no-rows-match ()
  (let ((result (datagrid-filter (datagrid-test-simple) #'ignore "score")))
    (should (datagridp result))
    (should (equal (datagrid-dimensions result) '(3 . 0)))))

(ert-deftest datagrid-test-filter-shares-columns ()
  (let* ((dg (datagrid-test-simple))
         (result (datagrid-filter dg (lambda (s) (> s 15)) "score")))
    (should (eq (datagrid-columns dg) (datagrid-columns result)))
    (should (equal (datagrid-pull dg "score") [10 20 30 40]))))

(ert-deftest datagrid-test-filter-follows-row-and-col-order ()
  ;; Values come from the logical view, and the result keeps it.
  (let* ((dg (thread-first (datagrid-test-simple)
                           (datagrid-sort '(:col "score" :reverse t))
                           (datagrid-select "score" "name")))
         (result (datagrid-filter dg (lambda (s) (< s 40)) "score")))
    (should (equal (datagrid-get-headings result) ["score" "name"]))
    (should (equal (datagrid-pull result "name") ["Carol" "Bob" "Alice"]))))

(ert-deftest datagrid-test-filter-preserves-metadata ()
  (let ((result (datagrid-filter (datagrid-test-coded)
                                 (lambda (r) (equal r "Agree")) "rating")))
    (should (equal (datagrid-pull result 0) ["Agree" "Agree"]))
    (should (equal (datagrid-column-lom (datagrid-test-col result 0))
                   "ordinal"))
    (should (datagrid-column-code (datagrid-test-col result 0)))))

(ert-deftest datagrid-test-filter-matches-filter-by-mask ()
  (let* ((dg (datagrid-test-simple))
         (pred (lambda (s) (/= s 20))))
    (should (equal (datagrid-to-alist (datagrid-filter dg pred "score") t)
                   (datagrid-to-alist
                    (datagrid-filter-by-mask
                     dg (datagrid-create-mask dg pred "score"))
                    t)))))

(ert-deftest datagrid-test-filter-requires-columns ()
  (should-error (datagrid-filter (datagrid-test-simple) #'identity)))

(ert-deftest datagrid-test-filter-bad-heading-errors ()
  (should-error (datagrid-filter (datagrid-test-simple) #'identity "nope")))

(ert-deftest datagrid-test-filter-chains-with-thread-first ()
  (let ((result (thread-first (datagrid-test-simple)
                              (datagrid-filter (lambda (g) (equal g "y"))
                                               "group")
                              (datagrid-filter (lambda (s) (> s 20))
                                               "score"))))
    (should (equal (datagrid-pull result "name") ["Dave"]))))


;;;; datagrid-group-by

(ert-deftest datagrid-test-group-by-two-groups ()
  (let* ((dg (datagrid-test-simple))
         (result (datagrid-group-by dg 2)))
    (should (= (length result) 2))
    (let ((group-x (cl-find "x" result :key #'car :test #'equal))
          (group-y (cl-find "y" result :key #'car :test #'equal)))
      (should group-x)
      (should group-y)
      (should (equal (aref (cadr group-x) 0) ["Alice" "Carol"]))
      (should (equal (aref (cadr group-y) 0) ["Bob" "Dave"])))))

(ert-deftest datagrid-test-group-by-single-group ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "g" :data ["a" "a" "a"])
              (datagrid-column-make :heading "v" :data [1 2 3])))
         (result (datagrid-group-by dg 0)))
    (should (= (length result) 1))
    (should (equal (car (aref result 0)) "a"))
    (should (equal (aref (cadr (aref result 0)) 1) [1 2 3]))))


;;;; datagrid--group-rows

(ert-deftest datagrid-test-group-rows-first-seen-order ()
  (let ((dg (datagrid-test-simple)))
    (should (equal (datagrid--group-rows dg '(2))
                   '((("x") 0 2) (("y") 1 3))))))

(ert-deftest datagrid-test-group-rows-two-keys ()
  (let ((dg (datagrid-test-mk
             (datagrid-column-make :heading "a" :data [1 1 2 1])
             (datagrid-column-make :heading "b" :data ["p" "q" "p" "p"]))))
    (should (equal (datagrid--group-rows dg '(0 1))
                   '(((1 "p") 0 3) ((1 "q") 1) ((2 "p") 2))))))

(ert-deftest datagrid-test-group-rows-no-keys-is-one-group ()
  (let ((dg (datagrid-test-simple)))
    (should (equal (datagrid--group-rows dg nil)
                   '((nil 0 1 2 3))))))

(ert-deftest datagrid-test-group-rows-nil-and-equal-keys ()
  "Nil is its own group and keys are compared with `equal'."
  (let ((dg (datagrid-test-mk
             (datagrid-column-make :heading "k"
                                   :data [nil "a" nil "a" 1 1.0]))))
    (should (equal (datagrid--group-rows dg '(0))
                   '(((nil) 0 2) (("a") 1 3) ((1) 4) ((1.0) 5))))))

(ert-deftest datagrid-test-group-rows-uses-logical-rows ()
  "Row indices are logical, so they match a sorted view."
  (let* ((dg (datagrid-sort (datagrid-test-simple)
                            '(:col "score" :reverse t)))
         (groups (datagrid--group-rows dg '(2))))
    (should (equal groups '((("y") 0 2) (("x") 1 3))))
    (should (equal (datagrid-pull (datagrid--slice-rows
                                   dg (cdr (assoc '("y") groups)))
                                  "name")
                   ["Dave" "Bob"]))))

(ert-deftest datagrid-test-group-rows-empty-datagrid ()
  (let ((dg (datagrid-test-mk
             (datagrid-column-make :heading "g" :data []))))
    (should-not (datagrid--group-rows dg '(0)))
    (should-not (datagrid--group-rows dg nil))))


;;;; datagrid-reduce-vec

(ert-deftest datagrid-test-reduce-vec-sum ()
  (should (= (datagrid-reduce-vec (datagrid-test-simple) #'+ 1) 100)))

(ert-deftest datagrid-test-reduce-vec-with-code ()
  ;; "Agree"=4 "Disagree"=2 "Neutral"=3 "Agree"=4 → sum=13
  (should (= (datagrid-reduce-vec (datagrid-test-coded) #'+ 0 :code t) 13)))

(ert-deftest datagrid-test-reduce-vec-with-convert ()
  (let ((dg (datagrid-test-mk (datagrid-column-make :data ["10" "20" "30"]))))
    (should (= (datagrid-reduce-vec dg #'+ 0 :convert t) 60))))

(defun datagrid-test-nums (&rest nums)
  "Return a one-column datagrid headed \"v\" holding NUMS."
  (datagrid-test-mk (datagrid-column-make :heading "v" :data (vconcat nums))))

(ert-deftest datagrid-test-reduce-vec-max-all-negative ()
  ;; Regression: a 0 seed made this 0.
  (should (= (datagrid-reduce-vec (datagrid-test-nums -3 -7 -1) #'max 0) -1)))

(ert-deftest datagrid-test-reduce-vec-min-all-positive ()
  ;; Regression: a 0 seed made this 0.
  (should (= (datagrid-reduce-vec (datagrid-test-nums 5 3 9) #'min 0) 3)))

(ert-deftest datagrid-test-reduce-vec-product ()
  ;; Regression: a 0 seed made every product 0.
  (should (= (datagrid-reduce-vec (datagrid-test-nums 2 3 4) #'* 0) 24)))

(ert-deftest datagrid-test-reduce-vec-floats ()
  (should (= (datagrid-reduce-vec (datagrid-test-nums 0.5 0.25 -1.5) #'max 0) 0.5)))

(ert-deftest datagrid-test-reduce-vec-single-element ()
  (let ((calls 0))
    (should (= (datagrid-reduce-vec (datagrid-test-nums -4)
                                    (lambda (a b) (cl-incf calls) (+ a b))
                                    0)
               -4))
    (should (= calls 0))))

(ert-deftest datagrid-test-reduce-vec-left-fold-order ()
  ;; Left fold in row order: (- (- 10 3) 2) = 5.
  (should (= (datagrid-reduce-vec (datagrid-test-nums 10 3 2) #'- 0) 5))
  (should (equal (datagrid-reduce-vec (datagrid-test-nums 1 2 3)
                                      (lambda (acc x) (append acc (list x)))
                                      0 :initial-value nil)
                 '(1 2 3))))

(ert-deftest datagrid-test-reduce-vec-non-numeric ()
  (let ((dg (datagrid-test-mk (datagrid-column-make :data ["a" "b" "c"]))))
    (should (equal (datagrid-reduce-vec dg #'concat 0) "abc"))))

(ert-deftest datagrid-test-reduce-vec-empty-column ()
  (let ((dg (datagrid-test-nums)))
    ;; Zero-argument identity when FUNCTION has one.
    (should (equal (datagrid-reduce-vec dg #'+ 0) 0))
    (should (equal (datagrid-reduce-vec dg #'* 0) 1))
    ;; nil when FUNCTION needs arguments.
    (should (null (datagrid-reduce-vec dg #'max 0)))
    (should (null (datagrid-reduce-vec dg (lambda (a b) (+ a b)) 0)))
    ;; INITIAL-VALUE wins on an empty column.
    (should (equal (datagrid-reduce-vec dg #'max 0 :initial-value 7) 7))))

(ert-deftest datagrid-test-reduce-vec-initial-value ()
  (let ((dg (datagrid-test-nums 1 2 3)))
    (should (= (datagrid-reduce-vec dg #'+ 0 :initial-value 100) 106))
    (should (= (datagrid-reduce-vec dg #'max 0 :initial-value 50) 50))
    (should (= (datagrid-reduce-vec dg #'* 0 :initial-value 0) 0))
    ;; An explicit nil seed is honored, not treated as absent.
    (should (equal (datagrid-reduce-vec dg (lambda (acc x) (cons x acc))
                                        0 :initial-value nil)
                   '(3 2 1)))))

(ert-deftest datagrid-test-reduce-vec-heading-string ()
  (should (= (datagrid-reduce-vec (datagrid-test-simple) #'max "score") 40))
  (should (= (datagrid-reduce-vec (datagrid-test-simple) #'min "score") 10)))

(ert-deftest datagrid-test-reduce-vec-code-product ()
  ;; "Agree"=4 "Disagree"=2 "Neutral"=3 "Agree"=4 -> 96, min 2.
  (should (= (datagrid-reduce-vec (datagrid-test-coded) #'* 0 :code t) 96))
  (should (= (datagrid-reduce-vec (datagrid-test-coded) #'min 0 :code t) 2)))

(ert-deftest datagrid-test-reduce-vec-convert-negative-strings ()
  (let ((dg (datagrid-test-mk (datagrid-column-make :data ["-5" "-2" "-9"]))))
    (should (= (datagrid-reduce-vec dg #'max 0 :convert t) -2))
    (should (= (datagrid-reduce-vec dg #'* 0 :convert t) -90))))

(ert-deftest datagrid-test-reduce-vec-respects-row-order ()
  ;; Sorting, slicing, and filtering set a row-order; the reduction
  ;; must see the logical rows only, in logical order.
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "k" :data [3 1 2 4])
              (datagrid-column-make :heading "v" :data [-30 -10 -20 -40])))
         (sorted (datagrid-sort dg "k"))
         (sliced (datagrid-slice dg 0 2))
         (filtered (datagrid-filter-by-mask dg [nil t t nil])))
    (should (equal (datagrid-reduce-vec sorted (lambda (acc x) (append acc (list x)))
                                        "v" :initial-value nil)
                   '(-10 -20 -30 -40)))
    (should (= (datagrid-reduce-vec sorted #'- "v") 80))
    (should (= (datagrid-reduce-vec sliced #'max "v") -20))
    (should (= (datagrid-reduce-vec sliced #'* "v") 600))
    (should (= (datagrid-reduce-vec filtered #'min "v") -20))
    (should (= (datagrid-reduce-vec filtered #'+ "v") -30))))

(ert-deftest datagrid-test-reduce-vec-respects-col-order ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "a" :data [1 2])
              (datagrid-column-make :heading "b" :data [-5 -6])))
         (proj (datagrid-select dg "b" "a")))
    (should (= (datagrid-reduce-vec proj #'max 0) -5))
    (should (= (datagrid-reduce-vec proj #'* 1) 2))))

(ert-deftest datagrid-test-reduce-vec-empty-after-filter ()
  (let ((none (datagrid-filter-by-mask (datagrid-test-nums -1 -2) [nil nil])))
    (should (null (datagrid-reduce-vec none #'max 0)))
    (should (equal (datagrid-reduce-vec none #'+ 0) 0))))

(ert-deftest datagrid-test-reduce-vec-does-not-mutate ()
  (let* ((dg (datagrid-test-nums 3 1 2))
         (before (copy-sequence (datagrid-pull dg 0))))
    (datagrid-reduce-vec dg #'* 0)
    (datagrid-reduce-vec dg #'+ 0 :initial-value 10)
    (should (equal (datagrid-pull dg 0) before))))


;;;; datagrid-prep-for-calc

;; Obsolete since 1.1. These tests stay until the function is
;; removed, which is no earlier than 2027-05-19.

(ert-deftest datagrid-test-prep-for-calc-is-obsolete ()
  (should (get 'datagrid-prep-for-calc 'byte-obsolete-info)))

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
  (should (= (datagrid-calc-function-wrapper "vmedian" '(1 2 3 4)) 2.5)))

(ert-deftest datagrid-test-calc-function-wrapper-max ()
  (should (= (datagrid-calc-function-wrapper "vmax" '(3 1 4 1 5 9)) 9.0)))

(ert-deftest datagrid-test-calc-function-wrapper-count ()
  (should (= (datagrid-calc-function-wrapper "vcount" '(1 2 3)) 3.0)))

(ert-deftest datagrid-test-calc-function-wrapper-ignores-display-modes ()
  ;; Regression: results were printed with the user's display modes
  ;; and read back, so digit grouping turned 1234567 into 1.
  (let ((calc-group-digits t)
        (calc-float-format '(fix 2))
        (calc-point-char ",")
        (calc-number-radix 16))
    (should (equal (datagrid-calc-function-wrapper "vsum" '(1234567)) 1234567))
    (should (equal (datagrid-calc-function-wrapper "vmean" '(1 2 2))
                   1.66666666667))
    (should (equal (datagrid-calc-function-wrapper "vmean" '(1 2)) 1.5))))

(ert-deftest datagrid-test-calc-function-wrapper-number-types ()
  ;; Whole results are integers, others floats.
  (should (equal (datagrid-calc-function-wrapper "vsum" '(1 2 3)) 6))
  (should (equal (datagrid-calc-function-wrapper "vmax" '(-3 -1)) -1))
  (should (equal (datagrid-calc-function-wrapper "vmean" '(1 2)) 1.5))
  ;; Large integers stay exact.
  (should (equal (datagrid-calc-function-wrapper "vsum" '(1234567890123 1))
                 1234567890124))
  (should (equal (datagrid-calc-function-wrapper "vmax" '(1234567890123 1))
                 1234567890123))
  ;; Very small and negative floats survive the round trip.
  (should (equal (datagrid-calc-function-wrapper "vmean" '(1e-30 3e-30)) 2e-30))
  (should (equal (datagrid-calc-function-wrapper "vmean" '(-0.5 -0.25)) -0.375)))

(ert-deftest datagrid-test-calc-function-wrapper-input-forms ()
  ;; Plain Lisp floats work, as do Calc numbers from prep-for-calc.
  (should (equal (datagrid-calc-function-wrapper "vmean" '(1.5 2.5)) 2))
  (should (equal (datagrid-calc-function-wrapper
                  "vmean" (seq-keep #'datagrid-prep-for-calc '(1.5 "2.5" 3)))
                 2.33333333333))
  (should (equal (datagrid-calc-function-wrapper "vsum" '((frac 1 3) (frac 2 3)))
                 1)))

(ert-deftest datagrid-test-calc-function-wrapper-non-real-result-is-nil ()
  ;; Regression: these all printed as non-numbers and read back as 0.
  (should (null (datagrid-calc-function-wrapper "vsdev" '(5))))
  (should (null (datagrid-calc-function-wrapper "vmedian" '())))
  (should (null (datagrid-calc-function-wrapper "vmin" '())))
  (should (null (datagrid-calc-function-wrapper "vgmean" '(-2 8))))
  (should (null (datagrid-calc-function-wrapper "vflat" '(1 2 3)))))

(ert-deftest datagrid-test-calc-function-wrapper-rejects-bad-input ()
  (should-error (datagrid-calc-function-wrapper "vsum" '(1 "2")))
  (should-error (datagrid-calc-function-wrapper "vsum" '(1 nil)))
  (should-error (datagrid-calc-function-wrapper "vsum" '(1 1.0e+INF)))
  (should-error (datagrid-calc-function-wrapper "vsum" '(1 0.0e+NaN))))

(ert-deftest datagrid-test-calc-function-wrapper-uses-internal-prec ()
  (let ((calc-internal-prec 6))
    (should (equal (datagrid-calc-function-wrapper "vmean" '(1 2 2)) 1.66667))))

(ert-deftest datagrid-test-calc-function-wrapper-leaves-modes-alone ()
  (let ((calc-group-digits t) (calc-internal-prec 15))
    (datagrid-calc-function-wrapper "vmean" '(1 2 2))
    (should (eq calc-group-digits t))
    (should (= calc-internal-prec 15))))


;;;; datagrid-reduce-vec-calc

(ert-deftest datagrid-test-reduce-vec-calc-mean ()
  (should (= (datagrid-reduce-vec-calc (datagrid-test-simple) "vmean" 1) 25.0)))

(ert-deftest datagrid-test-reduce-vec-calc-with-code ()
  (should (= (datagrid-reduce-vec-calc (datagrid-test-coded) "vmean" 0 :code t) 3.25)))

(ert-deftest datagrid-test-reduce-vec-calc-nil-returns-nil-on-empty ()
  (let ((dg (datagrid-test-mk (datagrid-column-make :data [nil nil nil]))))
    (should (null (datagrid-reduce-vec-calc dg "vmean" 0 :convert t)))))


;;;; datagrid-column-frequencies

(ert-deftest datagrid-test-column-frequencies-basic ()
  (let* ((dg (datagrid-test-mk (datagrid-column-make :data ["a" "b" "a" "c" "a"])))
         (freq (with-no-warnings (datagrid-column-frequencies dg 0))))
    (should (= (cdr (assoc "a" freq)) 3))
    (should (= (cdr (assoc "b" freq)) 1))
    (should (= (cdr (assoc "c" freq)) 1))))

(ert-deftest datagrid-test-column-frequencies-sorted-descending ()
  (let* ((dg (datagrid-test-mk (datagrid-column-make :data ["a" "b" "a" "b" "b"])))
         (freq (with-no-warnings (datagrid-column-frequencies dg 0))))
    (should (= (cdar freq) 3))
    (should (>= (cdr (nth 0 freq)) (cdr (nth 1 freq))))))

(ert-deftest datagrid-test-column-frequencies-with-code ()
  (let* ((freq (with-no-warnings
                 (datagrid-column-frequencies (datagrid-test-coded) 0 t))))
    (should (= (cdr (assoc 4 freq)) 2))
    (should (= (cdr (assoc 2 freq)) 1))
    (should (= (cdr (assoc 3 freq)) 1))))


;;;; datagrid-column-quartiles

(ert-deftest datagrid-test-column-quartiles-returns-four-keys ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :data [1 3 5 7 9 11 13 15])))
         (q (datagrid-column-quartiles dg 0)))
    (should (assoc "1Q" q))
    (should (assoc "2Q" q))
    (should (assoc "3Q" q))
    (should (assoc "IQR" q))))

(ert-deftest datagrid-test-column-quartiles-iqr-consistent ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :data [1 3 5 7 9 11 13 15])))
         (q (datagrid-column-quartiles dg 0)))
    (should (= (cdr (assoc "IQR" q))
               (- (cdr (assoc "3Q" q)) (cdr (assoc "1Q" q)))))))

(ert-deftest datagrid-test-column-quartiles-known-values ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :data [15 1 9 3 13 5 11 7])))
         (q (datagrid-column-quartiles dg 0)))
    (should (= (cdr (assoc "1Q" q)) 3.5))
    (should (= (cdr (assoc "2Q" q)) 8))
    (should (= (cdr (assoc "3Q" q)) 12.5))))

(ert-deftest datagrid-test-column-quartiles-odd-length-exact ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make
               :data (vconcat (number-sequence 15 1 -1)))))
         (q (datagrid-column-quartiles dg 0)))
    (should (equal (cdr (assoc "1Q" q)) 4))
    (should (equal (cdr (assoc "2Q" q)) 8))
    (should (equal (cdr (assoc "3Q" q)) 12))))

(ert-deftest datagrid-test-column-quartiles-interpolates ()
  (let* ((dg (datagrid-test-mk (datagrid-column-make :data [4 1 3 2])))
         (q (datagrid-column-quartiles dg 0)))
    (should (= (cdr (assoc "1Q" q)) 1.25))
    (should (= (cdr (assoc "2Q" q)) 2.5))
    (should (= (cdr (assoc "3Q" q)) 3.75))))

(ert-deftest datagrid-test-column-quartiles-median-matches-calc ()
  (let ((dg (datagrid-test-mk
             (datagrid-column-make :data [7 2 19 4 11 3 8 30 1 6]))))
    (should (= (cdr (assoc "2Q" (datagrid-column-quartiles dg 0)))
               (datagrid-reduce-vec-calc dg "vmedian" 0)))))

(ert-deftest datagrid-test-column-quartiles-single-value ()
  (let* ((dg (datagrid-test-mk (datagrid-column-make :data [42])))
         (q (datagrid-column-quartiles dg 0)))
    (should (equal (mapcar #'cdr q) '(42 42 42 0)))))

(ert-deftest datagrid-test-column-quartiles-all-nil-returns-nil ()
  (let* ((dg (datagrid-test-mk (datagrid-column-make :data [nil nil nil]))))
    (should (null (datagrid-column-quartiles dg 0)))))

(ert-deftest datagrid-test-column-quartiles-small-column-no-error ()
  (let* ((dg (datagrid-test-mk (datagrid-column-make :data [1 2 3]))))
    (should (datagrid-column-quartiles dg 0)))
  (let* ((dg (datagrid-test-mk (datagrid-column-make :data [42]))))
    (should (datagrid-column-quartiles dg 0))))

(ert-deftest datagrid-test-column-quartiles-ignores-nil-values ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make
               :data [1 nil 3 5 nil 7 9 11 nil 13 15])))
         (q (datagrid-column-quartiles dg 0)))
    (should q)))


;;;; datagrid-column-mode

(ert-deftest datagrid-test-column-mode-unimodal-string ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :data ["a" "b" "a" "c" "a"]))))
    (should (equal (datagrid-column-mode dg 0) '("a")))))

(ert-deftest datagrid-test-column-mode-unimodal-numeric ()
  (let* ((dg (datagrid-test-mk (datagrid-column-make :data [1 2 2 3 3 3]))))
    (should (equal (datagrid-column-mode dg 0) '(3)))))

(ert-deftest datagrid-test-column-mode-empty-returns-nil ()
  (let* ((dg (datagrid-test-mk (datagrid-column-make :data []))))
    (should (null (datagrid-column-mode dg 0)))))

(ert-deftest datagrid-test-column-mode-no-redundant-sort ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :data ["z" "z" "a" "a" "a"])))
         (mode (datagrid-column-mode dg 0)))
    (should (equal mode '("a")))))


;;;; datagrid-column-unique

(ert-deftest datagrid-test-column-unique-removes-duplicates ()
  (let* ((dg (datagrid-test-mk (datagrid-column-make :data ["a" "b" "a" "c"])))
         (uniq (datagrid-column-unique dg 0)))
    (should (= (length uniq) 3))
    (should (seq-contains-p uniq "a" #'equal))
    (should (seq-contains-p uniq "b" #'equal))
    (should (seq-contains-p uniq "c" #'equal))))

(ert-deftest datagrid-test-column-unique-no-duplicates-unchanged ()
  (let* ((dg (datagrid-test-mk (datagrid-column-make :data [1 2 3 4])))
         (uniq (datagrid-column-unique dg 0)))
    (should (= (length uniq) 4))))

(ert-deftest datagrid-test-column-unique-with-code ()
  (let* ((uniq (datagrid-column-unique (datagrid-test-coded) 0 t)))
    (should (= (length uniq) 3))))


;;;; datagrid-column-mad

(ert-deftest datagrid-test-column-mad-basic ()
  (let* ((dg (datagrid-test-mk (datagrid-column-make :data [1 2 3 4 5]))))
    (should (= (datagrid-column-mad dg 0) 1.0))))

(ert-deftest datagrid-test-column-mad-string-data ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :data ["1" "2" "3" "4" "5"]))))
    (should (= (datagrid-column-mad dg 0) 1.0))))


;;;; datagrid-to-vec-of-vec

(ert-deftest datagrid-test-to-vec-of-vec-structure ()
  (let* ((dg (datagrid-test-simple))
         (vov (datagrid-to-vec-of-vec dg)))
    (should (vectorp vov))
    (should (= (length vov) 3))
    (should (equal (aref vov 0) ["Alice" "Bob" "Carol" "Dave"]))
    (should (equal (aref vov 1) [10 20 30 40]))))


;;;; datagrid-to-alist

(ert-deftest datagrid-test-to-alist-without-headings ()
  (let* ((dg (datagrid-test-mk (datagrid-column-make :heading "a" :data [1 2])
                          (datagrid-column-make :heading "b" :data [3 4])))
         (result (datagrid-to-alist dg)))
    (should (equal result '((1 2) (3 4))))))

(ert-deftest datagrid-test-to-alist-with-headings ()
  (let* ((dg (datagrid-test-mk (datagrid-column-make :heading "a" :data [1 2])
                          (datagrid-column-make :heading "b" :data [3 4])))
         (result (datagrid-to-alist dg t)))
    (should (equal result '(("a" 1 2) ("b" 3 4))))))


;;;; Report functions

(defun datagrid-test-approx= (a b &optional tol)
  (< (abs (- a b)) (or tol 1e-6)))

(defun datagrid-test-ratio-dg ()
  (datagrid-test-mk (datagrid-column-make :heading "r"
                                     :data [1 2 3 4 5]
                                     :lom "ratio")))

(defun datagrid-test-interval-dg ()
  (datagrid-test-mk (datagrid-column-make :heading "i"
                                     :data [10 20 30 40]
                                     :lom "interval")))

(defun datagrid-test-ordinal-dg ()
  (datagrid-test-mk (datagrid-column-make :heading "o"
                                     :data [1 2 2 3 4 5]
                                     :lom "ordinal")))


;;;; datagrid-report-nominal

(ert-deftest datagrid-test-report-nominal-heading-first ()
  (let ((report (datagrid-report-nominal (datagrid-test-simple) 0)))
    (should (equal (car report) "name"))))

(ert-deftest datagrid-test-report-nominal-cardinality ()
  (let ((report (datagrid-report-nominal (datagrid-test-simple) 2)))
    (should (= (cdr (assoc "cardinality" report)) 2))))

(ert-deftest datagrid-test-report-nominal-mode ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "h"
                                    :data ["a" "b" "a" "c" "a"]
                                    :lom "nominal")))
         (report (datagrid-report-nominal dg 0)))
    (should (equal (cdr (assoc "mode" report)) '("a")))))

(ert-deftest datagrid-test-report-nominal-frequency-key-present ()
  (let ((report (datagrid-report-nominal (datagrid-test-simple) 2)))
    (should (assoc "frequency - five or fewer" report))))

(ert-deftest datagrid-test-report-nominal-cardinality-over-five ()
  ;; Regression: cardinality was the length of the top-5 list.
  (let* ((dg (datagrid-test-mk (datagrid-column-make
                           :heading "c" :data ["a" "b" "c" "d" "e" "f" "g" "a"])))
         (report (datagrid-report-nominal dg "c")))
    (should (= (cdr (assoc "cardinality" report)) 7))
    (should (= (length (cdr (assoc "frequency - five or fewer" report))) 5))))

(ert-deftest datagrid-test-report-nominal-cardinality-respects-row-order ()
  (let* ((dg (datagrid-test-mk (datagrid-column-make
                           :heading "c" :data ["a" "b" "c" "d" "e" "f" "g"])))
         (sliced (datagrid-slice dg 0 1 2 0)))
    (should (= (cdr (assoc "cardinality" (datagrid-report-nominal sliced 0))) 3))))


;;;; Calc-backed functions

(ert-deftest datagrid-test-reduce-vec-calc-floats-without-convert ()
  ;; Plain Lisp floats used to need :convert t.
  (let ((dg (datagrid-test-mk (datagrid-column-make :heading "v" :data [0.5 0.25 1.75]))))
    (should (equal (datagrid-reduce-vec-calc dg "vsum" "v") 2.5))
    (should (equal (datagrid-reduce-vec-calc dg "vmax" "v") 1.75))))

(ert-deftest datagrid-test-reduce-vec-calc-sorted-grid ()
  (let* ((dg (datagrid-test-mk (datagrid-column-make :heading "k" :data [3 1 2 4])
                          (datagrid-column-make :heading "v" :data [30.5 10 20 40])))
         (view (datagrid-slice (datagrid-sort dg "k") 0 1 2)))
    (should (equal (datagrid-reduce-vec-calc view "vsum" "v") 60.5))
    (should (equal (datagrid-reduce-vec-calc view "vmax" "v") 30.5))))

(ert-deftest datagrid-test-report-interval-single-value ()
  ;; Regression: the standard deviation of one value used to signal.
  (let* ((dg (datagrid-test-mk (datagrid-column-make :heading "v" :data [5])))
         (report (datagrid-report-interval dg "v")))
    (should (equal (cdr (assoc "vmean" report)) 5))
    (should (assoc "vsdev" report))
    (should (null (cdr (assoc "vsdev" report))))))

(ert-deftest datagrid-test-reports-all-nil-column ()
  ;; Regression: reports on an all-nil column used to signal.
  (let ((dg (datagrid-test-mk (datagrid-column-make :heading "v" :data [nil nil]))))
    (dolist (fn '(datagrid-report-ordinal datagrid-report-interval
                                          datagrid-report-ratio))
      (let ((report (funcall fn dg "v")))
        (should (equal (cdr (assoc "vcount" report)) 0))
        (should (null (cdr (assoc "vmedian" report))))))))

(ert-deftest datagrid-test-report-ratio-gmean-of-negative-is-nil ()
  ;; Regression: a complex geometric mean used to come back as 0.
  (let* ((dg (datagrid-test-mk (datagrid-column-make :heading "v" :data [0.5 -0.5 2.0])))
         (report (datagrid-report-ratio dg "v")))
    (should (assoc "vgmean" report))
    (should (null (cdr (assoc "vgmean" report))))
    (should (equal (cdr (assoc "vmax" report)) 2))))

(ert-deftest datagrid-test-column-mad-mixed-floats ()
  ;; Median 4.75; deviations 5.25 2.25 2.25 5.75; their median 3.75.
  (let ((dg (datagrid-test-mk (datagrid-column-make :data [10 2.5 7 -1]))))
    (should (equal (datagrid-column-mad dg 0) 3.75))))

(ert-deftest datagrid-test-column-mad-skips-empty-strings ()
  ;; Regression: "" was dropped from the median but counted as 0 in
  ;; the deviations, giving 4.75.
  (let ((dg (datagrid-test-mk (datagrid-column-make :data ["10" "2.5" "" "7" "-1"]))))
    (should (equal (datagrid-column-mad dg 0) 3.75))))

(ert-deftest datagrid-test-column-mad-coded-and-empty ()
  (should (equal (datagrid-column-mad (datagrid-test-coded) 0 t) 0.5))
  (should (null (datagrid-column-mad
                 (datagrid-test-mk (datagrid-column-make :data [nil nil])) 0))))


;;;; datagrid-report-ordinal

(ert-deftest datagrid-test-report-ordinal-heading-first ()
  (let ((report (datagrid-report-ordinal (datagrid-test-ordinal-dg) 0)))
    (should (equal (car report) "o"))))

(ert-deftest datagrid-test-report-ordinal-stats-keys ()
  (let ((report (datagrid-report-ordinal (datagrid-test-ordinal-dg) 0)))
    (dolist (k '("vcount" "vmin" "vmax" "vmedian" "mode" "quartiles"
                 "median absolute deviation"))
      (should (assoc k report)))))

(ert-deftest datagrid-test-report-ordinal-stat-values ()
  (let ((report (datagrid-report-ordinal (datagrid-test-ordinal-dg) 0)))
    (should (= (cdr (assoc "vcount" report)) 6.0))
    (should (= (cdr (assoc "vmin" report)) 1.0))
    (should (= (cdr (assoc "vmax" report)) 5.0))
    (should (= (cdr (assoc "vmedian" report)) 2.5))))

(ert-deftest datagrid-test-report-ordinal-mode-value ()
  (let ((report (datagrid-report-ordinal (datagrid-test-ordinal-dg) 0)))
    (should (equal (cdr (assoc "mode" report)) '(2)))))

(ert-deftest datagrid-test-report-ordinal-no-mean-or-sdev ()
  (let ((report (datagrid-report-ordinal (datagrid-test-ordinal-dg) 0)))
    (should-not (assoc "vmean" report))
    (should-not (assoc "vsdev" report))))

(ert-deftest datagrid-test-report-ordinal-with-code ()
  (let ((report (datagrid-report-ordinal (datagrid-test-coded) 0 :code t)))
    (should (= (cdr (assoc "vcount" report)) 4.0))
    (should (= (cdr (assoc "vmin" report)) 2.0))
    (should (= (cdr (assoc "vmax" report)) 4.0))))

(ert-deftest datagrid-test-report-ordinal-with-convert ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "s"
                                    :data ["1" "2" "3" "4" "5"]
                                    :lom "ordinal")))
         (report (datagrid-report-ordinal dg 0 :convert t)))
    (should (= (cdr (assoc "vcount" report)) 5.0))
    (should (= (cdr (assoc "vmedian" report)) 3.0))))


;;;; datagrid-report-interval

(ert-deftest datagrid-test-report-interval-heading-first ()
  (let ((report (datagrid-report-interval (datagrid-test-interval-dg) 0)))
    (should (equal (car report) "i"))))

(ert-deftest datagrid-test-report-interval-stats-keys ()
  (let ((report (datagrid-report-interval (datagrid-test-interval-dg) 0)))
    (dolist (k '("vcount" "vmin" "vmax" "vmedian" "vmean" "vsdev"
                 "mode" "quartiles"))
      (should (assoc k report)))))

(ert-deftest datagrid-test-report-interval-stat-values ()
  (let ((report (datagrid-report-interval (datagrid-test-interval-dg) 0)))
    (should (= (cdr (assoc "vcount" report)) 4.0))
    (should (= (cdr (assoc "vmin" report)) 10.0))
    (should (= (cdr (assoc "vmax" report)) 40.0))
    (should (= (cdr (assoc "vmedian" report)) 25.0))
    (should (= (cdr (assoc "vmean" report)) 25.0))
    (should (datagrid-test-approx= (cdr (assoc "vsdev" report)) 12.909944487 1e-4))))

(ert-deftest datagrid-test-report-interval-no-gmean-or-rms ()
  (let ((report (datagrid-report-interval (datagrid-test-interval-dg) 0)))
    (should-not (assoc "vgmean" report))
    (should-not (assoc "rms" report))))


;;;; datagrid-report-ratio

(ert-deftest datagrid-test-report-ratio-heading-first ()
  (let ((report (datagrid-report-ratio (datagrid-test-ratio-dg) 0)))
    (should (equal (car report) "r"))))

(ert-deftest datagrid-test-report-ratio-stats-keys ()
  (let ((report (datagrid-report-ratio (datagrid-test-ratio-dg) 0)))
    (dolist (k '("vcount" "vmin" "vmax" "vmedian" "vmean" "vgmean"
                 "vsdev" "rms" "mode" "quartiles"))
      (should (assoc k report)))))

(ert-deftest datagrid-test-report-ratio-stat-values ()
  (let ((report (datagrid-report-ratio (datagrid-test-ratio-dg) 0)))
    (should (= (cdr (assoc "vcount" report)) 5.0))
    (should (= (cdr (assoc "vmin" report)) 1.0))
    (should (= (cdr (assoc "vmax" report)) 5.0))
    (should (= (cdr (assoc "vmedian" report)) 3.0))
    (should (= (cdr (assoc "vmean" report)) 3.0))
    (should (datagrid-test-approx= (cdr (assoc "vsdev" report)) 1.5811388 1e-4))
    (should (datagrid-test-approx= (cdr (assoc "vgmean" report)) 2.6051710 1e-4))
    (should (datagrid-test-approx= (cdr (assoc "rms" report)) 3.3166247 1e-4))))

(ert-deftest datagrid-test-report-ratio-with-convert ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "s"
                                    :data ["1" "2" "3" "4" "5"]
                                    :lom "ratio")))
         (report (datagrid-report-ratio dg 0 :convert t)))
    (should (= (cdr (assoc "vmean" report)) 3.0))))


;;;; datagrid-report-all-lom dispatch

(ert-deftest datagrid-test-report-all-lom-dispatches-by-lom ()
  (let* ((dg (datagrid-test-mk
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
    (should (assoc "cardinality" (cdr (nth 0 reports))))
    (should (assoc "median absolute deviation" (cdr (nth 1 reports))))
    (should-not (assoc "vmean" (cdr (nth 1 reports))))
    (should (assoc "vmean" (cdr (nth 2 reports))))
    (should-not (assoc "vgmean" (cdr (nth 2 reports))))
    (should (assoc "vgmean" (cdr (nth 3 reports))))
    (should (assoc "rms" (cdr (nth 3 reports))))))

(ert-deftest datagrid-test-report-all-lom-nil-treated-as-nominal ()
  (let* ((dg (datagrid-test-mk (datagrid-column-make :heading "x"
                                                :data ["a" "b" "a"]
                                                :lom nil)))
         (reports (datagrid-report-all-lom dg)))
    (should (assoc "cardinality" (cdr (nth 0 reports))))))


;;;; dplyr-aligned verbs

(ert-deftest datagrid-test-pull-by-index-and-heading ()
  (let ((dg (datagrid-test-simple)))
    (should (equal (datagrid-pull dg 1) [10 20 30 40]))
    (should (equal (datagrid-pull dg "score") [10 20 30 40]))
    (should-error (datagrid-pull dg "missing"))))

(ert-deftest datagrid-test-select-mixes-index-and-heading ()
  (let* ((dg (datagrid-test-simple))
         (out (datagrid-select dg "score" 0)))
    (should (equal (datagrid-get-headings out) ["score" "name"]))
    (should (equal (datagrid-pull out 0) [10 20 30 40]))
    (should (equal (datagrid-pull out 1) ["Alice" "Bob" "Carol" "Dave"]))))

(ert-deftest datagrid-test-rename-single-and-alist ()
  (let* ((dg (datagrid-test-simple))
         (single (datagrid-rename dg "name" "who"))
         (multi (datagrid-rename dg '(("name" . "who") (1 . "pts")))))
    (should (equal (datagrid-get-headings single) ["who" "score" "group"]))
    (should (equal (datagrid-get-headings multi) ["who" "pts" "group"]))
    (should (equal (datagrid-get-headings dg) ["name" "score" "group"]))))

(ert-deftest datagrid-test-slice-keeps-column-metadata ()
  (let* ((dg (datagrid-test-simple))
         (out (datagrid-slice dg 0 2)))
    (should (equal (datagrid-pull out 0) ["Alice" "Carol"]))
    (should (equal (datagrid-pull out 1) [10 30]))
    (should (equal (datagrid-column-lom (datagrid-test-col out 1)) "ratio"))))

(ert-deftest datagrid-test-mutate-appends-and-replaces ()
  (let* ((dg (datagrid-test-simple))
         (added (datagrid-mutate dg "double" (lambda (s) (* 2 s)) 1))
         (replaced (datagrid-mutate dg "score" (lambda (s) (1+ s)) 1)))
    (should (equal (datagrid-get-headings added)
                   ["name" "score" "group" "double"]))
    (should (equal (datagrid-pull added "double") [20 40 60 80]))
    (should (equal (datagrid-get-headings replaced) ["name" "score" "group"]))
    (should (equal (datagrid-pull replaced "score") [11 21 31 41]))))

(ert-deftest datagrid-test-mutate-replace-keeps-metadata ()
  (let* ((code '(("low" . 10) ("high" . 40)))
         (dg (datagrid-column-add-code (datagrid-test-simple) "score" code))
         (out (datagrid-mutate dg "score" #'number-to-string "score"))
         (col (datagrid-test-col out 1)))
    (should (equal (datagrid-pull out "score") ["10" "20" "30" "40"]))
    (should (equal (datagrid-column-lom col) "ratio"))
    (should (equal (datagrid-column-code col) code))
    ;; DATAGRID itself is unchanged.
    (should (equal (datagrid-pull dg "score") [10 20 30 40]))))

(ert-deftest datagrid-test-mutate-rejects-non-string-heading ()
  (let ((dg (datagrid-test-simple)))
    (should-error (datagrid-mutate dg nil #'identity 1))
    (should-error (datagrid-mutate dg 1 #'identity 1))))

(ert-deftest datagrid-test-column-coerce-converts-and-keeps-metadata ()
  (let* ((code '(("low" . 10)))
         (dg (datagrid-column-add-code (datagrid-test-simple) "score" code))
         (out (datagrid-column-coerce
               dg '(("score" . number-to-string) (0 . upcase))))
         (col (datagrid-test-col out 1)))
    (should (equal (datagrid-pull out "name")
                   ["ALICE" "BOB" "CAROL" "DAVE"]))
    (should (equal (datagrid-pull out "score") ["10" "20" "30" "40"]))
    (should (equal (datagrid-column-lom col) "ratio"))
    (should (equal (datagrid-column-code col) code))
    (should (equal (datagrid-pull dg "score") [10 20 30 40]))))

(ert-deftest datagrid-test-column-coerce-list-of-cols ()
  (let* ((dg (datagrid-from-alist '(("a" "1" "2") ("b" "3" "4") ("c" "x" "y"))
                                  t))
         (out (datagrid-column-coerce dg '((("a" "b") . string-to-number)))))
    (should (equal (datagrid-pull out "a") [1 2]))
    (should (equal (datagrid-pull out "b") [3 4]))
    (should (equal (datagrid-pull out "c") ["x" "y"]))))

(ert-deftest datagrid-test-column-coerce-without-headings ()
  (let* ((dg (datagrid-from-alist '(("1" "2") ("x" "y"))))
         (out (datagrid-column-coerce dg '((1 . upcase)))))
    (should (equal (datagrid-pull out 0) ["1" "2"]))
    (should (equal (datagrid-pull out 1) ["X" "Y"]))))

(ert-deftest datagrid-test-column-coerce-respects-row-order ()
  (let* ((dg (datagrid-sort (datagrid-test-simple) '(:col "score" :reverse t)))
         (out (datagrid-column-coerce dg '(("score" . 1+)))))
    (should (equal (datagrid-pull out "score") [41 31 21 11]))))

(ert-deftest datagrid-test-column-coerce-errors ()
  (let ((dg (datagrid-test-simple)))
    (should-error (datagrid-column-coerce dg '(("nope" . identity))))
    (should-error (datagrid-column-coerce dg '((9 . identity))))
    (should-error (datagrid-column-coerce dg '(("score" . not-a-fn))))
    (should-error (datagrid-column-coerce
                   dg '(("score" . identity) (1 . identity))))))

(ert-deftest datagrid-test-mutate-multiple-source-cols ()
  (let* ((dg (datagrid-test-simple))
         (out (datagrid-mutate dg "label" (lambda (n s) (format "%s:%d" n s))
                               0 1)))
    (should (equal (datagrid-pull out "label")
                   ["Alice:10" "Bob:20" "Carol:30" "Dave:40"]))))

(ert-deftest datagrid-test-count-returns-datagrid ()
  (let* ((dg (datagrid-test-mk (datagrid-column-make
                           :heading "name"
                           :data ["a" "b" "a" "c" "b" "a"])))
         (out (datagrid-count dg "name"))
         (sorted (datagrid-count dg "name" :sort t :name "freq")))
    (should (equal (datagrid-get-headings out) ["name" "n"]))
    (should (equal (datagrid-pull out 0) ["a" "b" "c"]))
    (should (equal (datagrid-pull out 1) [3 2 1]))
    (should (equal (datagrid-get-headings sorted) ["name" "freq"]))
    (should (equal (datagrid-pull sorted 1) [3 2 1]))))

(ert-deftest datagrid-test-distinct-row-and-column-forms ()
  (let* ((dg (datagrid-test-mk (datagrid-column-make
                           :heading "name" :data ["a" "b" "a" "c" "b"])
                          (datagrid-column-make
                           :heading "v" :data [1 2 1 3 9])))
         (col-only (datagrid-distinct dg "name"))
         (rows (datagrid-distinct dg))
         (subset (datagrid-distinct dg "name" "v")))
    (should (equal (datagrid-get-headings col-only) ["name"]))
    (should (equal (datagrid-pull col-only 0) ["a" "b" "c"]))
    (should (equal (datagrid-pull rows 0) ["a" "b" "c" "b"]))
    (should (equal (datagrid-pull rows 1) [1 2 3 9]))
    (should (equal (datagrid-pull subset 0) ["a" "b" "c" "b"]))))

(ert-deftest datagrid-test-summarize-returns-one-row-datagrid ()
  (let* ((dg (datagrid-test-simple))
         (out (datagrid-summarize
               dg
               '((:name "sum" :fn + :col 1)
                 (:name "max" :fn max :col "score")))))
    (should (datagridp out))
    (should (equal (datagrid-dimensions out) '(2 . 1)))
    (should (equal (datagrid-to-alist out t) '(("sum" 100) ("max" 40))))))

(ert-deftest datagrid-test-summarize-negative-and-product ()
  ;; Regression: the 0 seed leaked through summarize too.
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "v" :data [-4 -2 -8])))
         (out (datagrid-summarize
               dg
               '((:name "max" :fn max :col "v")
                 (:name "min" :fn min :col "v")
                 (:name "prod" :fn * :col 0)
                 (:name "sum" :fn + :col 0)))))
    (should (equal (datagrid-to-alist out t)
                   '(("max" -2) ("min" -8) ("prod" -64) ("sum" -14))))))

(ert-deftest datagrid-test-summarize-initial-value ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "v" :data [1 2 3])))
         (out (datagrid-summarize
               dg
               '((:name "seeded" :fn + :col "v" :initial-value 10)
                 (:name "nil-seed" :fn (lambda (acc x) (cons x acc))
                        :col "v" :initial-value nil)
                 (:name "unseeded" :fn + :col "v")))))
    (should (equal (datagrid-to-alist out t)
                   '(("seeded" 16)
                     ("nil-seed" (3 2 1))
                     ("unseeded" 6))))))

(ert-deftest datagrid-test-summarize-calc-ignores-initial-value ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "v" :data [1 2 3])))
         (out (datagrid-summarize
               dg '((:name "m" :fn "vmax" :col "v" :calc t
                           :initial-value 100)))))
    (should (equal (datagrid-to-alist out t) '(("m" 3))))))

(ert-deftest datagrid-test-summarize-old-forms-explain-change ()
  "Calls in the pre-1.1 form signal an error that names the change."
  (let ((dg (datagrid-test-simple)))
    (dolist (thunk
             (list (lambda ()
                     (datagrid-summarize dg '(:name "sum" :fn + :col 1)))
                   (lambda ()
                     (datagrid-summarize dg
                                         '(:name "sum" :fn + :col 1)
                                         '(:name "max" :fn max :col 1)))))
      (let ((err (should-error (funcall thunk))))
        (should (string-match-p "Since version 1\\.1"
                                (error-message-string err)))))))

(ert-deftest datagrid-test-summarize-rejects-unknown-keyword ()
  (let* ((dg (datagrid-test-simple))
         (err (should-error
               (datagrid-summarize dg '((:name "sum" :fn + :col 1))
                                   :bye "group"))))
    (should (string-match-p "Unknown keyword argument :bye"
                            (error-message-string err)))))

(ert-deftest datagrid-test-summarize-help-shows-new-signature ()
  (should (string-match-p
           "(fn DATAGRID SPECS &key BY)"
           (documentation 'datagrid-summarize t))))

(ert-deftest datagrid-test-summarize-ungrouped-empty-datagrid ()
  "Without BY, an empty datagrid still gives one row."
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "v" :data [])))
         (out (datagrid-summarize
               dg '((:name "sum" :fn + :col "v")
                    (:name "max" :fn max :col "v")))))
    (should (equal (datagrid-to-alist out t) '(("sum" 0) ("max" nil))))))

(ert-deftest datagrid-test-summarize-by-one-column ()
  (let* ((dg (datagrid-test-simple))
         (out (datagrid-summarize
               dg
               '((:name "total" :fn + :col "score")
                 (:name "top" :fn max :col "score"))
               :by "group")))
    (should (equal (datagrid-get-headings out) ["group" "total" "top"]))
    (should (equal (datagrid-pull out "group") ["x" "y"]))
    (should (equal (datagrid-pull out "total") [40 60]))
    (should (equal (datagrid-pull out "top") [30 40]))))

(ert-deftest datagrid-test-summarize-by-index-and-list-forms-agree ()
  (let* ((dg (datagrid-test-simple))
         (specs '((:name "total" :fn + :col "score")))
         (by-heading (datagrid-summarize dg specs :by "group"))
         (by-index (datagrid-summarize dg specs :by 2))
         (by-list (datagrid-summarize dg specs :by '("group"))))
    (should (equal (datagrid-to-alist by-heading t)
                   (datagrid-to-alist by-index t)))
    (should (equal (datagrid-to-alist by-heading t)
                   (datagrid-to-alist by-list t)))))

(ert-deftest datagrid-test-summarize-by-two-columns ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "site"
                                    :data ["a" "a" "b" "a" "b"])
              (datagrid-column-make :heading "year"
                                    :data [1 2 1 1 1])
              (datagrid-column-make :heading "v"
                                    :data [1 10 100 2 200])))
         (out (datagrid-summarize
               dg '((:name "sum" :fn + :col "v"))
               :by '("site" "year"))))
    (should (equal (datagrid-get-headings out) ["site" "year" "sum"]))
    (should (equal (datagrid-pull out "site") ["a" "a" "b"]))
    (should (equal (datagrid-pull out "year") [1 2 1]))
    (should (equal (datagrid-pull out "sum") [3 10 300]))))

(ert-deftest datagrid-test-summarize-by-keeps-key-metadata ()
  "Key columns keep their level of measurement and coding."
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "rating"
                                    :data ["Agree" "Neutral" "Agree"]
                                    :lom "ordinal"
                                    :code '(("Neutral" . 3)
                                            ("Agree" . 4)))
              (datagrid-column-make :heading "v" :data [1 2 3])))
         (out (datagrid-summarize
               dg '((:name "sum" :fn + :col "v")) :by "rating"))
         (key (aref (datagrid-columns out) 0))
         (sum (aref (datagrid-columns out) 1)))
    (should (equal (datagrid-column-lom key) "ordinal"))
    (should (equal (datagrid-column-code key)
                   '(("Neutral" . 3) ("Agree" . 4))))
    (should (equal (datagrid-column-decode out "rating") [4 3]))
    (should-not (datagrid-column-lom sum))
    (should (equal (datagrid-pull out "sum") [4 2]))))

(ert-deftest datagrid-test-summarize-by-code-and-calc-per-group ()
  "Spec options such as :code and :calc apply within each group."
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "g" :data ["a" "b" "a" "b"])
              (datagrid-column-make :heading "r"
                                    :data ["lo" "hi" "hi" "hi"]
                                    :code '(("lo" . 1) ("hi" . 5)))))
         (out (datagrid-summarize
               dg '((:name "coded-sum" :fn + :col "r" :code t)
                    (:name "coded-max" :fn "vmax" :col "r"
                           :code t :calc t))
               :by "g")))
    (should (equal (datagrid-pull out "coded-sum") [6 10]))
    (should (equal (datagrid-pull out "coded-max") [5 5]))))

(ert-deftest datagrid-test-summarize-by-nil-key-is-a-group ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "g" :data ["a" nil "a" nil])
              (datagrid-column-make :heading "v" :data [1 2 3 4])))
         (out (datagrid-summarize
               dg '((:name "sum" :fn + :col "v")) :by "g")))
    (should (equal (datagrid-pull out "g") ["a" nil]))
    (should (equal (datagrid-pull out "sum") [4 6]))))

(ert-deftest datagrid-test-summarize-by-follows-row-order ()
  "Groups appear in the datagrid's logical row order."
  (let* ((dg (datagrid-sort (datagrid-test-simple)
                            '(:col "score" :reverse t)))
         (out (datagrid-summarize
               dg '((:name "first" :fn (lambda (acc _x) acc)
                           :col "name"))
               :by "group")))
    (should (equal (datagrid-pull out "group") ["y" "x"]))
    (should (equal (datagrid-pull out "first") ["Dave" "Carol"]))))

(ert-deftest datagrid-test-summarize-by-on-filtered-view ()
  "Grouping a filtered view sees only the kept rows."
  (let* ((dg (datagrid-filter-by-mask (datagrid-test-simple)
                                      [t t nil t]))
         (out (datagrid-summarize
               dg '((:name "total" :fn + :col "score")) :by "group")))
    (should (equal (datagrid-pull out "group") ["x" "y"]))
    (should (equal (datagrid-pull out "total") [10 60]))))

(ert-deftest datagrid-test-summarize-by-empty-datagrid ()
  "With BY and no rows, the result has headings and no rows."
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "g" :data [] :lom "nominal")
              (datagrid-column-make :heading "v" :data [])))
         (out (datagrid-summarize
               dg '((:name "sum" :fn + :col "v")) :by "g")))
    (should (equal (datagrid-get-headings out) ["g" "sum"]))
    (should (equal (datagrid-dimensions out) '(2 . 0)))))

(ert-deftest datagrid-test-summarize-bad-column-errors-without-rows ()
  "Column references are checked even when there are no groups."
  (let ((dg (datagrid-test-mk
             (datagrid-column-make :heading "g" :data [])
             (datagrid-column-make :heading "v" :data []))))
    (should-error (datagrid-summarize
                   dg '((:name "sum" :fn + :col "missing")) :by "g"))
    (should-error (datagrid-summarize
                   dg '((:name "sum" :fn + :col "v")) :by "missing"))))

(ert-deftest datagrid-test-summarize-chains-with-thread-first ()
  (let ((out (thread-first (datagrid-test-simple)
               (datagrid-summarize '((:name "total" :fn + :col "score"))
                                   :by "group")
               (datagrid-sort '(:col "total" :reverse t)))))
    (should (equal (datagrid-pull out "group") ["y" "x"]))
    (should (equal (datagrid-pull out "total") [60 40]))))


(ert-deftest datagrid-test-rows-patch-fills-only-empties ()
  (let* ((a (datagrid-test-mk (datagrid-column-make :heading "id" :data [1 2 3 4])
                         (datagrid-column-make :heading "name"
                                               :data ["alice" nil "" "dave"])))
         (b (datagrid-test-mk (datagrid-column-make :heading "id" :data [1 2 3 5])
                         (datagrid-column-make :heading "name"
                                               :data ["A." "bob" "carol" "ed"])))
         (out (datagrid-rows-patch a b :on "id"
                                   :cols '(("name" . "name")))))
    (should (equal (datagrid-pull out "name")
                   ["alice" "bob" "carol" "dave"]))
    (should (equal (datagrid-pull out "id") [1 2 3 4]))
    (should (equal (datagrid-pull a "name") ["alice" nil "" "dave"]))))

(ert-deftest datagrid-test-rows-patch-on-cons-and-int-cols ()
  (let* ((a (datagrid-test-mk (datagrid-column-make :heading "akey" :data [1 2 3])
                         (datagrid-column-make :heading "v"    :data [nil nil 9])))
         (b (datagrid-test-mk (datagrid-column-make :heading "bkey" :data [2 1 3])
                         (datagrid-column-make :heading "w"    :data [20 10 30])))
         (out (datagrid-rows-patch a b :on '("akey" . "bkey")
                                   :cols '((1 . 1)))))
    (should (equal (datagrid-pull out 1) [10 20 9]))))

(ert-deftest datagrid-test-rows-patch-on-errors ()
  (let ((a (datagrid-test-simple))
        (cols '(("name" . "name"))))
    (should-error (datagrid-rows-patch a a :cols cols))
    (should-error (datagrid-rows-patch a a :on 5.0 :cols cols))
    (should-error (datagrid-rows-patch a a :on "nope" :cols cols))
    (should-error (datagrid-rows-patch a [1 2] :on "name" :cols cols)
                  :type 'wrong-type-argument)))

(ert-deftest datagrid-test-resolve-on-shared-ref ()
  ;; One ref names the column in both grids, which may sit at
  ;; different indices.
  (let ((a (datagrid-test-simple))
        (b (datagrid-select (datagrid-test-simple) "score" "name")))
    (should (equal (datagrid--resolve-on "name" a b) '(0 . 1)))
    (should (equal (datagrid--resolve-on 1 a b) '(1 . 1)))))

(ert-deftest datagrid-test-resolve-on-cons ()
  (let ((a (datagrid-test-simple)))
    (should (equal (datagrid--resolve-on '("score" . 0) a a) '(1 . 0)))))

(ert-deftest datagrid-test-resolve-on-errors ()
  (let ((a (datagrid-test-simple)))
    (should-error (datagrid--resolve-on nil a a))
    (should-error (datagrid--resolve-on :id a a))
    (should-error (datagrid--resolve-on "nope" a a))
    (should-error (datagrid--resolve-on "name" a nil)
                  :type 'wrong-type-argument)
    (should-error (datagrid--resolve-on "name" 'x a)
                  :type 'wrong-type-argument)))


(ert-deftest datagrid-test-coalesce-merges-and-drops ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "id"      :data [1 2 3 4])
              (datagrid-column-make :heading "email"   :data ["a" nil "c" ""])
              (datagrid-column-make :heading "email_2" :data ["A" "b" "" "d"])
              (datagrid-column-make :heading "phone"   :data [nil "p2" nil nil])
              (datagrid-column-make :heading "phone_2" :data ["q1" nil "q3" nil])))
         (out (datagrid-coalesce dg
                                 '("email" "email_2")
                                 '("phone" "phone_2"))))
    (should (equal (datagrid-get-headings out) ["id" "email" "phone"]))
    (should (equal (datagrid-pull out "email") ["a" "b" "c" "d"]))
    (should (equal (datagrid-pull out "phone") ["q1" "p2" "q3" nil]))
    (should (equal (datagrid-get-headings dg)
                   ["id" "email" "email_2" "phone" "phone_2"]))))


;;;; datagrid-pivot-wider

(defun datagrid-test-long ()
  "Return a long datagrid of ids, years, metrics, values, and notes."
  (datagrid-test-mk
   (datagrid-column-make :heading "id" :data ["a" "a" "b" "b" "a"]
                         :lom "nominal")
   (datagrid-column-make :heading "year" :data [2024 2024 2024 2024 2025])
   (datagrid-column-make :heading "metric"
                         :data ["fte" "exp" "fte" "exp" "fte"])
   (datagrid-column-make :heading "value" :data [10 100 20 200 11]
                         :lom "ratio")
   (datagrid-column-make :heading "note" :data ["x" "x" "y" "y" "z"])))

(ert-deftest datagrid-test-pivot-wider-explicit-id-cols ()
  (let ((out (datagrid-pivot-wider (datagrid-test-long) "metric" "value"
                                   :id-cols '("id" "year"))))
    (should (equal (datagrid-get-headings out) ["id" "year" "fte" "exp"]))
    (should (equal (datagrid-pull out "id") ["a" "b" "a"]))
    (should (equal (datagrid-pull out "year") [2024 2024 2025]))
    (should (equal (datagrid-pull out "fte") [10 20 11]))
    (should (equal (datagrid-pull out "exp") [100 200 nil]))))

(ert-deftest datagrid-test-pivot-wider-default-id-cols ()
  (let ((out (datagrid-pivot-wider (datagrid-test-long) "metric" "value")))
    (should (equal (datagrid-get-headings out)
                   ["id" "year" "note" "fte" "exp"]))
    (should (equal (datagrid-pull out "note") ["x" "y" "z"]))))

(ert-deftest datagrid-test-pivot-wider-integer-refs ()
  (should (equal (datagrid-to-alist
                  (datagrid-pivot-wider (datagrid-test-long) 2 3
                                        :id-cols '(0 1)))
                 (datagrid-to-alist
                  (datagrid-pivot-wider (datagrid-test-long) "metric" "value"
                                        :id-cols '("id" "year"))))))

(ert-deftest datagrid-test-pivot-wider-values-fill ()
  (let ((out (datagrid-pivot-wider (datagrid-test-long) "metric" "value"
                                   :id-cols '("id" "year")
                                   :values-fill 0)))
    (should (equal (datagrid-pull out "exp") [100 200 0]))))

(ert-deftest datagrid-test-pivot-wider-values-fn ()
  (let ((out (datagrid-pivot-wider (datagrid-test-long) "metric" "value"
                                   :id-cols '("id")
                                   :values-fn (lambda (vs)
                                                (apply #'+ vs)))))
    (should (equal (datagrid-pull out "fte") [21 20]))
    (should (equal (datagrid-pull out "exp") [100 200]))))

(ert-deftest datagrid-test-pivot-wider-duplicates-list-and-warn ()
  (let* ((warned nil)
         (out (cl-letf (((symbol-function 'display-warning)
                         (lambda (&rest args) (setq warned args))))
                (datagrid-pivot-wider (datagrid-test-long) "metric" "value"
                                      :id-cols '("id")))))
    (should (equal (datagrid-pull out "fte") [(10 11) 20]))
    (should (equal (datagrid-pull out "exp") [100 200]))
    (should (string-match-p "1 cells" (cadr warned)))))

(ert-deftest datagrid-test-pivot-wider-formats-headings ()
  (let ((out (datagrid-pivot-wider (datagrid-test-long) "year" "value"
                                   :id-cols '("metric")
                                   :values-fn (lambda (vs)
                                                (apply #'+ vs)))))
    (should (equal (datagrid-get-headings out) ["metric" "2024" "2025"]))
    (should (equal (datagrid-pull out "2025") [11 nil]))))

(ert-deftest datagrid-test-pivot-wider-keeps-column-metadata ()
  (let* ((out (datagrid-pivot-wider (datagrid-test-long) "metric" "value"
                                    :id-cols '("id" "year")))
         (cols (datagrid-columns out)))
    (should (equal (datagrid-column-lom (aref cols 0)) "nominal"))
    (should (equal (datagrid-column-lom (aref cols 2)) "ratio"))))

(ert-deftest datagrid-test-pivot-wider-respects-row-order ()
  (let ((out (datagrid-pivot-wider
              (datagrid-sort (datagrid-test-long) '(:col "id" :reverse t))
              "metric" "value" :id-cols '("id" "year"))))
    (should (equal (datagrid-pull out "id") ["b" "a" "a"]))))

(ert-deftest datagrid-test-pivot-wider-does-not-mutate-source ()
  (let* ((dg (datagrid-test-long))
         (before (datagrid-to-alist dg t)))
    (datagrid-pivot-wider dg "metric" "value" :id-cols '("id" "year"))
    (should (equal (datagrid-to-alist dg t) before))))


;;;; datagrid-pivot-longer

(defun datagrid-test-wide ()
  "Return a wide datagrid of ids, years, and two ratio metrics."
  (datagrid-test-mk
   (datagrid-column-make :heading "id" :data ["a" "b" "a"] :lom "nominal")
   (datagrid-column-make :heading "year" :data [2024 2024 2025])
   (datagrid-column-make :heading "fte" :data [10 20 11] :lom "ratio")
   (datagrid-column-make :heading "exp" :data [100 200 nil] :lom "ratio")))

(ert-deftest datagrid-test-pivot-longer-basic ()
  (let ((out (datagrid-pivot-longer (datagrid-test-wide) '("fte" "exp"))))
    (should (equal (datagrid-get-headings out) ["id" "year" "name" "value"]))
    (should (equal (datagrid-pull out "id") ["a" "a" "b" "b" "a" "a"]))
    (should (equal (datagrid-pull out "year")
                   [2024 2024 2024 2024 2025 2025]))
    (should (equal (datagrid-pull out "name")
                   ["fte" "exp" "fte" "exp" "fte" "exp"]))
    (should (equal (datagrid-pull out "value") [10 100 20 200 11 nil]))))

(ert-deftest datagrid-test-pivot-longer-headings-and-drop-empty ()
  (let ((out (datagrid-pivot-longer (datagrid-test-wide) '("fte" "exp")
                                    :headings-to "metric"
                                    :values-to "amount"
                                    :values-drop-empty t)))
    (should (equal (datagrid-get-headings out)
                   ["id" "year" "metric" "amount"]))
    (should (equal (datagrid-pull out "metric")
                   ["fte" "exp" "fte" "exp" "fte"]))
    (should (equal (datagrid-pull out "amount") [10 100 20 200 11]))))

(ert-deftest datagrid-test-pivot-longer-integer-refs ()
  (should (equal (datagrid-to-alist
                  (datagrid-pivot-longer (datagrid-test-wide) '(2 3)))
                 (datagrid-to-alist
                  (datagrid-pivot-longer (datagrid-test-wide)
                                         '("fte" "exp"))))))

(ert-deftest datagrid-test-pivot-longer-cols-order ()
  (let ((out (datagrid-pivot-longer (datagrid-test-wide) '("exp" "fte"))))
    (should (equal (seq-take (datagrid-pull out "name") 2) ["exp" "fte"]))))

(ert-deftest datagrid-test-pivot-longer-keeps-column-metadata ()
  (let ((cols (datagrid-columns
               (datagrid-pivot-longer (datagrid-test-wide) '("fte" "exp")))))
    (should (equal (datagrid-column-lom (aref cols 0)) "nominal"))
    (should (equal (datagrid-column-lom (aref cols 2)) "nominal"))
    (should (equal (datagrid-column-lom (aref cols 3)) "ratio")))
  (let ((cols (datagrid-columns
               (datagrid-pivot-longer (datagrid-test-wide) '("year" "fte")))))
    (should (null (datagrid-column-lom (aref cols 3))))))

(ert-deftest datagrid-test-pivot-longer-respects-orders ()
  (let* ((dg (datagrid-select
              (datagrid-sort (datagrid-test-wide) '(:col "fte" :reverse t))
              "fte" "id"))
         (out (datagrid-pivot-longer dg '("fte"))))
    (should (equal (datagrid-get-headings out) ["id" "name" "value"]))
    (should (equal (datagrid-pull out "id") ["b" "a" "a"]))
    (should (equal (datagrid-pull out "value") [20 11 10]))))

(ert-deftest datagrid-test-pivot-longer-inverts-pivot-wider ()
  (let* ((long (datagrid-select (datagrid-test-long)
                                "id" "year" "metric" "value"))
         (round-trip (datagrid-pivot-longer
                      (datagrid-pivot-wider long "metric" "value")
                      '("fte" "exp")
                      :headings-to "metric" :values-drop-empty t)))
    (should (equal (datagrid-to-alist
                    (datagrid-sort round-trip "id" "year" "metric"))
                   (datagrid-to-alist
                    (datagrid-sort long "id" "year" "metric"))))))

(ert-deftest datagrid-test-pivot-longer-errors ()
  (let ((dg (datagrid-test-wide)))
    (should-error (datagrid-pivot-longer dg nil))
    (should-error (datagrid-pivot-longer dg '("nope")))
    (should-error (datagrid-pivot-longer dg '("fte") :headings-to "id"))
    (should-error (datagrid-pivot-longer dg '("fte") :values-to "year"))
    (should-error (datagrid-pivot-longer dg '("fte")
                                         :headings-to "x" :values-to "x"))
    (should-error (datagrid-pivot-longer dg '("fte") :values-to 'v))
    (should-error (datagrid-pivot-longer
                   (datagrid-test-mk (datagrid-column-make :data [1 2])
                                     (datagrid-column-make :heading "k"
                                                           :data [3 4]))
                   '(0)))))

(ert-deftest datagrid-test-pivot-longer-does-not-mutate-source ()
  (let* ((dg (datagrid-test-wide))
         (before (datagrid-to-alist dg t)))
    (datagrid-pivot-longer dg '("fte" "exp"))
    (should (equal (datagrid-to-alist dg t) before))))



;;;; datagrid-group-modify

(ert-deftest datagrid-test-group-modify-one-row-per-group ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "g"   :data ["a" "b" "a" "b" "a"])
              (datagrid-column-make :heading "v"   :data [1 10 2 20 3])
              (datagrid-column-make :heading "tag" :data ["x" "y" "x" "y" "x"])))
         (out (datagrid-group-modify
               dg
               (lambda (sub)
                 (let ((vs (datagrid-pull sub "v"))
                       (ks (datagrid-pull sub "g")))
                   (datagrid-test-mk
                    (datagrid-column-make :heading "g"   :data (vector (aref ks 0)))
                    (datagrid-column-make :heading "sum" :data (vector (seq-reduce #'+ vs 0))))))
               "g")))
    (should (equal (datagrid-get-headings out) ["g" "sum"]))
    (should (equal (datagrid-pull out "g")   ["a" "b"]))
    (should (equal (datagrid-pull out "sum") [6 30]))))

(ert-deftest datagrid-test-group-modify-multi-row-per-group ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "g" :data ["a" "b" "a" "b"])
              (datagrid-column-make :heading "v" :data [1 10 2 20])))
         (out (datagrid-group-modify
               dg
               (lambda (sub)
                 (let* ((vs (datagrid-pull sub "v"))
                        (ks (datagrid-pull sub "g"))
                        (n  (length vs)))
                   (datagrid-test-mk
                    (datagrid-column-make :heading "g" :data (make-vector n (aref ks 0)))
                    (datagrid-column-make :heading "v" :data vs))))
               "g")))
    (should (equal (datagrid-get-headings out) ["g" "v"]))
    (should (equal (datagrid-pull out "g") ["a" "a" "b" "b"]))
    (should (equal (datagrid-pull out "v") [1 2 10 20]))))

(ert-deftest datagrid-test-group-modify-empty-input ()
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "g" :data [])
              (datagrid-column-make :heading "v" :data [])))
         (out (datagrid-group-modify dg #'identity "g")))
    (should (datagridp out))
    (should (zerop (datagrid--ncols out)))))

(ert-deftest datagrid-test-group-modify-schema-mismatch ()
  (let ((dg (datagrid-test-mk
             (datagrid-column-make :heading "g" :data ["a" "b"])
             (datagrid-column-make :heading "v" :data [1 2]))))
    (should-error
     (datagrid-group-modify
      dg
      (lambda (sub)
        (let ((ks (datagrid-pull sub "g"))
              (vs (datagrid-pull sub "v")))
          (if (equal (aref ks 0) "a")
              (datagrid-test-mk (datagrid-column-make :heading "g" :data (vector (aref ks 0)))
                           (datagrid-column-make :heading "v" :data vs))
            (datagrid-test-mk (datagrid-column-make :heading "g"     :data (vector (aref ks 0)))
                         (datagrid-column-make :heading "other" :data vs)))))
      "g"))))

(ert-deftest datagrid-test-group-modify-first-result-not-datagrid ()
  ;; The first group's result must be validated too, not just the rest.
  (let* ((dg (datagrid-test-mk
              (datagrid-column-make :heading "g" :data ["a" "b"])
              (datagrid-column-make :heading "v" :data [1 2])))
         (err (should-error
               (datagrid-group-modify
                dg
                (lambda (sub)
                  (if (equal (aref (datagrid-pull sub "g") 0) "a")
                      'not-a-datagrid
                    sub))
                "g"))))
    (should (string-match-p "not a datagrid" (error-message-string err)))))


;;;; Logical print representation

(ert-deftest datagrid-test-print-summary-reports-logical-dims ()
  "`cl-print-object' reports logical, not physical, dimensions."
  (let* ((dg (datagrid-test-simple))
         (out (cl-prin1-to-string (datagrid-select dg "score"))))
    (should (string-prefix-p "#<datagrid 1 col x 4 rows" out))
    (should (string-match-p "cols: \\[\"score\"\\]" out))))

(ert-deftest datagrid-test-print-hides-unselected-columns ()
  "A projection prints only its selected column, not the shared spine."
  (let* ((dg (datagrid-test-simple))
         (out (cl-prin1-to-string (datagrid-select dg "score"))))
    ;; The physical struct still holds name/group; they must not appear.
    (should-not (string-match-p "Alice" out))
    (should-not (string-match-p "name" out))
    (should (string-match-p "score" out))))

(ert-deftest datagrid-test-print-truncates-head ()
  "Only `datagrid-print-head-rows' rows print, with a remainder count."
  (let* ((dg (datagrid-test-simple))
         (datagrid-print-head-rows 2)
         (out (cl-prin1-to-string dg)))
    (should (string-match-p "Alice" out))
    (should (string-match-p "Bob" out))
    (should-not (string-match-p "Carol" out))
    (should (string-match-p "(2 more)" out))))

(ert-deftest datagrid-test-print-empty-datagrid ()
  "An empty datagrid prints a bare summary with no head."
  (let ((out (cl-prin1-to-string (datagrid-make :columns (vector)))))
    (should (string= out "#<datagrid 0 cols x 0 rows>"))))

(ert-deftest datagrid-test-describe-returns-datagrid ()
  "`datagrid-describe' prints the logical view and returns its argument."
  (let* ((dg (datagrid-test-simple))
         (out (with-output-to-string (should (eq (datagrid-describe dg) dg)))))
    (should (string-prefix-p "#<datagrid 3 cols x 4 rows" out))))


(provide 'datagrid-test)
;;; datagrid-test.el ends here
