;;; datagrid-join-test.el --- ERT tests for datagrid-join.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Joshua Lambert

;;; Code:
(require 'ert)
(add-to-list 'load-path
	     (file-name-directory (or load-file-name buffer-file-name)))
(require 'datagrid)
(require 'datagrid-join)


;;;; Fixtures

(defun dg-join-test--dg1 ()
  "Three-row datagrid with id, val."
  (vector
   (datagrid-column-make :heading "id"  :data ["a" "b" "c"])
   (datagrid-column-make :heading "val" :data [1 2 3])))

(defun dg-join-test--dg2 ()
  "Three-row datagrid with id, label, score (id overlaps dg1 except for \"c\")."
  (vector
   (datagrid-column-make :heading "id"    :data ["b" "a" "d"])
   (datagrid-column-make :heading "label" :data ["bee" "ay" "dee"])
   (datagrid-column-make :heading "score" :data [20 10 40])))


;;;; datagrid-left-join

(ert-deftest datagrid-join-test-left-basic ()
  (let* ((dg1 (dg-join-test--dg1))
	 (dg2 (dg-join-test--dg2))
	 (r (datagrid-left-join dg1 dg2 :on 0)))
    (should (= (length r) 4))
    (should (equal (datagrid-column-data (aref r 0)) ["a" "b" "c"]))
    (should (equal (datagrid-column-data (aref r 2)) ["ay" "bee" nil]))
    (should (equal (datagrid-column-data (aref r 3)) [10 20 nil]))))

(ert-deftest datagrid-join-test-left-on-string ()
  (let* ((r (datagrid-left-join (dg-join-test--dg1) (dg-join-test--dg2)
				:on "id")))
    (should (equal (datagrid-column-data (aref r 2)) ["ay" "bee" nil]))))

(ert-deftest datagrid-join-test-left-on-cons ()
  (let* ((dg1 (vector (datagrid-column-make :heading "k1" :data ["a" "b"])
		      (datagrid-column-make :heading "v"  :data [1 2])))
	 (dg2 (vector (datagrid-column-make :heading "k2" :data ["b" "a"])
		      (datagrid-column-make :heading "w"  :data [20 10])))
	 (r (datagrid-left-join dg1 dg2 :on '("k1" . "k2"))))
    (should (equal (datagrid-column-data (aref r 2)) [10 20]))))

(ert-deftest datagrid-join-test-left-cols-explicit ()
  (let* ((r (datagrid-left-join (dg-join-test--dg1) (dg-join-test--dg2)
				:on 0 :cols '(2))))
    (should (= (length r) 3))
    (should (equal (datagrid-column-heading (aref r 2)) "score"))))

(ert-deftest datagrid-join-test-left-heading-collision ()
  (let* ((dg1 (vector (datagrid-column-make :heading "id"  :data ["a"])
		      (datagrid-column-make :heading "val" :data [1])))
	 (dg2 (vector (datagrid-column-make :heading "id"  :data ["a"])
		      (datagrid-column-make :heading "val" :data [99])))
	 (r (datagrid-left-join dg1 dg2 :on "id")))
    (should (equal (datagrid-column-heading (aref r 2)) "val_2"))))

(ert-deftest datagrid-join-test-left-suffix-custom ()
  (let* ((dg1 (vector (datagrid-column-make :heading "id"  :data ["a"])
		      (datagrid-column-make :heading "val" :data [1])))
	 (dg2 (vector (datagrid-column-make :heading "id"  :data ["a"])
		      (datagrid-column-make :heading "val" :data [99])))
	 (r (datagrid-left-join dg1 dg2 :on "id" :suffix ".y")))
    (should (equal (datagrid-column-heading (aref r 2)) "val.y"))))

(ert-deftest datagrid-join-test-left-empty-key-no-match ()
  (let* ((dg1 (vector (datagrid-column-make :heading "id" :data ["a" "" nil])))
	 (dg2 (vector (datagrid-column-make :heading "id" :data ["a" "" nil])
		      (datagrid-column-make :heading "v"  :data [1 2 3])))
	 (r (datagrid-left-join dg1 dg2 :on 0)))
    (should (equal (datagrid-column-data (aref r 1)) [1 nil nil]))))

(ert-deftest datagrid-join-test-left-first-duplicate-wins ()
  (let* ((dg1 (vector (datagrid-column-make :heading "id" :data ["a"])))
	 (dg2 (vector (datagrid-column-make :heading "id" :data ["a" "a"])
		      (datagrid-column-make :heading "v"  :data [1 2])))
	 (r (datagrid-left-join dg1 dg2 :on 0)))
    (should (equal (datagrid-column-data (aref r 1)) [1]))))


;;;; datagrid-inner-join

(ert-deftest datagrid-join-test-inner-drops-unmatched ()
  (let* ((r (datagrid-inner-join (dg-join-test--dg1) (dg-join-test--dg2)
				 :on 0)))
    (should (equal (datagrid-column-data (aref r 0)) ["a" "b"]))
    (should (equal (datagrid-column-data (aref r 1)) [1 2]))
    (should (equal (datagrid-column-data (aref r 2)) ["ay" "bee"]))
    (should (equal (datagrid-column-data (aref r 3)) [10 20]))))

(ert-deftest datagrid-join-test-inner-no-matches ()
  (let* ((dg1 (vector (datagrid-column-make :heading "id" :data ["x" "y"])
		      (datagrid-column-make :heading "v"  :data [1 2])))
	 (dg2 (vector (datagrid-column-make :heading "id" :data ["a"])
		      (datagrid-column-make :heading "w"  :data [9])))
	 (r (datagrid-inner-join dg1 dg2 :on 0)))
    (should (= (length (datagrid-column-data (aref r 0))) 0))))


;;;; datagrid-full-join

(ert-deftest datagrid-join-test-full-appends-dg2-only ()
  (let* ((r (datagrid-full-join (dg-join-test--dg1) (dg-join-test--dg2)
				:on 0)))
    (should (equal (datagrid-column-data (aref r 0)) ["a" "b" "c" "d"]))
    (should (equal (datagrid-column-data (aref r 1)) [1 2 3 nil]))
    (should (equal (datagrid-column-data (aref r 2)) ["ay" "bee" nil "dee"]))
    (should (equal (datagrid-column-data (aref r 3)) [10 20 nil 40]))))

(ert-deftest datagrid-join-test-full-no-extras-equals-left ()
  (let* ((dg1 (vector (datagrid-column-make :heading "id" :data ["a" "b"])
		      (datagrid-column-make :heading "v"  :data [1 2])))
	 (dg2 (vector (datagrid-column-make :heading "id" :data ["a" "b"])
		      (datagrid-column-make :heading "w"  :data [10 20])))
	 (left (datagrid-left-join dg1 dg2 :on 0))
	 (full (datagrid-full-join dg1 dg2 :on 0)))
    (should (equal (datagrid-column-data (aref full 0))
		   (datagrid-column-data (aref left 0))))
    (should (equal (datagrid-column-data (aref full 2))
		   (datagrid-column-data (aref left 2))))))

(ert-deftest datagrid-join-test-full-skips-empty-dg2-keys ()
  "Empty keys in DG2 must not become extra rows."
  (let* ((dg1 (vector (datagrid-column-make :heading "id" :data ["a"])
		      (datagrid-column-make :heading "v"  :data [1])))
	 (dg2 (vector (datagrid-column-make :heading "id" :data ["a" "" nil])
		      (datagrid-column-make :heading "w"  :data [10 20 30])))
	 (r (datagrid-full-join dg1 dg2 :on 0)))
    (should (= (length (datagrid-column-data (aref r 0))) 1))))


;;;; datagrid-anti-join

(ert-deftest datagrid-join-test-anti-keeps-unmatched ()
  (let* ((r (datagrid-anti-join (dg-join-test--dg1) (dg-join-test--dg2)
				:on 0)))
    (should (equal (datagrid-column-data (aref r 0)) ["c"]))
    (should (equal (datagrid-column-data (aref r 1)) [3]))))

(ert-deftest datagrid-join-test-anti-keeps-empty-keys ()
  (let* ((dg1 (vector (datagrid-column-make :heading "id" :data ["a" "" nil])
		      (datagrid-column-make :heading "v"  :data [1 2 3])))
	 (dg2 (vector (datagrid-column-make :heading "id" :data ["a"])))
	 (r (datagrid-anti-join dg1 dg2 :on 0)))
    (should (equal (datagrid-column-data (aref r 0)) ["" nil]))))


;;;; datagrid-semi-join

(ert-deftest datagrid-join-test-semi-keeps-matched ()
  (let* ((r (datagrid-semi-join (dg-join-test--dg1) (dg-join-test--dg2)
				:on 0)))
    (should (equal (datagrid-column-data (aref r 0)) ["a" "b"]))
    (should (equal (datagrid-column-data (aref r 1)) [1 2]))
    (should (= (length r) 2))))

(ert-deftest datagrid-join-test-semi-drops-empty-keys ()
  (let* ((dg1 (vector (datagrid-column-make :heading "id" :data ["a" "" nil])
		      (datagrid-column-make :heading "v"  :data [1 2 3])))
	 (dg2 (vector (datagrid-column-make :heading "id" :data ["a"])))
	 (r (datagrid-semi-join dg1 dg2 :on 0)))
    (should (equal (datagrid-column-data (aref r 0)) ["a"]))))


;;;; :on errors

(ert-deftest datagrid-join-test-on-missing-errors ()
  (should-error (datagrid-left-join (dg-join-test--dg1) (dg-join-test--dg2))))

(ert-deftest datagrid-join-test-on-bad-heading-errors ()
  (should-error
   (datagrid-left-join (dg-join-test--dg1) (dg-join-test--dg2)
		       :on "no-such-heading")))


;;;; Type-strict equality

(ert-deftest datagrid-join-test-type-strict-equal ()
  "Strings and numbers with same printed form do not match."
  (let* ((dg1 (vector (datagrid-column-make :heading "id" :data ["1" "2"])
		      (datagrid-column-make :heading "v"  :data [1 2])))
	 (dg2 (vector (datagrid-column-make :heading "id" :data [1 2])
		      (datagrid-column-make :heading "w"  :data [10 20])))
	 (r (datagrid-left-join dg1 dg2 :on '(0 . 0))))
    (should (equal (datagrid-column-data (aref r 2)) [nil nil]))))


;;;; Deprecated alias

(ert-deftest datagrid-join-test-deprecated-alias-works ()
  (let* ((dg1 (dg-join-test--dg1))
	 (dg2 (dg-join-test--dg2))
	 (r (with-no-warnings (datagrid-join dg1 0 dg2 0 1))))
    (should (= (length r) 3))
    (should (equal (datagrid-column-data (aref r 2)) ["ay" "bee" nil]))))


(provide 'datagrid-join-test)
;;; datagrid-join-test.el ends here
