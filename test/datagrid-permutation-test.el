;;; datagrid-permutation-test.el --- Tests for row-order/col-order composition -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(add-to-list 'load-path
             (file-name-directory (or load-file-name buffer-file-name)))
(require 'datagrid)

(defun datagrid-perm-test-fixture ()
  "Three-column, four-row datagrid for composition tests."
  (datagrid-make
   :columns
   (vector (datagrid-column-make :heading "name"
                                 :data ["Alice" "Bob" "Carol" "Dave"])
           (datagrid-column-make :heading "score"
                                 :data [30 10 40 20])
           (datagrid-column-make :heading "group"
                                 :data ["x" "y" "x" "y"]))))


;;;; Fresh-grid invariants

(ert-deftest datagrid-perm-test-fresh-orders-nil ()
  (let ((dg (datagrid-perm-test-fixture)))
    (should-not (datagrid-row-order dg))
    (should-not (datagrid-col-order dg))))

(ert-deftest datagrid-perm-test-fresh-pull-is-shared ()
  (let* ((dg (datagrid-perm-test-fixture))
         (raw (datagrid-column-data
               (aref (datagrid-columns dg) 1))))
    (should (eq raw (datagrid-pull dg 1)))))


;;;; Sort

(ert-deftest datagrid-perm-test-sort-sets-row-order ()
  (let* ((dg (datagrid-perm-test-fixture))
         (sorted (datagrid-sort dg 1)))
    (should (datagrid-row-order sorted))
    (should-not (datagrid-col-order sorted))
    (should (equal (datagrid-pull sorted 1) [10 20 30 40]))
    (should (equal (datagrid-pull sorted 0) ["Bob" "Dave" "Alice" "Carol"]))))

(ert-deftest datagrid-perm-test-sort-shares-columns ()
  (let* ((dg (datagrid-perm-test-fixture))
         (sorted (datagrid-sort dg 1)))
    (should (eq (datagrid-columns dg) (datagrid-columns sorted)))))

(ert-deftest datagrid-perm-test-sort-does-not-mutate-source-data ()
  (let* ((dg (datagrid-perm-test-fixture))
         (orig (copy-sequence
                (datagrid-column-data (aref (datagrid-columns dg) 1)))))
    (datagrid-sort dg 1)
    (should (equal orig
                   (datagrid-column-data (aref (datagrid-columns dg) 1))))))


;;;; Filter

(ert-deftest datagrid-perm-test-filter-sets-row-order ()
  (let* ((dg (datagrid-perm-test-fixture))
         (mask [t nil t nil])
         (filtered (datagrid-filter-by-mask dg mask)))
    (should (datagrid-row-order filtered))
    (should (equal (datagrid-pull filtered 0) ["Alice" "Carol"]))
    (should (equal (datagrid-pull filtered 1) [30 40]))))

(ert-deftest datagrid-perm-test-filter-shares-columns ()
  (let* ((dg (datagrid-perm-test-fixture))
         (filtered (datagrid-filter-by-mask dg [t t nil nil])))
    (should (eq (datagrid-columns dg) (datagrid-columns filtered)))))


;;;; Slice

(ert-deftest datagrid-perm-test-slice-sets-row-order ()
  (let* ((dg (datagrid-perm-test-fixture))
         (sliced (datagrid-slice dg 2 0)))
    (should (datagrid-row-order sliced))
    (should (equal (datagrid-pull sliced 0) ["Carol" "Alice"]))
    (should (equal (datagrid-pull sliced 1) [40 30]))))


;;;; Select

(ert-deftest datagrid-perm-test-select-sets-col-order ()
  (let* ((dg (datagrid-perm-test-fixture))
         (selected (datagrid-select dg "score" "name")))
    (should (datagrid-col-order selected))
    (should-not (datagrid-row-order selected))
    (should (equal (datagrid-get-headings selected) ["score" "name"]))
    (should (equal (datagrid-pull selected 0) [30 10 40 20]))
    (should (equal (datagrid-pull selected 1) ["Alice" "Bob" "Carol" "Dave"]))))

(ert-deftest datagrid-perm-test-select-shares-columns ()
  (let* ((dg (datagrid-perm-test-fixture))
         (selected (datagrid-select dg 1 0)))
    (should (eq (datagrid-columns dg) (datagrid-columns selected)))))


;;;; Compositions

(ert-deftest datagrid-perm-test-sort-then-filter ()
  ;; Sort ascending by score, then keep first two logical rows via mask.
  (let* ((dg (datagrid-perm-test-fixture))
         (sorted (datagrid-sort dg 1))
         (filtered (datagrid-filter-by-mask sorted [t t nil nil])))
    (should (equal (datagrid-pull filtered 1) [10 20]))
    (should (equal (datagrid-pull filtered 0) ["Bob" "Dave"]))
    (should (eq (datagrid-columns dg) (datagrid-columns filtered)))))

(ert-deftest datagrid-perm-test-filter-then-sort ()
  ;; Filter out rows with group "y", then sort by score.
  (let* ((dg (datagrid-perm-test-fixture))
         (filtered (datagrid-filter-by-mask dg [t nil t nil]))
         (sorted (datagrid-sort filtered 1)))
    (should (equal (datagrid-pull sorted 1) [30 40]))
    (should (equal (datagrid-pull sorted 0) ["Alice" "Carol"]))))

(ert-deftest datagrid-perm-test-slice-then-sort ()
  ;; Take rows 2, 0, 3 (logical [Carol/40, Alice/30, Dave/20]), then sort.
  (let* ((dg (datagrid-perm-test-fixture))
         (sliced (datagrid-slice dg 2 0 3))
         (sorted (datagrid-sort sliced 1)))
    (should (equal (datagrid-pull sorted 1) [20 30 40]))
    (should (equal (datagrid-pull sorted 0) ["Dave" "Alice" "Carol"]))))

(ert-deftest datagrid-perm-test-select-then-pull-by-name ()
  ;; After reordering columns, pulling by heading must find the right data.
  (let* ((dg (datagrid-perm-test-fixture))
         (selected (datagrid-select dg "group" "name")))
    (should (equal (datagrid-pull selected "name")
                   ["Alice" "Bob" "Carol" "Dave"]))
    (should (equal (datagrid-pull selected "group") ["x" "y" "x" "y"]))))

(ert-deftest datagrid-perm-test-select-then-sort ()
  ;; Select reorders columns, and sort still works on the logical index.
  (let* ((dg (datagrid-perm-test-fixture))
         (selected (datagrid-select dg "score" "name"))
         ;; Logical column 0 is now "score".
         (sorted (datagrid-sort selected 0)))
    (should (equal (datagrid-pull sorted 0) [10 20 30 40]))
    (should (equal (datagrid-pull sorted 1) ["Bob" "Dave" "Alice" "Carol"]))))

(ert-deftest datagrid-perm-test-select-twice-composes ()
  (let* ((dg (datagrid-perm-test-fixture))
         (a (datagrid-select dg "score" "name" "group"))
         (b (datagrid-select a "name")))
    (should (equal (datagrid-get-headings b) ["name"]))
    (should (equal (datagrid-pull b 0)
                   ["Alice" "Bob" "Carol" "Dave"]))))

(ert-deftest datagrid-perm-test-sort-twice-composes ()
  ;; Sort ascending twice should be idempotent on the logical ordering.
  (let* ((dg (datagrid-perm-test-fixture))
         (once (datagrid-sort dg 1))
         (twice (datagrid-sort once 1)))
    (should (equal (datagrid-pull once 1) (datagrid-pull twice 1)))
    (should (equal (datagrid-pull once 0) (datagrid-pull twice 0)))))


;;;; Rename respects col-order

(ert-deftest datagrid-perm-test-rename-after-select ()
  (let* ((dg (datagrid-perm-test-fixture))
         (selected (datagrid-select dg "score" "name"))
         (renamed (datagrid-rename selected 0 "points")))
    (should (equal (datagrid-get-headings renamed) ["points" "name"]))
    ;; Original heading at physical column 1 (score) is what got renamed,
    ;; and the source datagrid's heading must be untouched.
    (should (equal (datagrid-column-heading
                    (aref (datagrid-columns dg) 1))
                   "score"))))

(provide 'datagrid-permutation-test)
;;; datagrid-permutation-test.el ends here
