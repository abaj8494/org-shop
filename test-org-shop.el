;;; test-org-shop.el --- Tests for org-shop -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for org-shop price calculation and rounding.
;; Run with: emacs -batch -l org -l org-shop.el -l test-org-shop.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org)
(require 'org-table)
(require 'org-shop)

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defmacro with-org-shop-temp-buffer (contents &rest body)
  "Create a temp org buffer with CONTENTS and execute BODY.
Point is placed at the beginning of the buffer."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

(defun test-org-shop--get-summary-cell (col-name)
  "Get the value of COL-NAME in the Summary row of the current table."
  (save-excursion
    (org-table-goto-line 1)
    (let ((result nil))
      (cl-loop for line from 1 to 100
               do (org-table-goto-line line)
               while (org-at-table-p)
               unless (org-at-table-hline-p)
               do (when (string= (string-trim (or (org-shop--get-cell "product") "")) "Summary")
                    (setq result (string-trim (or (org-shop--get-cell col-name) "")))))
      result)))

;;; ============================================================================
;;; org-shop--round-price tests
;;; ============================================================================

(ert-deftest test-round-price-basic ()
  "Round-price handles basic cases."
  (should (= (org-shop--round-price 1.0) 1.0))
  (should (= (org-shop--round-price 1.99) 1.99))
  (should (= (org-shop--round-price 0.0) 0.0)))

(ert-deftest test-round-price-rounds-down ()
  "Round-price rounds down at < 0.005."
  (should (= (org-shop--round-price 1.994) 1.99))
  (should (= (org-shop--round-price 2.3749) 2.37)))

(ert-deftest test-round-price-rounds-up ()
  "Round-price rounds up at >= 0.005."
  (should (= (org-shop--round-price 1.995) 2.0))
  ;; 1.005 is actually 1.00499... in IEEE 754, so it rounds down
  (should (= (org-shop--round-price 1.005) 1.0))
  (should (= (org-shop--round-price 1.006) 1.01))
  (should (= (org-shop--round-price 2.3750) 2.38)))

(ert-deftest test-round-price-negative ()
  "Round-price handles negative values."
  (should (= (org-shop--round-price -1.995) -2.0))
  (should (= (org-shop--round-price -0.994) -0.99)))

(ert-deftest test-round-price-large-values ()
  "Round-price handles larger dollar amounts."
  (should (= (org-shop--round-price 999.999) 1000.0))
  (should (= (org-shop--round-price 123.454) 123.45))
  (should (= (org-shop--round-price 123.455) 123.46)))

;;; ============================================================================
;;; org-shop--parse-price tests
;;; ============================================================================

(ert-deftest test-parse-price-normal ()
  "Parse-price handles normal numeric strings."
  (should (= (org-shop--parse-price "1.99") 1.99))
  (should (= (org-shop--parse-price "10") 10.0))
  (should (= (org-shop--parse-price "0.50") 0.5)))

(ert-deftest test-parse-price-empty ()
  "Parse-price returns 0.0 for empty/nil/dash."
  (should (= (org-shop--parse-price "") 0.0))
  (should (= (org-shop--parse-price nil) 0.0))
  (should (= (org-shop--parse-price "-") 0.0)))

(ert-deftest test-parse-price-non-numeric ()
  "Parse-price returns 0.0 for non-numeric strings."
  (should (= (org-shop--parse-price "abc") 0.0))
  (should (= (org-shop--parse-price "N/A") 0.0)))

;;; ============================================================================
;;; Per-item rounding in recalculate
;;; ============================================================================

(ert-deftest test-recalculate-per-item-rounding ()
  "Recalculate rounds each item before summing, not just the total.
This is the key test: 3 items each at $1.33 * 3 = $3.99 per item.
Per-item rounding: 3.99 + 3.99 + 3.99 = 11.97
End rounding:     1.33*3 + 1.33*3 + 1.33*3 = 11.97 (same here, but
1.333*3 = 3.999 -> 4.00 per item = 12.00 vs 11.997 -> 12.00 end)"
  (with-org-shop-temp-buffer
      "| product | done  | count | discount | notes | known_price | new_price |
|-----+------+-------+----------+-------+-------------+-----------|
| Apple   | [ ]   | 3     |          |       | 1.333       |           |
| Banana  | [ ]   | 3     |          |       | 1.333       |           |
| Orange  | [ ]   | 3     |          |       | 1.333       |           |
|-----+------+-------+----------+-------+-------------+-----------|
| Summary |       |       |          |       |             |           |
"
    (org-table-goto-line 1)
    (org-shop-recalculate)
    ;; Per-item: round(1.333 * 3) = round(3.999) = 4.00 each, total = 12.00
    (should (string= (test-org-shop--get-summary-cell "known_price") "12.00"))))

(ert-deftest test-recalculate-rounding-accumulation ()
  "Demonstrate that per-item rounding prevents floating-point drift.
5 items at $0.33 each, count 1.
Exact: 0.33 * 5 = 1.65 (no difference here, but tests the path)."
  (with-org-shop-temp-buffer
      "| product | done  | count | discount | notes | known_price | new_price |
|-----+------+-------+----------+-------+-------------+-----------|
| A       | [ ]   | 1     |          |       | 0.33        |           |
| B       | [ ]   | 1     |          |       | 0.33        |           |
| C       | [ ]   | 1     |          |       | 0.33        |           |
| D       | [ ]   | 1     |          |       | 0.33        |           |
| E       | [ ]   | 1     |          |       | 0.33        |           |
|-----+------+-------+----------+-------+-------------+-----------|
| Summary |       |       |          |       |             |           |
"
    (org-table-goto-line 1)
    (org-shop-recalculate)
    (should (string= (test-org-shop--get-summary-cell "known_price") "1.65"))))

(ert-deftest test-recalculate-fractional-count-rounding ()
  "Per-kg items with fractional counts are rounded per line.
1.259 kg at $5.99/kg = $7.54 (rounded from 7.54141)."
  (with-org-shop-temp-buffer
      "| product   | done  | count | discount | notes | known_price | new_price |
|-----+------+-------+----------+-------+-------------+-----------|
| Banana    | [ ]   | 1.259 |          |       | 5.99        |           |
|-----+------+-------+----------+-------+-------------+-----------|
| Summary   |       |       |          |       |             |           |
"
    (org-table-goto-line 1)
    (org-shop-recalculate)
    (should (string= (test-org-shop--get-summary-cell "known_price") "7.54"))))

(ert-deftest test-recalculate-multiple-fractional-counts ()
  "Multiple per-kg items: rounding per item prevents accumulation errors.
1.259 kg @ $5.99 = round(7.54141) = 7.54
0.743 kg @ $3.49 = round(2.59307) = 2.59
Total = 10.13"
  (with-org-shop-temp-buffer
      "| product   | done  | count | discount | notes | known_price | new_price |
|-----+------+-------+----------+-------+-------------+-----------|
| Banana    | [ ]   | 1.259 |          |       | 5.99        |           |
| Tomato    | [ ]   | 0.743 |          |       | 3.49        |           |
|-----+------+-------+----------+-------+-------------+-----------|
| Summary   |       |       |          |       |             |           |
"
    (org-table-goto-line 1)
    (org-shop-recalculate)
    (should (string= (test-org-shop--get-summary-cell "known_price") "10.13"))))

(ert-deftest test-recalculate-discount-per-item-rounding ()
  "Discount savings are rounded per item.
$4.99 at 30% off: cost = round(4.99 * 0.7) = round(3.493) = 3.49
                  saved = round(4.99 * 0.3) = round(1.497) = 1.50"
  (with-org-shop-temp-buffer
      "| product | done  | count | discount | notes | known_price | new_price |
|-----+------+-------+----------+-------+-------------+-----------|
| Steak   | [ ]   | 1     | 0.3      |       | 4.99        |           |
|-----+------+-------+----------+-------+-------------+-----------|
| Summary |       |       |          |       |             |           |
"
    (org-table-goto-line 1)
    (org-shop-recalculate)
    (should (string= (test-org-shop--get-summary-cell "known_price") "3.49"))
    (should (string= (test-org-shop--get-summary-cell "discount") "1.50"))))

(ert-deftest test-recalculate-discount-multiple-items ()
  "Multiple discounted items: each rounded individually.
Item A: $7.99 * 2 * 0.8 = round(12.784) = 12.78, saved = round(3.196) = 3.20
Item B: $3.49 * 1 * 0.85 = round(2.9665) = 2.97, saved = round(0.5235) = 0.52
Total paid = 15.75, total saved = 3.72"
  (with-org-shop-temp-buffer
      "| product | done  | count | discount | notes | known_price | new_price |
|-----+------+-------+----------+-------+-------------+-----------|
| Cereal  | [ ]   | 2     | 0.2      |       | 7.99        |           |
| Yogurt  | [ ]   | 1     | 0.15     |       | 3.49        |           |
|-----+------+-------+----------+-------+-------------+-----------|
| Summary |       |       |          |       |             |           |
"
    (org-table-goto-line 1)
    (org-shop-recalculate)
    (should (string= (test-org-shop--get-summary-cell "known_price") "15.75"))
    (should (string= (test-org-shop--get-summary-cell "discount") "3.72"))))

(ert-deftest test-recalculate-new-price-diff-rounding ()
  "Price diffs are rounded per item.
Known: 2.99, New: 3.49, count 3 -> diff = round(3 * 0.50) = 1.50
Known: 1.49, New: 1.29, count 2 -> diff = round(2 * -0.20) = -0.40
Total diff = +1.10"
  (with-org-shop-temp-buffer
      "| product | done  | count | discount | notes | known_price | new_price |
|-----+------+-------+----------+-------+-------------+-----------|
| Milk    | [ ]   | 3     |          |       | 2.99        | 3.49      |
| Bread   | [ ]   | 2     |          |       | 1.49        | 1.29      |
|-----+------+-------+----------+-------+-------------+-----------|
| Summary |       |       |          |       |             |           |
"
    (org-table-goto-line 1)
    (org-shop-recalculate)
    (should (string= (test-org-shop--get-summary-cell "new_price") "+1.10"))))

(ert-deftest test-recalculate-new-price-fractional-diff ()
  "Price diffs with fractional counts are rounded per item.
Known: 5.99, New: 6.49, count 1.259 -> diff = round(1.259 * 0.50) = round(0.6295) = 0.63"
  (with-org-shop-temp-buffer
      "| product | done  | count | discount | notes | known_price | new_price |
|-----+------+-------+----------+-------+-------------+-----------|
| Banana  | [ ]   | 1.259 |          |       | 5.99        | 6.49      |
|-----+------+-------+----------+-------+-------------+-----------|
| Summary |       |       |          |       |             |           |
"
    (org-table-goto-line 1)
    (org-shop-recalculate)
    (should (string= (test-org-shop--get-summary-cell "new_price") "+0.63"))))

;;; ============================================================================
;;; Edge cases for recalculate
;;; ============================================================================

(ert-deftest test-recalculate-empty-count-defaults-to-1 ()
  "Empty count field defaults to 1."
  (with-org-shop-temp-buffer
      "| product | done  | count | discount | notes | known_price | new_price |
|-----+------+-------+----------+-------+-------------+-----------|
| Milk    | [ ]   |       |          |       | 3.49        |           |
|-----+------+-------+----------+-------+-------------+-----------|
| Summary |       |       |          |       |             |           |
"
    (org-table-goto-line 1)
    (org-shop-recalculate)
    (should (string= (test-org-shop--get-summary-cell "known_price") "3.49"))))

(ert-deftest test-recalculate-marked-items-only ()
  "Only items with checkboxes are included in totals."
  (with-org-shop-temp-buffer
      "| product | done  | count | discount | notes | known_price | new_price |
|-----+------+-------+----------+-------+-------------+-----------|
| Milk    | [ ]   | 1     |          |       | 3.49        |           |
| Bread   |       | 1     |          |       | 2.99        |           |
|-----+------+-------+----------+-------+-------------+-----------|
| Summary |       |       |          |       |             |           |
"
    (org-table-goto-line 1)
    (org-shop-recalculate)
    ;; Bread has no checkbox so should be excluded
    (should (string= (test-org-shop--get-summary-cell "known_price") "3.49"))))

(ert-deftest test-recalculate-checked-and-unchecked ()
  "Both [X] and [ ] items are counted in totals."
  (with-org-shop-temp-buffer
      "| product | done  | count | discount | notes | known_price | new_price |
|-----+------+-------+----------+-------+-------------+-----------|
| Milk    | [X]   | 1     |          |       | 3.49        |           |
| Bread   | [ ]   | 1     |          |       | 2.99        |           |
|-----+------+-------+----------+-------+-------------+-----------|
| Summary |       |       |          |       |             |           |
"
    (org-table-goto-line 1)
    (org-shop-recalculate)
    (should (string= (test-org-shop--get-summary-cell "known_price") "6.48"))
    (should (string= (test-org-shop--get-summary-cell "done") "1U 1M"))))

(ert-deftest test-recalculate-zero-known-price ()
  "Items with zero known_price contribute nothing."
  (with-org-shop-temp-buffer
      "| product | done  | count | discount | notes | known_price | new_price |
|-----+------+-------+----------+-------+-------------+-----------|
| Free    | [ ]   | 1     |          |       | 0           |           |
| Milk    | [ ]   | 1     |          |       | 3.49        |           |
|-----+------+-------+----------+-------+-------------+-----------|
| Summary |       |       |          |       |             |           |
"
    (org-table-goto-line 1)
    (org-shop-recalculate)
    (should (string= (test-org-shop--get-summary-cell "known_price") "3.49"))))

(ert-deftest test-recalculate-new-price-overrides-known ()
  "When new_price is set, it is used as the effective price."
  (with-org-shop-temp-buffer
      "| product | done  | count | discount | notes | known_price | new_price |
|-----+------+-------+----------+-------+-------------+-----------|
| Milk    | [ ]   | 2     |          |       | 3.49        | 3.99      |
|-----+------+-------+----------+-------+-------------+-----------|
| Summary |       |       |          |       |             |           |
"
    (org-table-goto-line 1)
    (org-shop-recalculate)
    ;; Total paid uses new_price: 2 * 3.99 = 7.98
    (should (string= (test-org-shop--get-summary-cell "known_price") "7.98"))))

(ert-deftest test-recalculate-formats-price-cells ()
  "Recalculate formats price cells to 2 decimal places."
  (with-org-shop-temp-buffer
      "| product | done  | count | discount | notes | known_price | new_price |
|-----+------+-------+----------+-------+-------------+-----------|
| Milk    | [ ]   | 1     |          |       | 3.5         |           |
|-----+------+-------+----------+-------+-------------+-----------|
| Summary |       |       |          |       |             |           |
"
    (org-table-goto-line 1)
    (org-shop-recalculate)
    ;; After recalculate, the summary known_price should show 3.50
    ;; which confirms the data cell was parsed and formatted correctly
    (should (string= (test-org-shop--get-summary-cell "known_price") "3.50"))
    ;; Also verify via buffer text that the data row cell was formatted
    (goto-char (point-min))
    (should (search-forward "3.50" nil t))))

;;; ============================================================================
;;; Realistic receipt scenario tests
;;; ============================================================================

(ert-deftest test-recalculate-realistic-grocery-receipt ()
  "Simulate a realistic grocery receipt with mixed items.
Per-item rounding should match what a register prints."
  (with-org-shop-temp-buffer
      "| product     | done  | count | discount | notes | known_price | new_price |
|-----+------+-------+----------+-------+-------------+-----------|
| Banana      | [ ]   | 1.247 |          |       | 3.99        |           |
| Chicken     | [ ]   | 1.834 |          |       | 11.99       |           |
| Yogurt      | [ ]   | 3     | 0.33     |       | 2.49        |           |
| Rice        | [ ]   | 1     |          |       | 4.50        |           |
| Avocado     | [ ]   | 4     |          |       | 2.99        | 3.29      |
|-----+------+-------+----------+-------+-------------+-----------|
| Summary     |       |       |          |       |             |           |
"
    (org-table-goto-line 1)
    (org-shop-recalculate)
    ;; Banana:  round(1.247 * 3.99) = round(4.97553) = 4.98
    ;; Chicken: round(1.834 * 11.99) = round(21.9897) = 21.99
    ;; Yogurt:  round(3 * 2.49 * 0.67) = round(5.0049) = 5.00
    ;; Rice:    round(1 * 4.50) = 4.50
    ;; Avocado: round(4 * 3.29) = 13.16  (uses new_price)
    ;; Total paid = 4.98 + 21.99 + 5.00 + 4.50 + 13.16 = 49.63
    (should (string= (test-org-shop--get-summary-cell "known_price") "49.63"))
    ;; Yogurt saved: round(3 * 2.49 * 0.33) = round(2.4651) = 2.47
    (should (string= (test-org-shop--get-summary-cell "discount") "2.47"))))

(ert-deftest test-recalculate-many-small-items ()
  "Many small items where per-item rounding matters.
10 items at $0.99 each, count 3 = round(2.97) = 2.97 each.
Total = 29.70 (same either way here, but validates the path)."
  (with-org-shop-temp-buffer
      "| product | done  | count | discount | notes | known_price | new_price |
|-----+------+-------+----------+-------+-------------+-----------|
| A       | [ ]   | 3     |          |       | 0.99        |           |
| B       | [ ]   | 3     |          |       | 0.99        |           |
| C       | [ ]   | 3     |          |       | 0.99        |           |
| D       | [ ]   | 3     |          |       | 0.99        |           |
| E       | [ ]   | 3     |          |       | 0.99        |           |
| F       | [ ]   | 3     |          |       | 0.99        |           |
| G       | [ ]   | 3     |          |       | 0.99        |           |
| H       | [ ]   | 3     |          |       | 0.99        |           |
| I       | [ ]   | 3     |          |       | 0.99        |           |
| J       | [ ]   | 3     |          |       | 0.99        |           |
|-----+------+-------+----------+-------+-------------+-----------|
| Summary |       |       |          |       |             |           |
"
    (org-table-goto-line 1)
    (org-shop-recalculate)
    (should (string= (test-org-shop--get-summary-cell "known_price") "29.70"))))

(ert-deftest test-recalculate-rounding-boundary-case ()
  "Items specifically chosen to trigger rounding boundary.
$1.15 * 3 = 3.45 (exact)
$1.15 * 7 = 8.05 (exact)
$2.35 * 3 = 7.05 (exact)
But: $1.33 * 3 = 3.99 (3.990 rounds to 3.99)
     $1.67 * 3 = 5.01 (5.010 rounds to 5.01)
Total = 3.45 + 8.05 + 7.05 + 3.99 + 5.01 = 27.55"
  (with-org-shop-temp-buffer
      "| product | done  | count | discount | notes | known_price | new_price |
|-----+------+-------+----------+-------+-------------+-----------|
| A       | [ ]   | 3     |          |       | 1.15        |           |
| B       | [ ]   | 7     |          |       | 1.15        |           |
| C       | [ ]   | 3     |          |       | 2.35        |           |
| D       | [ ]   | 3     |          |       | 1.33        |           |
| E       | [ ]   | 3     |          |       | 1.67        |           |
|-----+------+-------+----------+-------+-------------+-----------|
| Summary |       |       |          |       |             |           |
"
    (org-table-goto-line 1)
    (org-shop-recalculate)
    (should (string= (test-org-shop--get-summary-cell "known_price") "27.55"))))

(ert-deftest test-recalculate-discount-rounding-boundary ()
  "Discount rounding at half-cent boundary.
$9.99 at 15% off:
  cost  = round(9.99 * 0.85) = round(8.4915) = 8.49
  saved = round(9.99 * 0.15) = round(1.4985) = 1.50"
  (with-org-shop-temp-buffer
      "| product | done  | count | discount | notes | known_price | new_price |
|-----+------+-------+----------+-------+-------------+-----------|
| Wine    | [ ]   | 1     | 0.15     |       | 9.99        |           |
|-----+------+-------+----------+-------+-------------+-----------|
| Summary |       |       |          |       |             |           |
"
    (org-table-goto-line 1)
    (org-shop-recalculate)
    (should (string= (test-org-shop--get-summary-cell "known_price") "8.49"))
    (should (string= (test-org-shop--get-summary-cell "discount") "1.50"))))

;;; ============================================================================
;;; Count summary tests
;;; ============================================================================

(ert-deftest test-recalculate-count-total ()
  "Count total sums all item counts."
  (with-org-shop-temp-buffer
      "| product | done  | count | discount | notes | known_price | new_price |
|-----+------+-------+----------+-------+-------------+-----------|
| Milk    | [ ]   | 2     |          |       | 3.49        |           |
| Bread   | [ ]   | 3     |          |       | 2.99        |           |
|-----+------+-------+----------+-------+-------------+-----------|
| Summary |       |       |          |       |             |           |
"
    (org-table-goto-line 1)
    (org-shop-recalculate)
    (should (string= (test-org-shop--get-summary-cell "count") "5"))))

;;; ============================================================================
;;; History recalculation tests
;;; ============================================================================

(ert-deftest test-history-recalculate-per-item-rounding ()
  "History total rounds per item before summing.
3 * 1.333 = round(3.999) = 4.00
2 * 2.777 = round(5.554) = 5.55
Total = 9.55"
  (with-org-shop-temp-buffer
      "* Purchase History :noexport:
| product | date       | count | discount | price |
|---------+------------+-------+----------+-------|
| Apple   | 2026-01-15 | 3     |          | 1.333 |
| Banana  | 2026-01-15 | 2     |          | 2.777 |
|---------+------------+-------+----------+-------|
| TOTAL   |            |       |          |       |
|---------+------------+-------+----------+-------|
"
    (let ((org-shop-history-heading "Purchase History"))
      ;; Navigate into the table
      (re-search-forward "^\\s-*|" nil t)
      (beginning-of-line)
      (forward-line 2)  ; skip header + hline to data row
      (org-shop--recalculate-history-total)
      ;; Check TOTAL row
      (org-table-goto-line 1)
      (let ((total-price nil))
        (cl-loop for line from 1 to 20
                 do (org-table-goto-line line)
                 while (org-at-table-p)
                 unless (org-at-table-hline-p)
                 do (when (string= (string-trim (or (org-table-get nil 1) "")) "TOTAL")
                      (setq total-price (string-trim (or (org-table-get nil 5) "")))))
        (should (string= total-price "9.55"))))))

;;; ============================================================================
;;; Separator-flexible matching tests
;;; ============================================================================

(ert-deftest test-flexible-regexp-separators ()
  "-, _ and space are interchangeable when matching shop names."
  (should (string-match-p (org-shop--flexible-regexp "dan_murphy") "dan murphy's"))
  (should (string-match-p (org-shop--flexible-regexp "dan_murphy") "dan-murphy"))
  (should (string-match-p (org-shop--flexible-regexp "dan_murphy") "dan_murphy"))
  (should (string-match-p (org-shop--flexible-regexp "no-gap-dental") "no gap dental"))
  ;; A single-word shop name is unaffected.
  (should (string-match-p (org-shop--flexible-regexp "aldi") "aldi run"))
  (should-not (string-match-p (org-shop--flexible-regexp "aldi") "coles")))

(ert-deftest test-shop-from-heading-separator-match ()
  "A `dan_murphy.org' file matches a `Dan Murphy's Run' heading."
  (let ((dir (make-temp-file "org-shop-shops" t)))
    (unwind-protect
        (progn
          (write-region "#+TITLE: dan murphy's\n" nil
                        (expand-file-name "dan_murphy.org" dir))
          (write-region "#+TITLE: aldi\n" nil
                        (expand-file-name "aldi.org" dir))
          (let ((org-shop-directory dir))
            (with-org-shop-temp-buffer "* Dan Murphy's Run\n"
              (should (string= (org-shop--get-shop-from-heading) "dan_murphy")))
            (with-org-shop-temp-buffer "* Aldi Trip\n"
              (should (string= (org-shop--get-shop-from-heading) "aldi")))))
      (delete-directory dir t))))

;;; ============================================================================
;;; next shop checklist tests
;;; ============================================================================

(defun test-org-shop--with-temp-shop-file (contents fn)
  "Write CONTENTS to a temp .org file and call FN with its path."
  (let ((file (make-temp-file "org-shop-next" nil ".org" contents)))
    (unwind-protect
        (funcall fn file)
      (when (get-file-buffer file)
        (with-current-buffer (get-file-buffer file)
          (set-buffer-modified-p nil))
        (kill-buffer (get-file-buffer file)))
      (delete-file file))))

(ert-deftest test-next-shop-items-preserve-nesting ()
  "Nested sub-items and non-blank checkbox states are copied verbatim."
  (test-org-shop--with-temp-shop-file
   "#+TITLE: bunnings

* next shop

- [ ] eyelets
- [ ] extension chords
  - outside, shortish, 2m
  - inside, 1m, 2m, 3m
- [-] power board 5 plugs
  - [ ] splitters maybe 3 to 5
  - [X] permanent markers.

* inventory
| next | product |
"
   (lambda (file)
     (should
      (equal (org-shop--get-next-shop-items file)
             '("- [ ] eyelets"
               "- [ ] extension chords"
               "  - outside, shortish, 2m"
               "  - inside, 1m, 2m, 3m"
               "- [-] power board 5 plugs"
               "  - [ ] splitters maybe 3 to 5"
               "  - [X] permanent markers."))))))

(ert-deftest test-next-shop-heading-hyphen-tolerant ()
  "A `* next-shop' heading is matched by the default `next shop' config."
  (test-org-shop--with-temp-shop-file
   "#+TITLE: dan murphy's

* next-shop

- [ ] jägermeister

* inventory
| next | product |
"
   (lambda (file)
     (should (equal (org-shop--get-next-shop-items file)
                    '("- [ ] jägermeister"))))))

(ert-deftest test-next-shop-items-empty ()
  "An empty next shop heading yields nil."
  (test-org-shop--with-temp-shop-file
   "#+TITLE: bunnings

* next shop

* inventory
| next | product |
"
   (lambda (file)
     (should (null (org-shop--get-next-shop-items file))))))

(ert-deftest test-insert-next-shop-items-verbatim ()
  "Items are inserted verbatim, preserving nesting and checkbox state."
  (with-temp-buffer
    (org-shop--insert-next-shop-items
     '("- [ ] eyelets"
       "  - outside, shortish, 2m"
       "- [X] permanent markers."))
    (should (string= (buffer-string)
                     "\n- [ ] eyelets\n  - outside, shortish, 2m\n- [X] permanent markers.\n"))))

;;; ============================================================================
;;; Table row enumeration (org-shop--table-data-lines)
;;; ============================================================================

(ert-deftest test-table-data-lines-enumerates-data-rows ()
  "Header and hlines are excluded; every data row (incl. Summary) is listed once."
  (with-org-shop-temp-buffer
      "| product | done |
|---------+------|
| A       | [ ]  |
| B       | [ ]  |
|---------+------|
| Summary |      |
"
    (org-table-goto-line 1)
    (let ((lines (org-shop--table-data-lines)))
      ;; A, B and Summary -> three rows
      (should (= (length lines) 3))
      ;; each line number is reported exactly once
      (should (equal lines (delete-dups (copy-sequence lines)))))))

(ert-deftest test-table-data-lines-single-row ()
  "A table with a single data row enumerates exactly that row."
  (with-org-shop-temp-buffer
      "| product | done |
|---------+------|
| Only    | [ ]  |
"
    (org-table-goto-line 1)
    (should (= (length (org-shop--table-data-lines)) 1))))

;;; ============================================================================
;;; Regression: a final table row without a trailing newline must not hang
;;; ============================================================================
;; A shop table rendered as the last line of a buffer (e.g. inside a notmuch
;; message) has no newline after its final row.  Enumeration used to spin
;; forever there: `forward-line' could not advance past the row, yet
;; `org-at-table-p' stayed non-nil.  These tests assert termination *and*
;; correctness -- a regression of the guard never returns, surfacing as a hung
;; (failed) suite rather than a wrong assertion.

(ert-deftest test-table-data-lines-eob-no-trailing-newline ()
  "Enumeration terminates when the last row has no trailing newline."
  (with-temp-buffer
    (org-mode)
    (insert "| product | done | known_price |\n")
    (insert "|---------+------+-------------|\n")
    (insert "| Milk    | [X]  | 2.50        |\n")
    (insert "|---------+------+-------------|\n")
    (insert "| Summary |      |             |") ; no trailing newline
    (org-table-goto-line 1)
    ;; Milk + Summary
    (should (= (length (org-shop--table-data-lines)) 2))))

(ert-deftest test-recalculate-eob-no-trailing-newline ()
  "`org-shop-recalculate' completes and totals correctly on such a table."
  (with-temp-buffer
    (org-mode)
    (insert "| product | done | count | discount | known_price | new_price |\n")
    (insert "|---------+------+-------+----------+-------------+-----------|\n")
    (insert "| Milk    | [X]  | 1     |          | -           | 2.50      |\n")
    (insert "| Eggs    | [X]  | 1     |          | -           | 4.00      |\n")
    (insert "|---------+------+-------+----------+-------------+-----------|\n")
    (insert "| Summary |      |       |          |             |           |") ; no newline
    (org-table-goto-line 1)
    (org-shop-recalculate)
    (should (string= (test-org-shop--get-summary-cell "known_price") "6.50"))
    (should (string= (test-org-shop--get-summary-cell "done") "0U 2M"))))

;;; test-org-shop.el ends here
