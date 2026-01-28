;;; org-shop.el --- Org-table based shopping list management -*- lexical-binding: t; -*-

;; Author: Aayush Bajaj
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: org, shopping, inventory, prices
;; URL: https://github.com/abaj8494/org-shop

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; org-shop provides a workflow for managing shopping lists using org-tables.
;;
;; Features:
;; - Mark items in shop files for your next shopping trip
;; - Generate shopping lists in daily/destination files
;; - Track price changes and maintain price history
;; - Sync prices back to source shop files
;;
;; Usage:
;;   (require 'org-shop)
;;   (org-shop-setup)
;;
;; Keybindings (default C-c S prefix, customizable via org-shop-keymap-prefix):
;;   C-c S m - Toggle mark (next in shop files, done in daily files)
;;   C-c S g - Generate shopping list from marked items
;;   C-c S s - Sync prices back to shop file
;;   C-c S c - Clear all marks in current shop file

;;; Code:

(require 'org)
(require 'org-table)
(require 'cl-lib)

;;; ============================================================================
;;; Customization
;;; ============================================================================

(defgroup org-shop nil
  "Org-table based shopping list management."
  :group 'org
  :prefix "org-shop-")

(defcustom org-shop-directory "~/org/shops/"
  "Directory containing shop files."
  :type 'directory
  :group 'org-shop)

(defcustom org-shop-file-suffix ".org"
  "Suffix for shop files."
  :type 'string
  :group 'org-shop)

(defcustom org-shop-source-heading "regular"
  "Heading name containing the product table in shop files."
  :type 'string
  :group 'org-shop)

(defcustom org-shop-history-heading "history"
  "Heading name for purchase history table."
  :type 'string
  :group 'org-shop)

(defcustom org-shop-detect-shop-from-heading t
  "If non-nil, auto-detect shop from heading (e.g., `Aldi Run' -> aldi).
If nil, prompt user to select shop."
  :type 'boolean
  :group 'org-shop)

(defcustom org-shop-clear-marks-after-generate t
  "If non-nil, clear [X] marks in shop file after generating list."
  :type 'boolean
  :group 'org-shop)

(defcustom org-shop-update-last-bought t
  "If non-nil, update last_bought column when generating list."
  :type 'boolean
  :group 'org-shop)

(defcustom org-shop-setup-keymaps t
  "Whether to setup default keymaps on load."
  :type 'boolean
  :group 'org-shop)

(defcustom org-shop-keymap-prefix "C-c S"
  "Prefix key for org-shop commands.
Change this if the default conflicts with other bindings."
  :type 'string
  :group 'org-shop)

;;; ============================================================================
;;; Column Configuration
;;; ============================================================================

(defcustom org-shop-source-columns '("next" "product" "price" "quantity" "last_bought")
  "Column names for shop source tables (in order)."
  :type '(repeat string)
  :group 'org-shop)

(defcustom org-shop-dest-columns '("product" "done" "count" "discount" "notes" "known_price" "new_price")
  "Column names for generated shopping list tables (in order).
Column order: product first, then done, for better readability."
  :type '(repeat string)
  :group 'org-shop)

;;; ============================================================================
;;; Internal State
;;; ============================================================================

(defvar org-shop--mark-char "X"
  "Character used to mark items (inside brackets).")

;;; ============================================================================
;;; Utility Functions - Shop File Discovery
;;; ============================================================================

(defun org-shop--list-shops ()
  "Return list of available shop names from `org-shop-directory'."
  (when (file-directory-p org-shop-directory)
    (let ((files (directory-files org-shop-directory nil
                                  (concat ".*" (regexp-quote org-shop-file-suffix) "$"))))
      (mapcar (lambda (f)
                (string-remove-suffix org-shop-file-suffix f))
              files))))

(defun org-shop--find-shop-file (shop-name)
  "Return full path to shop file for SHOP-NAME."
  (expand-file-name (concat shop-name org-shop-file-suffix)
                    org-shop-directory))

(defun org-shop--prompt-shop ()
  "Prompt user to select a shop from available shops."
  (let ((shops (org-shop--list-shops)))
    (if shops
        (completing-read "Select shop: " shops nil t)
      (user-error "No shop files found in %s" org-shop-directory))))

(defun org-shop--get-shop-from-heading ()
  "Extract shop name from current heading.
E.g., `** TODO Aldi Run' -> \"aldi\", `** Woolworths Trip' -> \"woolworths\"."
  (save-excursion
    (org-back-to-heading t)
    (let ((heading (org-get-heading t t t t)))
      (when heading
        ;; Remove common suffixes and normalize
        (let* ((normalized (downcase heading))
               (normalized (replace-regexp-in-string
                            "\\s-*\\(run\\|trip\\|shop\\|shopping\\|list\\)\\s-*$"
                            "" normalized))
               (normalized (string-trim normalized)))
          ;; Check if this matches a known shop
          (let ((shops (org-shop--list-shops)))
            (cl-find-if (lambda (shop)
                          (string-match-p (regexp-quote shop) normalized))
                        shops)))))))

(defun org-shop--resolve-shop ()
  "Determine which shop to use based on settings and context."
  (if org-shop-detect-shop-from-heading
      (or (org-shop--get-shop-from-heading)
          (org-shop--prompt-shop))
    (org-shop--prompt-shop)))

;;; ============================================================================
;;; Utility Functions - Table Operations
;;; ============================================================================

(defun org-shop--at-table-p ()
  "Return non-nil if point is inside an org table."
  (org-at-table-p))

(defun org-shop--get-row-fields ()
  "Get fields from current table row as list of strings."
  (when (org-at-table-p)
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position))))
      ;; Split by | and remove first/last empty elements
      (let ((fields (split-string line "|")))
        (when (> (length fields) 2)
          (mapcar #'string-trim (butlast (cdr fields))))))))

(defun org-shop--get-table-columns ()
  "Get column names from table header row.
Returns list of column names."
  (save-excursion
    (org-table-goto-line 1)
    (org-shop--get-row-fields)))

(defun org-shop--column-index (column-name)
  "Return 0-based index of COLUMN-NAME in current table, or nil."
  (let ((columns (org-shop--get-table-columns)))
    (cl-position column-name columns :test #'string-equal-ignore-case)))

(defun org-shop--get-cell (column-name)
  "Get value of cell in COLUMN-NAME for current row."
  (let ((col-idx (org-shop--column-index column-name)))
    (when col-idx
      (string-trim (or (org-table-get nil (1+ col-idx)) "")))))

(defun org-shop--set-cell (column-name value)
  "Set cell in COLUMN-NAME to VALUE for current row."
  (let ((col-idx (org-shop--column-index column-name)))
    (when col-idx
      (org-table-put nil (1+ col-idx) value)
      (org-table-align))))

(defun org-shop--table-data-lines ()
  "Return list of line numbers containing table data (excluding header/hline)."
  (save-excursion
    (let ((lines '())
          (line 2))  ; Start after header
      (org-table-goto-line line)
      (while (org-at-table-p)
        (unless (org-at-table-hline-p)
          (push line lines))
        (setq line (1+ line))
        (forward-line 1))
      (nreverse lines))))

(defun org-shop--parse-table ()
  "Parse current org table into list of alists.
Each alist represents a row with (column-name . value) pairs."
  (let ((columns (org-shop--get-table-columns))
        (rows '()))
    (dolist (line-num (org-shop--table-data-lines))
      (org-table-goto-line line-num)
      (let ((fields (org-shop--get-row-fields))
            (row '()))
        (cl-loop for col in columns
                 for val in fields
                 do (push (cons col val) row))
        (push (nreverse row) rows)))
    (nreverse rows)))

;;; ============================================================================
;;; Utility Functions - Navigation
;;; ============================================================================

(defun org-shop--goto-heading (heading-name)
  "Go to heading with HEADING-NAME in current buffer.
Returns point if found, nil otherwise."
  (goto-char (point-min))
  (let ((case-fold-search t))
    (when (re-search-forward
           (concat "^\\*+\\s-+" (regexp-quote heading-name) "\\(\\s-\\|:\\)")
           nil t)
      (beginning-of-line)
      (point))))

(defun org-shop--goto-table-after-heading (heading-name)
  "Navigate to first table under HEADING-NAME.
Returns point at start of table, or nil if not found."
  (when (org-shop--goto-heading heading-name)
    (org-narrow-to-subtree)
    (unwind-protect
        (when (re-search-forward "^\\s-*|" nil t)
          (beginning-of-line)
          (point))
      (widen))))

(defun org-shop--in-shop-file-p ()
  "Return non-nil if current buffer is a shop file."
  (and buffer-file-name
       (string-prefix-p (expand-file-name org-shop-directory)
                        (expand-file-name buffer-file-name))))

;;; ============================================================================
;;; Core Functions - Marking
;;; ============================================================================

(defun org-shop--has-column-p (column-name)
  "Return non-nil if current table has COLUMN-NAME."
  (let ((columns (org-shop--get-table-columns)))
    (cl-find column-name columns :test #'string-equal-ignore-case)))

(defun org-shop--is-marked-p (&optional column)
  "Return non-nil if current row is marked in COLUMN (default \"next\")."
  (let ((val (org-shop--get-cell (or column "next"))))
    (and val
         (string-match-p (regexp-quote org-shop--mark-char) val))))

(defun org-shop--mark-row (&optional column)
  "Mark current row (set COLUMN to [X], default \"next\")."
  (org-shop--set-cell (or column "next") (format "[%s]" org-shop--mark-char)))

(defun org-shop--unmark-row (&optional column)
  "Unmark current row (set COLUMN to [ ], default \"next\")."
  (org-shop--set-cell (or column "next") "[ ]"))

(defun org-shop--toggle-mark (&optional column)
  "Toggle mark on current row in COLUMN (default \"next\")."
  (if (org-shop--is-marked-p column)
      (org-shop--unmark-row column)
    (org-shop--mark-row column)))

;;;###autoload
(defun org-shop-mark ()
  "Toggle mark on current table row or rows in region.
In shop files (with \"next\" column), marks items for shopping.
In daily files (with \"done\" column), marks items as done."
  (interactive)
  (unless (org-shop--at-table-p)
    (user-error "Not in an org table"))
  ;; Determine which column to toggle based on table structure
  (let ((column (cond
                 ((org-shop--has-column-p "next") "next")
                 ((org-shop--has-column-p "done") "done")
                 (t (user-error "Table has no 'next' or 'done' column")))))
    (if (use-region-p)
        ;; Handle region
        (let ((beg (region-beginning))
              (end (region-end)))
          (save-excursion
            (goto-char beg)
            (while (< (point) end)
              (when (and (org-at-table-p)
                         (not (org-at-table-hline-p)))
                (org-shop--toggle-mark column))
              (forward-line 1))))
      ;; Single row
      (org-shop--toggle-mark column))
    (message "%s toggled" (capitalize column))))

;;;###autoload
(defun org-shop-clear-marks ()
  "Clear all marks in current buffer's source table."
  (interactive)
  (save-excursion
    (when (org-shop--goto-table-after-heading org-shop-source-heading)
      (dolist (line-num (org-shop--table-data-lines))
        (org-table-goto-line line-num)
        (when (org-shop--is-marked-p)
          (org-shop--unmark-row)))))
  (message "All marks cleared"))

;;; ============================================================================
;;; Core Functions - Generate Shopping List
;;; ============================================================================

(defun org-shop--get-marked-rows (shop-file)
  "Get marked rows from SHOP-FILE.
Returns list of alists with product data."
  (with-current-buffer (find-file-noselect shop-file)
    (save-excursion
      (when (org-shop--goto-table-after-heading org-shop-source-heading)
        (let ((rows (org-shop--parse-table)))
          (cl-remove-if-not
           (lambda (row)
             (let ((next-val (cdr (assoc "next" row))))
               (and next-val
                    (string-match-p (regexp-quote org-shop--mark-char) next-val))))
           rows))))))

(defun org-shop--clear-marks-in-file (shop-file products)
  "Clear marks for PRODUCTS in SHOP-FILE and update last_bought."
  (with-current-buffer (find-file-noselect shop-file)
    (save-excursion
      (when (org-shop--goto-table-after-heading org-shop-source-heading)
        (dolist (line-num (org-shop--table-data-lines))
          (org-table-goto-line line-num)
          (let ((product (org-shop--get-cell "product")))
            (when (member product products)
              ;; Clear mark
              (when org-shop-clear-marks-after-generate
                (org-shop--unmark-row))
              ;; Update last_bought
              (when org-shop-update-last-bought
                (org-shop--set-cell "last_bought"
                                    (format-time-string "%Y-%m-%d"))))))))
    (save-buffer)))

(defun org-shop--insert-shopping-table (rows)
  "Insert shopping list table with ROWS at point.
Includes summary row with aggregation formulas."
  (let* ((num-items (length rows))
         (total-known 0))
    ;; Calculate initial total
    (dolist (row rows)
      (let ((price (cdr (assoc "price" row))))
        (when (and price (not (string-empty-p price)))
          (setq total-known (+ total-known (string-to-number price))))))
    ;; Insert header (new order: product first)
    (insert "| product | done | count | discount | notes | known_price | new_price |\n")
    (insert "|---------+------+-------+----------+-------+-------------+-----------|\n")
    ;; Insert data rows
    (dolist (row rows)
      (let ((product (cdr (assoc "product" row)))
            (price (cdr (assoc "price" row))))
        (insert (format "| %s | [ ] |  |  |  | %s |  |\n"
                        (or product "")
                        (or price "")))))
    ;; Insert separator before summary
    (insert "|---------+------+-------+----------+-------+-------------+-----------|\n")
    ;; Insert summary row with initial values
    (insert (format "| Summary | %dU 0M |  |  | - | %.2f | +0.00 |\n"
                    num-items total-known))
    ;; Align table
    (org-table-align)))

;;; ============================================================================
;;; Summary Row Calculation Functions (for #+TBLFM)
;;; ============================================================================

(defun org-shop--summary-done-count (_col)
  "Calculate marked/unmarked count for summary row.
Returns string like '3U 2M' (3 unmarked, 2 marked)."
  (let ((marked 0)
        (unmarked 0))
    (save-excursion
      (org-table-goto-line 1)
      (let ((last-line (1- (org-table-current-line))))
        (cl-loop for line from 1 to 100  ; safety limit
                 do (org-table-goto-line line)
                 while (and (org-at-table-p)
                            (< (org-table-current-line) last-line))
                 unless (org-at-table-hline-p)
                 do (let ((done (string-trim (or (org-table-get nil 2) ""))))
                      (if (string-match-p "X" done)
                          (cl-incf marked)
                        (when (string-match-p "\\[ \\]" done)
                          (cl-incf unmarked)))))))
    (format "%dU %dM" unmarked marked)))

(defun org-shop--summary-count (_col)
  "Calculate sum of count column for summary row."
  (let ((total 0))
    (save-excursion
      (org-table-goto-line 1)
      (cl-loop for line from 1 to 100
               do (org-table-goto-line line)
               while (org-at-table-p)
               unless (org-at-table-hline-p)
               do (let* ((val-str (string-trim (or (org-table-get nil 3) "")))
                         (val (if (string-empty-p val-str) 0
                                (string-to-number val-str))))
                    (unless (string= (string-trim (or (org-table-get nil 1) "")) "Summary")
                      (setq total (+ total val))))))
    (if (zerop total) "" (number-to-string total))))

(defun org-shop--summary-discount (_disc-col _price-col)
  "Calculate sum of (discount * known_price) for summary row.
Discount is a decimal (e.g., 0.1 for 10%)."
  (let ((total 0.0))
    (save-excursion
      (org-table-goto-line 1)
      (cl-loop for line from 1 to 100
               do (org-table-goto-line line)
               while (org-at-table-p)
               unless (org-at-table-hline-p)
               do (let* ((disc-str (string-trim (or (org-table-get nil 4) "")))
                         (price-str (string-trim (or (org-table-get nil 6) "")))
                         (product-str (string-trim (or (org-table-get nil 1) ""))))
                    (unless (string= product-str "Summary")
                      (when (and (not (string-empty-p disc-str))
                                 (not (string-empty-p price-str)))
                        (let ((disc (string-to-number disc-str))
                              (price (string-to-number price-str)))
                          (setq total (+ total (* disc price)))))))))
    (if (zerop total) "" (format "%.2f" total))))

(defun org-shop--summary-known-price (_col)
  "Calculate sum of known_price column for summary row."
  (let ((total 0.0))
    (save-excursion
      (org-table-goto-line 1)
      (cl-loop for line from 1 to 100
               do (org-table-goto-line line)
               while (org-at-table-p)
               unless (org-at-table-hline-p)
               do (let* ((val-str (string-trim (or (org-table-get nil 6) "")))
                         (product-str (string-trim (or (org-table-get nil 1) ""))))
                    (unless (string= product-str "Summary")
                      (unless (string-empty-p val-str)
                        (setq total (+ total (string-to-number val-str))))))))
    (format "%.2f" total)))

(defun org-shop--summary-diff (_known-col _new-col)
  "Calculate sum of (new_price - known_price) differences for summary row.
Only counts rows where new_price is filled. Returns with +/- prefix."
  (let ((diff 0.0))
    (save-excursion
      (org-table-goto-line 1)
      (cl-loop for line from 1 to 100
               do (org-table-goto-line line)
               while (org-at-table-p)
               unless (org-at-table-hline-p)
               do (let* ((known-str (string-trim (or (org-table-get nil 6) "")))
                         (new-str (string-trim (or (org-table-get nil 7) "")))
                         (product-str (string-trim (or (org-table-get nil 1) ""))))
                    (unless (string= product-str "Summary")
                      (when (not (string-empty-p new-str))
                        (let ((known (if (string-empty-p known-str) 0
                                       (string-to-number known-str)))
                              (new (string-to-number new-str)))
                          (setq diff (+ diff (- new known)))))))))
    (format "%s%.2f" (if (>= diff 0) "+" "") diff)))

;;;###autoload
(defun org-shop-generate ()
  "Generate shopping list from marked items in shop file.
Inserts table at point with marked products."
  (interactive)
  (let* ((shop-name (org-shop--resolve-shop))
         (shop-file (org-shop--find-shop-file shop-name))
         (marked-rows (org-shop--get-marked-rows shop-file)))
    (unless marked-rows
      (user-error "No marked items found in %s" shop-name))
    ;; Insert the table
    (org-shop--insert-shopping-table marked-rows)
    ;; Clear marks and update last_bought in shop file
    (let ((products (mapcar (lambda (row) (cdr (assoc "product" row)))
                            marked-rows)))
      (org-shop--clear-marks-in-file shop-file products))
    (message "Generated shopping list with %d items from %s"
             (length marked-rows) shop-name)))

;;; ============================================================================
;;; Core Functions - Price Sync
;;; ============================================================================

(defun org-shop--parse-shopping-table ()
  "Parse shopping list table at point.
Returns list of alists with shopping data."
  (when (org-shop--at-table-p)
    (org-shop--parse-table)))

(defun org-shop--update-price-in-shop (shop-file product new-price)
  "Update PRODUCT's price to NEW-PRICE in SHOP-FILE."
  (with-current-buffer (find-file-noselect shop-file)
    (save-excursion
      (when (org-shop--goto-table-after-heading org-shop-source-heading)
        (dolist (line-num (org-shop--table-data-lines))
          (org-table-goto-line line-num)
          (when (string-equal-ignore-case (org-shop--get-cell "product") product)
            (org-shop--set-cell "price" new-price)
            (cl-return t)))))))

(defun org-shop--ensure-history-table (shop-file)
  "Ensure purchase history table exists in SHOP-FILE.
Creates it if missing. Returns point at history table."
  (with-current-buffer (find-file-noselect shop-file)
    (save-excursion
      ;; Try to find existing history heading
      (goto-char (point-min))
      (let ((case-fold-search t))
        (unless (re-search-forward
                 (concat "^\\*+\\s-+" (regexp-quote org-shop-history-heading))
                 nil t)
          ;; Create history section under source heading
          (when (org-shop--goto-heading org-shop-source-heading)
            (org-end-of-subtree)
            (insert "\n\n*** " org-shop-history-heading "                                        :noexport:\n")
            (insert "| product | date | count | price |\n")
            (insert "|---------+------+-------+-------|\n"))))
      ;; Return to history table
      (org-shop--goto-table-after-heading org-shop-history-heading))))

(defun org-shop--find-history-entry (product date)
  "Find history table row matching PRODUCT and DATE.
Returns line number if found, nil otherwise.
Point must be at start of history table."
  (let ((found nil)
        (table-start (point)))
    (while (and (not found) (org-at-table-p) (not (eobp)))
      (unless (org-at-table-hline-p)
        (let ((row-product (string-trim (or (org-table-get nil 1) "")))
              (row-date (string-trim (or (org-table-get nil 2) ""))))
          (when (and (string-equal-ignore-case row-product product)
                     (string-equal row-date date))
            (setq found (line-number-at-pos)))))
      (forward-line 1))
    (goto-char table-start)
    found))

(defun org-shop--delete-table-row ()
  "Delete the current table row."
  (beginning-of-line)
  (let ((start (point)))
    (forward-line 1)
    (delete-region start (point)))
  (org-table-align))

(defun org-shop--upsert-history (shop-file product count price)
  "Upsert purchase history entry for PRODUCT to SHOP-FILE.
If entry for PRODUCT + today exists: update count/price.
If count is 0 or empty: remove entry.
Otherwise: insert new row."
  (let ((date (format-time-string "%Y-%m-%d"))
        (count-num (if (and count (not (string-empty-p count)))
                       (string-to-number count)
                     1)))
    (with-current-buffer (find-file-noselect shop-file)
      (save-excursion
        (org-shop--ensure-history-table shop-file)
        ;; Find the history heading
        (goto-char (point-min))
        (when (re-search-forward
               (concat "^\\*+\\s-+" (regexp-quote org-shop-history-heading) "\\(\\s-\\|:\\)")
               nil t)
          ;; Find the table
          (when (re-search-forward "^\\s-*|" nil t)
            (beginning-of-line)
            ;; Skip header and hline
            (forward-line 1)
            (when (org-at-table-hline-p)
              (forward-line 1))
            (let ((existing-line (org-shop--find-history-entry product date)))
              (cond
               ;; Entry exists and count > 0: update it
               ((and existing-line (> count-num 0))
                (goto-char (point-min))
                (forward-line (1- existing-line))
                (org-table-put nil 3 (number-to-string count-num))
                (org-table-put nil 4 price)
                (org-table-align))
               ;; Entry exists and count = 0: remove it
               ((and existing-line (<= count-num 0))
                (goto-char (point-min))
                (forward-line (1- existing-line))
                (org-shop--delete-table-row))
               ;; No entry and count > 0: insert new row
               ((> count-num 0)
                ;; Go to end of table
                (goto-char (point-min))
                (re-search-forward
                 (concat "^\\*+\\s-+" (regexp-quote org-shop-history-heading) "\\(\\s-\\|:\\)")
                 nil t)
                (re-search-forward "^\\s-*|" nil t)
                (beginning-of-line)
                (while (and (org-at-table-p) (not (eobp)))
                  (forward-line 1))
                (forward-line -1)
                (end-of-line)
                (insert (format "\n| %s | %s | %s | %s |"
                                product
                                date
                                count-num
                                price))
                (org-table-align))
               ;; No entry and count = 0: do nothing
               (t nil)))))))))

;; Keep old name as alias for compatibility
(defalias 'org-shop--append-history 'org-shop--upsert-history)

;;;###autoload
(defun org-shop-sync ()
  "Sync shopping list back to shop file.
For done items [X]: upserts purchase to history (product, date, count, price).
For not-done items [ ]: removes from history if previously synced.
For items with new_price: updates price in shop file.
Re-syncing is safe - updates existing entries instead of creating duplicates."
  (interactive)
  (unless (org-shop--at-table-p)
    (user-error "Not in an org table"))
  (let* ((shop-name (org-shop--resolve-shop))
         (shop-file (org-shop--find-shop-file shop-name))
         (rows (org-shop--parse-shopping-table))
         (logged 0)
         (removed 0)
         (price-updated 0))
    (dolist (row rows)
      (let ((done (cdr (assoc "done" row)))
            (product (cdr (assoc "product" row)))
            (count (cdr (assoc "count" row)))
            (known-price (cdr (assoc "known_price" row)))
            (new-price (cdr (assoc "new_price" row))))
        ;; Determine effective price
        (let ((price (if (and new-price (not (string-empty-p new-price)))
                         new-price
                       known-price)))
          ;; Upsert to history based on done status
          (if (and done (string-match-p "X" done))
              ;; Done: upsert with count (or 1 if empty)
              (progn
                (org-shop--upsert-history shop-file product count price)
                (cl-incf logged))
            ;; Not done: upsert with count=0 to remove from history
            (org-shop--upsert-history shop-file product "0" price)
            (cl-incf removed)))
        ;; Update price in shop file if new_price differs
        (when (and new-price
                   (not (string-empty-p new-price))
                   (not (string-equal new-price known-price)))
          (org-shop--update-price-in-shop shop-file product new-price)
          (cl-incf price-updated))))
    ;; Save shop file
    (with-current-buffer (find-file-noselect shop-file)
      (save-buffer))
    (message "Synced to %s: %d logged, %d removed, %d price(s) updated"
             shop-name logged removed price-updated)))

;;; ============================================================================
;;; Keymap Setup
;;; ============================================================================

;;;###autoload
(defun org-shop-recalculate ()
  "Recalculate the summary row in current shopping list table.
Updates: done count, total count, discount, known_price sum, price diff."
  (interactive)
  (unless (org-at-table-p)
    (user-error "Not in an org table"))
  (save-excursion
    ;; Start from line 2 to skip header
    (org-table-goto-line 2)
    (let ((marked 0) (unmarked 0)
          (total-count 0)
          (total-discount 0.0)
          (total-known 0.0)
          (total-diff 0.0)
          (summary-line nil))
      ;; Iterate through table rows
      (while (and (org-at-table-p) (not (eobp)))
        (unless (org-at-table-hline-p)
          (let ((product (string-trim (or (org-table-get nil 1) ""))))
            (if (string= product "Summary")
                (setq summary-line (org-table-current-line))
              ;; Data row - collect stats (only rows with checkbox format)
              (let ((done (string-trim (or (org-table-get nil 2) "")))
                    (count-str (string-trim (or (org-table-get nil 3) "")))
                    (disc-str (string-trim (or (org-table-get nil 4) "")))
                    (known-str (string-trim (or (org-table-get nil 6) "")))
                    (new-str (string-trim (or (org-table-get nil 7) ""))))
                ;; Only process rows with checkbox format [ ] or [X]
                (when (string-match-p "\\[.\\]" done)
                  ;; Count marked/unmarked
                  (if (string-match-p "X" done)
                      (cl-incf marked)
                    (cl-incf unmarked))
                  ;; Sum counts
                  (unless (string-empty-p count-str)
                    (setq total-count (+ total-count (string-to-number count-str))))
                  ;; Sum known prices
                  (unless (string-empty-p known-str)
                    (setq total-known (+ total-known (string-to-number known-str))))
                  ;; Calculate discount (rate * price)
                  (when (and (not (string-empty-p disc-str))
                             (not (string-empty-p known-str)))
                    (setq total-discount (+ total-discount
                                            (* (string-to-number disc-str)
                                               (string-to-number known-str)))))
                  ;; Calculate diff (new - known)
                  (when (not (string-empty-p new-str))
                    (let ((known (if (string-empty-p known-str) 0
                                   (string-to-number known-str)))
                          (new (string-to-number new-str)))
                      (setq total-diff (+ total-diff (- new known))))))))))
        (forward-line 1))
      ;; Update summary row
      (when summary-line
        (org-table-goto-line summary-line)
        (org-table-put nil 2 (format "%dU %dM" unmarked marked))
        (org-table-put nil 3 (if (zerop total-count) "" (number-to-string total-count)))
        (org-table-put nil 4 (if (zerop total-discount) "" (format "%.2f" total-discount)))
        (org-table-put nil 6 (format "%.2f" total-known))
        (org-table-put nil 7 (format "%s%.2f" (if (>= total-diff 0) "+" "") total-diff))
        (org-table-align)
        (message "Summary recalculated: %dU %dM, total: %.2f, diff: %s%.2f"
                 unmarked marked total-known
                 (if (>= total-diff 0) "+" "") total-diff)))))

(defvar org-shop-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") #'org-shop-mark)
    (define-key map (kbd "g") #'org-shop-generate)
    (define-key map (kbd "s") #'org-shop-sync)
    (define-key map (kbd "c") #'org-shop-clear-marks)
    (define-key map (kbd "r") #'org-shop-recalculate)
    map)
  "Command map for org-shop.
\\{org-shop-command-map}")

;;;###autoload
(defun org-shop-setup ()
  "Setup org-shop with default keybindings.
Binds commands under `org-shop-keymap-prefix' (default C-c S):
  <prefix> m - Toggle mark (next in shop, done in daily)
  <prefix> g - Generate shopping list
  <prefix> s - Sync prices back to shop
  <prefix> c - Clear all marks
  <prefix> r - Recalculate summary row"
  (interactive)
  (when org-shop-setup-keymaps
    (global-set-key (kbd org-shop-keymap-prefix) org-shop-command-map))
  (message "org-shop: Initialized (%s prefix)" org-shop-keymap-prefix))

;;; ============================================================================
;;; Minor Mode (Optional)
;;; ============================================================================

;;;###autoload
(define-minor-mode org-shop-mode
  "Minor mode for org-shop shopping list management."
  :lighter " Shop"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd org-shop-keymap-prefix) org-shop-command-map)
            map)
  :group 'org-shop)

(provide 'org-shop)

;;; org-shop.el ends here
