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

(defcustom org-shop-history-heading "price history"
  "Heading name for price history table."
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

(defcustom org-shop-dest-columns '("done" "product" "count" "discount" "notes" "known_price" "new_price")
  "Column names for generated shopping list tables (in order)."
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
           (concat "^\\*+\\s-+" (regexp-quote heading-name) "\\s-*$")
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
  "Insert shopping list table with ROWS at point."
  (let ((header (mapconcat #'identity org-shop-dest-columns " | ")))
    ;; Insert header
    (insert "| " header " |\n")
    (insert "|")
    (dotimes (_ (length org-shop-dest-columns))
      (insert "---+"))
    (delete-char -1)
    (insert "|\n")
    ;; Insert rows
    (dolist (row rows)
      (let ((product (cdr (assoc "product" row)))
            (price (cdr (assoc "price" row))))
        (insert (format "| [ ] | %s |  |  |  | %s |  |\n"
                        (or product "")
                        (or price "")))))
    ;; Align table
    (org-table-align)))

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
  "Ensure price history table exists in SHOP-FILE.
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
            (insert "| product | date | price |\n")
            (insert "|---------+------+-------|\n"))))
      ;; Return to history table
      (org-shop--goto-table-after-heading org-shop-history-heading))))

(defun org-shop--append-history (shop-file product price)
  "Append price history entry for PRODUCT at PRICE to SHOP-FILE."
  (with-current-buffer (find-file-noselect shop-file)
    (save-excursion
      (org-shop--ensure-history-table shop-file)
      (when (org-shop--goto-table-after-heading org-shop-history-heading)
        ;; Go to end of table
        (while (and (org-at-table-p) (not (eobp)))
          (forward-line 1))
        (forward-line -1)
        (end-of-line)
        (insert (format "\n| %s | %s | %s |"
                        product
                        (format-time-string "%Y-%m-%d")
                        price))
        (org-table-align)))))

;;;###autoload
(defun org-shop-sync ()
  "Sync prices from shopping list back to shop file.
Updates prices where new_price differs from known_price.
Also appends to price history."
  (interactive)
  (unless (org-shop--at-table-p)
    (user-error "Not in an org table"))
  (let* ((shop-name (org-shop--resolve-shop))
         (shop-file (org-shop--find-shop-file shop-name))
         (rows (org-shop--parse-shopping-table))
         (updated 0))
    (dolist (row rows)
      (let ((product (cdr (assoc "product" row)))
            (known-price (cdr (assoc "known_price" row)))
            (new-price (cdr (assoc "new_price" row))))
        ;; Only sync if new_price is filled and different
        (when (and new-price
                   (not (string-empty-p new-price))
                   (not (string-equal new-price known-price)))
          (org-shop--update-price-in-shop shop-file product new-price)
          (org-shop--append-history shop-file product new-price)
          (cl-incf updated))))
    ;; Save shop file
    (with-current-buffer (find-file-noselect shop-file)
      (save-buffer))
    (if (zerop updated)
        (message "No price changes to sync")
      (message "Synced %d price update(s) to %s" updated shop-name))))

;;; ============================================================================
;;; Keymap Setup
;;; ============================================================================

(defvar org-shop-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") #'org-shop-mark)
    (define-key map (kbd "g") #'org-shop-generate)
    (define-key map (kbd "s") #'org-shop-sync)
    (define-key map (kbd "c") #'org-shop-clear-marks)
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
  <prefix> c - Clear all marks"
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
