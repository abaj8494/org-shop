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
;; - Seasonal produce integration (optional)
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
;;
;; Seasonal Produce:
;;   To enable seasonal produce suggestions, set `org-shop-seasons-file':
;;     (setq org-shop-seasons-file "~/path/to/seasons.org")
;;
;;   The seasons.org file should have this structure:
;;     * Summer (Dec-Feb)
;;     ** Fruit
;;     *** Mango
;;     *** Peach
;;     ** Vegetables
;;     *** Tomato
;;     *** Corn
;;     * Autumn (Mar-May)
;;     ...
;;
;;   When C-c S g is invoked, a seasonal subheading will be inserted
;;   below the shopping table with in-season fruits and vegetables.

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

(defcustom org-shop-source-heading "inventory"
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

(defcustom org-shop-seasons-file nil
  "Path to seasons.org file containing seasonal produce data.
If nil, seasonal produce table will not be generated."
  :type '(choice (const :tag "Disabled" nil)
                 (file :tag "Seasons file path"))
  :group 'org-shop)

(defcustom org-shop-seasonal-heading "seasonal fruit + veg"
  "Heading name for seasonal produce subheading.
The word 'seasonal' will be wrapped in an ID link if seasons file has an ID."
  :type 'string
  :group 'org-shop)

(defcustom org-shop-seasonal-required-tag "grocery"
  "Filetag required in shop file for seasonal produce table to appear.
If nil, seasonal table appears for all shops."
  :type '(choice (const :tag "No restriction" nil)
                 (string :tag "Required filetag"))
  :group 'org-shop)

(defcustom org-shop-next-shop-heading "next shop"
  "Heading name for ad-hoc shopping items in shop files.
Items under this heading are plain checklist entries (- [ ] item)
that get copied into the generated shopping list."
  :type 'string
  :group 'org-shop)

(defcustom org-shop-complete-auto-fill t
  "If non-nil, auto-fill price/quantity/notes when selecting a product."
  :type 'boolean
  :group 'org-shop)

(defcustom org-shop-complete-show-annotations t
  "If non-nil, show price and quantity annotations in completion candidates."
  :type 'boolean
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

(defvar org-shop--product-cache (make-hash-table :test 'equal)
  "Cache of products per shop file.
Key: shop-file path, Value: (mod-time . list-of-product-names).")

(defvar org-shop--product-data-cache (make-hash-table :test 'equal)
  "Cache mapping shop files to product data hash tables.
Key: shop-file path, Value: hash-table of (downcase-product -> row-alist).")

(defun org-shop--page-date ()
  "Extract date from current buffer filename if it matches YYYY-MM-DD.
Falls back to today's date."
  (or (when buffer-file-name
        (let ((name (file-name-nondirectory buffer-file-name)))
          (when (string-match "\\`\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" name)
            (match-string 1 name))))
      (format-time-string "%Y-%m-%d")))

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

(defun org-shop--get-shop-file-id (shop-file)
  "Get the org ID from SHOP-FILE, or nil if none."
  (when (and shop-file (file-exists-p shop-file))
    (with-temp-buffer
      (insert-file-contents shop-file nil 0 1000)  ; Only read first 1KB
      (goto-char (point-min))
      (when (re-search-forward "^:ID:\\s-*\\(.+\\)$" nil t)
        (string-trim (match-string 1))))))

(defun org-shop--get-shop-aliases (shop-file)
  "Get ROAM_ALIASES from SHOP-FILE as a list of strings."
  (when (and shop-file (file-exists-p shop-file))
    (with-temp-buffer
      (insert-file-contents shop-file nil 0 2000)  ; Only read first 2KB
      (goto-char (point-min))
      (when (re-search-forward "^:ROAM_ALIASES:\\s-*\\(.+\\)$" nil t)
        (split-string (string-trim (match-string 1)) "\\s-+" t)))))

(defun org-shop--flexible-regexp (term)
  "Return a regexp matching TERM with `-', `_' and space treated alike.
Any run of separators in TERM matches any run of separators in the target,
so \"dan_murphy\" matches \"dan murphy\" and \"dan-murphy\"."
  (replace-regexp-in-string "[-_ ]+" "[-_ ]+" (regexp-quote term)))

(defun org-shop--get-shop-from-heading ()
  "Extract shop name from current heading.
E.g., `** TODO Aldi Run' -> \"aldi\", `** Woolworths Trip' -> \"woolworths\".
Also checks ROAM_ALIASES in shop files (e.g., \"woolies\" -> \"woolworths\")."
  (save-excursion
    (org-back-to-heading t)
    (let ((heading (org-get-heading t t t t)))
      (when heading
        ;; Remove common suffixes and normalize
        (let* ((normalized (downcase heading))
               (normalized (replace-regexp-in-string
                            "\\s-*\\(run\\|trip\\|shop\\|shopping\\|list\\)\\s-*$"
                            "" normalized))
               (normalized (string-trim normalized))
               (shops (org-shop--list-shops)))
          ;; First try to match by shop name directly.  Treat -, _ and
          ;; space as interchangeable so file names like "dan_murphy"
          ;; match headings like "Dan Murphy's".
          (or (cl-find-if (lambda (shop)
                            (string-match-p (org-shop--flexible-regexp shop)
                                            normalized))
                          shops)
              ;; Then try to match by ROAM_ALIASES
              (cl-find-if (lambda (shop)
                            (let* ((shop-file (org-shop--find-shop-file shop))
                                   (aliases (org-shop--get-shop-aliases shop-file)))
                              (cl-some (lambda (alias)
                                         (string-match-p
                                          (org-shop--flexible-regexp (downcase alias))
                                          normalized))
                                       aliases)))
                          shops)))))))

(defun org-shop--linkify-shop-in-heading (shop-name)
  "Replace shop name in current heading with an org ID link.
SHOP-NAME is the resolved shop (e.g., \"costco\").
Transforms \"Costco Shop\" to \"[[id:xxx][Costco]] Shop\".
Also matches ROAM_ALIASES (e.g., \"Woolies\" -> link to woolworths)."
  (let* ((shop-file (org-shop--find-shop-file shop-name))
         (shop-id (org-shop--get-shop-file-id shop-file))
         (aliases (org-shop--get-shop-aliases shop-file)))
    (when shop-id
      (save-excursion
        (org-back-to-heading t)
        (let* ((heading-start (point))
               (heading-end (line-end-position))
               (heading-text (buffer-substring heading-start heading-end)))
          ;; Skip if already contains an ID link
          (unless (string-match-p "\\[\\[id:" heading-text)
            ;; Build list of terms to search for: shop name + aliases
            (let ((search-terms (cons shop-name (or aliases '())))
                  (case-fold-search t))  ; Case-insensitive matching
              ;; Find and replace the first matching term
              (cl-loop for term in search-terms
                       when (string-match (org-shop--flexible-regexp term) heading-text)
                       do (let* ((match-start (+ heading-start (match-beginning 0)))
                                 (match-end (+ heading-start (match-end 0)))
                                 (original-text (buffer-substring match-start match-end))
                                 (link (format "[[id:%s][%s]]" shop-id original-text)))
                            (goto-char match-start)
                            (delete-region match-start match-end)
                            (insert link))
                       and return t))))))))

(defun org-shop--resolve-shop ()
  "Determine which shop to use based on settings and context.
Also linkifies the shop name in the heading with an org ID link."
  (let ((shop (if org-shop-detect-shop-from-heading
                  (or (org-shop--get-shop-from-heading)
                      (org-shop--prompt-shop))
                (org-shop--prompt-shop))))
    ;; Linkify the shop name in the heading
    (when shop
      (org-shop--linkify-shop-in-heading shop))
    shop))

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

(defun org-shop--get-heading-level (heading-name)
  "Get the level (number of stars) for HEADING-NAME in current buffer.
Returns nil if heading not found."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward
             (concat "^\\(\\*+\\)\\s-+" (regexp-quote heading-name) "\\(\\s-\\|:\\|$\\)")
             nil t)
        (length (match-string 1))))))

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

(defun org-shop--ensure-column (column-name &optional after-column)
  "Ensure COLUMN-NAME exists in current table.
If AFTER-COLUMN is specified, inserts after that column.
Otherwise inserts at the end.
Returns t if column was created, nil if it already existed."
  (if (org-shop--has-column-p column-name)
      nil
    (save-excursion
      (let* ((columns (org-shop--get-table-columns))
             (insert-idx (if after-column
                             (1+ (or (cl-position after-column columns
                                                  :test #'string-equal-ignore-case)
                                     (1- (length columns))))
                           (length columns))))
        ;; Go to header row and the target column position
        (org-table-goto-line 1)
        (org-table-goto-column (1+ insert-idx))
        ;; Insert a new column (inserts to the left of current)
        (org-table-insert-column)
        ;; Set the header cell
        (org-table-put nil (1+ insert-idx) column-name)
        (org-table-align)
        t))))

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
;;; Seasonal Produce Functions
;;; ============================================================================

(defun org-shop--season-from-date (date-string)
  "Return Australian season name for DATE-STRING (YYYY-MM-DD format).
Returns one of: \"Summer\", \"Autumn\", \"Winter\", \"Spring\"."
  (let ((month (string-to-number (substring date-string 5 7))))
    (cond
     ((member month '(12 1 2)) "Summer")
     ((member month '(3 4 5)) "Autumn")
     ((member month '(6 7 8)) "Winter")
     ((member month '(9 10 11)) "Spring"))))

(defun org-shop--shop-has-filetag-p (shop-file tag)
  "Return non-nil if SHOP-FILE has TAG in its filetags."
  (when (and shop-file tag)
    (with-current-buffer (find-file-noselect shop-file)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^#\\+filetags:.*" nil t)
          (let ((filetags-line (match-string 0)))
            (string-match-p (concat ":" (regexp-quote tag) ":") filetags-line)))))))

(defun org-shop--get-seasons-file-id ()
  "Get the org ID from `org-shop-seasons-file', or nil if none."
  (when (and org-shop-seasons-file
             (file-exists-p org-shop-seasons-file))
    (with-current-buffer (find-file-noselect org-shop-seasons-file)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^:ID:\\s-*\\(.+\\)$" nil t)
          (string-trim (match-string 1)))))))

(defun org-shop--parse-seasons-file ()
  "Parse `org-shop-seasons-file' and return alist of seasons to produce.
Returns ((\"Summer\" . ((\"Fruit\" . (items...)) (\"Vegetables\" . (items...))))
         (\"Autumn\" . ...) ...)."
  (when (and org-shop-seasons-file
             (file-exists-p org-shop-seasons-file))
    (with-temp-buffer
      (insert-file-contents org-shop-seasons-file)
      (org-mode)
      (let ((seasons '())
            (current-season nil)
            (current-type nil))
        (goto-char (point-min))
        (while (not (eobp))
          (when (looking-at "^\\(\\*+\\)\\s-+\\(.+?\\)\\s-*\\(:.+:\\)?$")
            (let* ((level (length (match-string 1)))
                   (heading (string-trim (match-string 2)))
                   (heading-clean (replace-regexp-in-string "\\s-*(.*)" "" heading)))
              (cond
               ;; Level 1: Season heading (e.g., "Summer (Dec-Feb)")
               ((= level 1)
                (when (string-match "^\\(Summer\\|Autumn\\|Winter\\|Spring\\|Year-Round\\)" heading)
                  (setq current-season (match-string 1 heading))
                  (unless (assoc current-season seasons)
                    (push (cons current-season '()) seasons))))
               ;; Level 2: Type heading (Fruit/Vegetables)
               ((= level 2)
                (when (and current-season
                           (member heading-clean '("Fruit" "Vegetables")))
                  (setq current-type heading-clean)))
               ;; Level 3: Individual produce item
               ((= level 3)
                (when (and current-season current-type)
                  (let* ((season-data (assoc current-season seasons))
                         (type-data (assoc current-type (cdr season-data)))
                         (item-name heading-clean))
                    (if type-data
                        (setcdr type-data (append (cdr type-data) (list item-name)))
                      (setcdr season-data
                              (append (cdr season-data)
                                      (list (cons current-type (list item-name))))))))))))
          (forward-line 1))
        (nreverse seasons)))))

(defun org-shop--get-seasonal-produce-separate (date-string)
  "Get seasonal produce for DATE-STRING, separating year-round from seasonal.
Returns plist (:year-round-fruits :year-round-veg :seasonal-fruits :seasonal-veg)."
  (let* ((season (org-shop--season-from-date date-string))
         (all-seasons (org-shop--parse-seasons-file))
         (season-data (cdr (assoc season all-seasons)))
         (year-round-data (cdr (assoc "Year-Round" all-seasons))))
    (list :year-round-fruits (cdr (assoc "Fruit" year-round-data))
          :year-round-veg (cdr (assoc "Vegetables" year-round-data))
          :seasonal-fruits (cdr (assoc "Fruit" season-data))
          :seasonal-veg (cdr (assoc "Vegetables" season-data)))))

(defun org-shop--current-heading-level ()
  "Return the level of the current org heading, or 0 if not under a heading."
  (save-excursion
    (condition-case nil
        (progn
          (org-back-to-heading t)
          (org-current-level))
      (error 0))))

(defun org-shop--seasonal-heading-exists-in-parent-p ()
  "Check if seasonal produce heading exists under current parent heading.
Returns non-nil if a seasonal heading is found at the current heading level
within the same parent subtree."
  (save-excursion
    (let ((current-level (org-shop--current-heading-level)))
      (when (> current-level 1)
        ;; Go to parent heading
        (outline-up-heading 1 t)
        (let ((parent-end (save-excursion (org-end-of-subtree t) (point))))
          ;; Search for seasonal heading within parent at the same level
          (re-search-forward
           (concat "^\\*\\{" (number-to-string current-level) "\\}\\s-+.*"
                   "seasonal.*fruit.*veg")
           parent-end t))))))

(defun org-shop--insert-seasonal-table (date-string shop-file)
  "Insert seasonal produce heading and table for DATE-STRING.
Only inserts if SHOP-FILE has the required filetag (if configured)
and if no seasonal heading already exists under the parent heading.
Inserts at the same level as current heading (as a sibling)."
  (when org-shop-seasons-file
    ;; Check filetag requirement
    (when (or (null org-shop-seasonal-required-tag)
              (org-shop--shop-has-filetag-p shop-file org-shop-seasonal-required-tag))
      ;; Only insert if no seasonal heading exists in parent
      (unless (org-shop--seasonal-heading-exists-in-parent-p)
        (let* ((season (org-shop--season-from-date date-string))
               (season-tag (downcase season))
               (seasons-id (org-shop--get-seasons-file-id))
               (produce (org-shop--get-seasonal-produce-separate date-string))
               (yr-fruits (plist-get produce :year-round-fruits))
               (yr-veg (plist-get produce :year-round-veg))
               (s-fruits (plist-get produce :seasonal-fruits))
               (s-veg (plist-get produce :seasonal-veg))
               (all-fruits (append yr-fruits s-fruits))
               (all-veg (append yr-veg s-veg))
               (current-level (org-shop--current-heading-level))
               (stars (make-string current-level ?*))
               ;; Build heading with optional ID link
               (seasonal-word (if seasons-id
                                  (format "[[id:%s][seasonal]]" seasons-id)
                                "seasonal"))
               (heading-text (replace-regexp-in-string
                              "seasonal" seasonal-word
                              org-shop-seasonal-heading t t)))
          (when (or all-fruits all-veg)
            ;; Insert blank line and heading with season tag (at same level, not sub-level)
            (insert (format "\n\n%s %s %s:%s:\n\n"
                            stars heading-text
                            (make-string (max 0 (- 70 (length heading-text) current-level 3)) ?\s)
                            season-tag))
            ;; Insert table header
            (insert "|------+--------------+------+--------------|\n")
            (insert "| done | fruit        | done | vegetable    |\n")
            (insert "|------+--------------+------+--------------|\n")
            ;; Insert year-round rows first
            (let ((yr-max (max (length yr-fruits) (length yr-veg))))
              (dotimes (i yr-max)
                (let ((fruit (or (nth i yr-fruits) ""))
                      (veg (or (nth i yr-veg) "")))
                  (insert (format "| [ ]  | %s | [ ]  | %s |\n" fruit veg)))))
            ;; Insert hline separator between year-round and seasonal
            (when (and (or yr-fruits yr-veg)
                       (or s-fruits s-veg))
              (insert "|------+--------------+------+--------------|\n"))
            ;; Insert seasonal rows
            (let ((s-max (max (length s-fruits) (length s-veg))))
              (dotimes (i s-max)
                (let ((fruit (or (nth i s-fruits) ""))
                      (veg (or (nth i s-veg) "")))
                  (insert (format "| [ ]  | %s | [ ]  | %s |\n" fruit veg)))))
            ;; Insert bottom hline
            (insert "|------+--------------+------+--------------|\n")
            ;; Align table
            (save-excursion
              (forward-line -1)
              (org-table-align))))))))

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

(defun org-shop--next-shop-heading-regexp ()
  "Regexp matching the `next shop' heading.
Treats -, _ and space as interchangeable so a configured heading of
\"next shop\" also matches \"* next-shop\"."
  (concat "^\\*+\\s-+"
          (org-shop--flexible-regexp org-shop-next-shop-heading)
          "\\s-*$"))

(defun org-shop--trim-blank-lines (lines)
  "Drop leading and trailing blank (whitespace-only) lines from LINES."
  (let ((blank-p (lambda (l) (string-match-p "\\`\\s-*\\'" l))))
    (while (and lines (funcall blank-p (car lines)))
      (setq lines (cdr lines)))
    (setq lines (nreverse lines))
    (while (and lines (funcall blank-p (car lines)))
      (setq lines (cdr lines)))
    (nreverse lines)))

(defun org-shop--get-next-shop-items (shop-file)
  "Get the checklist lines under the `next shop' heading in SHOP-FILE.
Returns the raw lines verbatim, preserving indentation, checkbox state and
nested sub-items, or nil if the heading is missing or empty."
  (when (and shop-file (file-exists-p shop-file))
    (with-current-buffer (find-file-noselect shop-file)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward (org-shop--next-shop-heading-regexp) nil t)
          (forward-line 1)
          (let* ((start (point))
                 (end (save-excursion
                        (if (re-search-forward "^\\*" nil t)
                            (line-beginning-position)
                          (point-max))))
                 (lines (split-string
                         (buffer-substring-no-properties start end) "\n")))
            (org-shop--trim-blank-lines lines)))))))

(defun org-shop--insert-next-shop-items (items)
  "Insert ITEMS (verbatim checklist lines) under a plain text block."
  (when items
    (insert "\n")
    (dolist (item items)
      (insert item "\n"))))

(defun org-shop--clear-next-shop-in-file (shop-file)
  "Clear all items under the `next shop' heading in SHOP-FILE."
  (with-current-buffer (find-file-noselect shop-file)
    (save-excursion
      (goto-char (point-min))
      (let ((heading-re (org-shop--next-shop-heading-regexp)))
        (when (re-search-forward heading-re nil t)
          (forward-line 1)
          (let ((start (point))
                (end (save-excursion
                       (if (re-search-forward "^\\*" nil t)
                           (line-beginning-position)
                         (point-max)))))
            ;; Delete content between heading and next heading, leave a blank line
            (delete-region start end)
            (insert "\n")))))
    (save-buffer)))

(defun org-shop--clear-marks-in-file (shop-file products &optional date)
  "Clear marks for PRODUCTS in SHOP-FILE and update last_bought.
DATE defaults to the page date of the calling buffer."
  (let ((date (or date (org-shop--page-date))))
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
                  (org-shop--set-cell "last_bought" date)))))))
      (save-buffer))))

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
    ;; Insert top hline
    (insert "|------+------+-------+----------+----------+-------+-------------+-----------|\n")
    ;; Insert header
    (insert "| product | done | count | discount | quantity | notes | known_price | new_price |\n")
    (insert "|------+------+-------+----------+----------+-------+-------------+-----------|\n")
    ;; Insert data rows
    (dolist (row rows)
      (let ((product (cdr (assoc "product" row)))
            (price (cdr (assoc "price" row)))
            (quantity (cdr (assoc "quantity" row)))
            (notes (cdr (assoc "notes" row))))
        (insert (format "| %s | [ ] |  |  | %s | %s | %s |  |\n"
                        (or product "")
                        (or quantity "")
                        (or notes "")
                        (if (and price (not (string-empty-p price)))
                            (format "%.2f" (string-to-number price))
                          "-")))))
    ;; Insert separator before summary
    (insert "|------+------+-------+----------+----------+-------+-------------+-----------|\n")
    ;; Insert summary row with initial values
    (insert (format "| Summary | %dU 0M |  |  |  | - | %.2f | +0.00 |\n"
                    num-items total-known))
    ;; Insert bottom hline
    (insert "|------+------+-------+----------+----------+-------+-------------+-----------|\n")
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
                          (setq total (+ total (org-shop--round-price (* disc price))))))))))
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
                          (setq diff (+ diff (org-shop--round-price (- new known))))))))))
    (format "%s%.2f" (if (>= diff 0) "+" "") diff)))

(defun org-shop--insert-empty-shopping-table ()
  "Insert an empty shopping list table at point."
  ;; Insert top hline
  (insert "|------+------+-------+----------+----------+-------+-------------+-----------|\n")
  ;; Insert header
  (insert "| product | done | count | discount | quantity | notes | known_price | new_price |\n")
  (insert "|------+------+-------+----------+----------+-------+-------------+-----------|\n")
  ;; Insert separator before summary
  (insert "|------+------+-------+----------+----------+-------+-------------+-----------|\n")
  ;; Insert summary row
  (insert "| Summary | 0U 0M |  |  |  | - | 0.00 | +0.00 |\n")
  ;; Insert bottom hline
  (insert "|------+------+-------+----------+----------+-------+-------------+-----------|\n")
  ;; Align table
  (org-table-align))

(defun org-shop--ensure-shopping-heading ()
  "Ensure we're under a ** Shopping heading. Create one if needed.
Returns point at the Shopping heading."
  (save-excursion
    (let ((found nil))
      ;; Check if we're already under a Shopping heading
      (condition-case nil
          (progn
            (org-back-to-heading t)
            (let ((heading (org-get-heading t t t t)))
              (when (string-match-p "\\bShopping\\b" heading)
                (setq found (point)))))
        (error nil))
      ;; Check parent headings
      (unless found
        (condition-case nil
            (while (and (not found) (> (org-outline-level) 1))
              (outline-up-heading 1 t)
              (let ((heading (org-get-heading t t t t)))
                (when (string-match-p "\\bShopping\\b" heading)
                  (setq found (point)))))
          (error nil)))
      found)))

(defun org-shop--find-or-create-shopping-heading ()
  "Find or create a ** Shopping heading under * Capture.
Returns point at the Shopping heading."
  (let ((existing (org-shop--ensure-shopping-heading)))
    (if existing
        (goto-char existing)
      ;; Find * Capture heading
      (goto-char (point-min))
      (if (re-search-forward "^\\* Capture" nil t)
          (progn
            ;; Go to end of Capture subtree
            (org-end-of-subtree t)
            ;; Insert new Shopping heading
            (insert "\n\n** Shopping\n")
            (forward-line -1)
            (point))
        (user-error "No * Capture heading found")))))

(defun org-shop--at-shop-heading-p ()
  "Return shop name if point is at a heading that matches a shop.
Returns nil if not at a shop heading."
  (when (org-at-heading-p)
    (org-shop--get-shop-from-heading)))

;;;###autoload
(defun org-shop-generate (&optional empty)
  "Generate shopping list from marked items in shop file.
Inserts table at point with marked products.
If `org-shop-seasons-file' is set, inserts a seasonal produce heading
as a sibling (same level), but only once per parent heading.

With prefix arg EMPTY (or if no marked items), generates empty table structure
under a ** Shopping heading (created if needed under * Capture).
If already at a shop heading, linkifies it and inserts table below."
  (interactive "P")
  (let* ((at-shop-heading (org-shop--at-shop-heading-p))
         (date (org-shop--page-date))
         (shop-name (or at-shop-heading
                        (org-shop--get-shop-from-heading)
                        (org-shop--prompt-shop)))
         (shop-file (org-shop--find-shop-file shop-name))
         (marked-rows (unless empty (org-shop--get-marked-rows shop-file)))
         (next-shop-items (org-shop--get-next-shop-items shop-file)))
    (if (or empty (not marked-rows))
        ;; Empty table generation mode
        (progn
          (if at-shop-heading
              ;; Already at a shop heading - linkify it and insert table below
              (progn
                (org-shop--linkify-shop-in-heading shop-name)
                (org-end-of-subtree t)
                (insert "\n\n"))
            ;; Not at shop heading - create structure
            (org-shop--find-or-create-shopping-heading)
            (org-end-of-subtree t)
            (insert "\n")
            ;; Insert seasonal heading first if configured
            (when org-shop-seasons-file
              (let ((shop-file-for-seasonal shop-file))
                ;; We need to be at a heading for org-shop--insert-seasonal-table
                ;; Insert a temporary shop heading, let seasonal insert as sibling
                (insert "\n*** placeholder\n")
                (forward-line -1)
                (org-shop--insert-seasonal-table date shop-file-for-seasonal)
                ;; Remove placeholder
                (save-excursion
                  (when (re-search-forward "^\\*\\*\\* placeholder$" nil t)
                    (beginning-of-line)
                    (delete-region (point) (1+ (line-end-position)))))))
            ;; Insert shop heading with link
            (let ((shop-id (org-shop--get-shop-file-id shop-file)))
              (if shop-id
                  (insert (format "\n*** [[id:%s][%s]]\n\n" shop-id (capitalize shop-name)))
                (insert (format "\n*** %s\n\n" (capitalize shop-name))))))
          ;; Insert empty table
          (org-shop--insert-empty-shopping-table)
          ;; Insert next shop items if any
          (when next-shop-items
            (org-shop--insert-next-shop-items next-shop-items)
            (org-shop--clear-next-shop-in-file shop-file))
          (message "Generated empty shopping structure for %s" shop-name))
      ;; Normal mode with marked items
      (org-shop--insert-shopping-table marked-rows)
      ;; Insert next shop items if any
      (when next-shop-items
        (org-shop--insert-next-shop-items next-shop-items)
        (org-shop--clear-next-shop-in-file shop-file))
      ;; Insert seasonal produce subheading if configured
      (when org-shop-seasons-file
        (org-shop--insert-seasonal-table date shop-file))
      ;; Clear marks and update last_bought in shop file
      (let ((products (mapcar (lambda (row) (cdr (assoc "product" row)))
                              marked-rows)))
        (org-shop--clear-marks-in-file shop-file products date))
      (message "Generated shopping list with %d items from %s"
               (length marked-rows) shop-name))))

;;; ============================================================================
;;; Core Functions - Price Sync
;;; ============================================================================

(defun org-shop--parse-shopping-table ()
  "Parse shopping list table at point.
Returns list of alists with shopping data."
  (when (org-shop--at-table-p)
    (org-shop--parse-table)))

(defun org-shop--update-price-in-shop (shop-file product new-price)
  "Update PRODUCT's price to NEW-PRICE in SHOP-FILE.
Returns t if product was found and updated, nil otherwise."
  (with-current-buffer (find-file-noselect shop-file)
    (save-excursion
      (when (org-shop--goto-table-after-heading org-shop-source-heading)
        (cl-loop for line-num in (org-shop--table-data-lines)
                 do (org-table-goto-line line-num)
                 when (string-equal-ignore-case (org-shop--get-cell "product") product)
                 return (progn (org-shop--set-cell "price" new-price) t))))))

(defun org-shop--update-notes-in-shop (shop-file product notes)
  "Update PRODUCT's notes to NOTES in SHOP-FILE.
Creates notes column if it doesn't exist (after quantity column).
Returns t if product was found and updated, nil otherwise."
  (with-current-buffer (find-file-noselect shop-file)
    (save-excursion
      (when (org-shop--goto-table-after-heading org-shop-source-heading)
        ;; Ensure notes column exists (insert after quantity if needed)
        (org-shop--ensure-column "notes" "quantity")
        (cl-loop for line-num in (org-shop--table-data-lines)
                 do (org-table-goto-line line-num)
                 when (string-equal-ignore-case (org-shop--get-cell "product") product)
                 return (progn (org-shop--set-cell "notes" notes) t))))))

(defun org-shop--update-quantity-in-shop (shop-file product quantity)
  "Update PRODUCT's quantity to QUANTITY in SHOP-FILE.
Returns t if product was found and updated, nil otherwise."
  (with-current-buffer (find-file-noselect shop-file)
    (save-excursion
      (when (org-shop--goto-table-after-heading org-shop-source-heading)
        (cl-loop for line-num in (org-shop--table-data-lines)
                 do (org-table-goto-line line-num)
                 when (string-equal-ignore-case (org-shop--get-cell "product") product)
                 return (progn (org-shop--set-cell "quantity" quantity) t))))))

(defun org-shop--product-exists-in-shop-p (shop-file product)
  "Check if PRODUCT exists in SHOP-FILE's regular table."
  (with-current-buffer (find-file-noselect shop-file)
    (save-excursion
      (when (org-shop--goto-table-after-heading org-shop-source-heading)
        (cl-loop for line-num in (org-shop--table-data-lines)
                 do (org-table-goto-line line-num)
                 when (string-equal-ignore-case (org-shop--get-cell "product") product)
                 return t)))))

(defun org-shop--add-product-to-shop (shop-file product price &optional notes date)
  "Add new PRODUCT with PRICE and optional NOTES to SHOP-FILE's regular table.
DATE is the purchase date; defaults to today.
Inserts before any TOTAL row or at end of table data.
Creates notes column if needed when notes are provided."
  (with-current-buffer (find-file-noselect shop-file)
    (save-excursion
      (when (org-shop--goto-table-after-heading org-shop-source-heading)
        ;; Ensure notes column exists if we have notes to add
        (when (and notes (not (string-empty-p notes)))
          (org-shop--ensure-column "notes" "quantity"))
        ;; Check if notes column exists (may have just been created)
        (let ((has-notes-col (org-shop--has-column-p "notes")))
          ;; Skip header
          (forward-line 1)
          (when (org-at-table-hline-p)
            (forward-line 1))
          ;; Find insertion point (before hline/TOTAL or end of data)
          (let ((insert-point nil))
            (while (and (org-at-table-p) (not (eobp)))
              (let ((first-col (string-trim (or (org-table-get nil 1) ""))))
                (if (or (org-at-table-hline-p)
                        (string-match-p "^TOTAL$\\|^$" first-col))
                    ;; Found hline or TOTAL/empty - insert before
                    (progn
                      (forward-line -1)
                      (end-of-line)
                      (setq insert-point (point))
                      (goto-char (point-max))) ; exit loop
                  (setq insert-point (progn (end-of-line) (point)))
                  (forward-line 1))))
            (when insert-point
              (goto-char insert-point)
              ;; Format depends on whether notes column exists
              (let ((d (or date (format-time-string "%Y-%m-%d"))))
                (if has-notes-col
                    ;; Format: | next | product | price | quantity | notes | last_bought |
                    (insert (format "\n| [ ] | %s | %s |  | %s | %s |"
                                    product
                                    (or price "")
                                    (or notes "")
                                    d))
                  ;; Format: | next | product | price | quantity | last_bought |
                  (insert (format "\n| [ ] | %s | %s |  | %s |"
                                  product
                                  (or price "")
                                  d))))
              (org-table-align))))))))

(defun org-shop--ensure-history-table (shop-file)
  "Ensure purchase history table exists in SHOP-FILE.
Creates it if missing. Migrates old 4-column format if needed.
Returns point at history table."
  (with-current-buffer (find-file-noselect shop-file)
    (save-excursion
      ;; Try to find existing history heading
      (goto-char (point-min))
      (let ((case-fold-search t))
        (if (re-search-forward
             (concat "^\\*+\\s-+" (regexp-quote org-shop-history-heading))
             nil t)
            ;; Found existing - check if migration needed
            (when (org-shop--goto-table-after-heading org-shop-history-heading)
              (when (org-shop--history-needs-migration-p)
                (org-shop--migrate-history-table)))
          ;; Create history section as sibling of source heading
          (when (org-shop--goto-heading org-shop-source-heading)
            (let ((source-level (org-shop--get-heading-level org-shop-source-heading)))
              (org-end-of-subtree)
              (insert (format "\n\n%s %s%s:noexport:\n"
                              (make-string source-level ?*)
                              org-shop-history-heading
                              (make-string (max 1 (- 60 (length org-shop-history-heading))) ?\s)))
              (insert "|---------+------+-------+----------+-------|\n")
              (insert "| product | date | count | discount | price |\n")
              (insert "|---------+------+-------+----------+-------|\n")))))
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

(defun org-shop--history-needs-migration-p ()
  "Return non-nil if history table at point needs discount column migration.
Returns t if table header has 4 columns (old format) instead of 5 (new format)."
  (when (org-at-table-p)
    (save-excursion
      ;; Go to header row
      (org-table-goto-line 1)
      (let ((header (org-table-get nil 4)))
        ;; Old format: column 4 is "price", new format: column 4 is "discount"
        (and header
             (string-match-p "price" (downcase (string-trim header))))))))

(defun org-shop--migrate-history-table ()
  "Migrate history table at point to include discount column.
Inserts empty discount column between count and price columns."
  (when (org-at-table-p)
    (save-excursion
      ;; Find the table start
      (org-table-goto-line 1)
      ;; Insert a column before the price column (column 4)
      (org-table-goto-column 4)
      (org-table-insert-column)
      ;; Set the header for the new column
      (org-table-put 1 4 "discount")
      ;; All data rows now have empty discount (which is correct for old entries)
      (org-table-align)
      (message "Migrated history table to include discount column"))))

(defun org-shop--reorganize-history-table ()
  "Reorganize history table: sort by date descending with dividers between date groups.
Point must be within the history table."
  (when (org-at-table-p)
    (let ((rows '())
          (total-row nil)
          (table-start nil)
          (table-end nil))
      ;; Find table boundaries and collect all data rows
      (org-table-goto-line 1)
      (setq table-start (point))
      ;; Skip header row
      (forward-line 1)
      ;; Skip header hline if present
      (when (org-at-table-hline-p)
        (forward-line 1))
      ;; Collect all data rows (skip hlines)
      (while (and (org-at-table-p) (not (eobp)))
        (if (org-at-table-hline-p)
            (forward-line 1)
          (let ((product (string-trim (or (org-table-get nil 1) "")))
                (date (string-trim (or (org-table-get nil 2) "")))
                (count-str (string-trim (or (org-table-get nil 3) "")))
                (discount-str (string-trim (or (org-table-get nil 4) "")))
                (price-str (string-trim (or (org-table-get nil 5) ""))))
            (if (string= product "TOTAL")
                (setq total-row (list product date count-str discount-str price-str))
              ;; Only collect rows with valid date format
              (when (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" date)
                (push (list product date count-str discount-str price-str) rows))))
          (forward-line 1)))
      (setq table-end (point))
      ;; Sort rows by date descending
      (setq rows (sort rows (lambda (a b)
                              (string> (nth 1 a) (nth 1 b)))))
      ;; Delete old table content (keep header)
      (goto-char table-start)
      (forward-line 1) ; skip header
      (when (org-at-table-hline-p)
        (forward-line 1)) ; skip header hline
      (let ((data-start (point)))
        (delete-region data-start table-end))
      ;; Insert sorted rows with date dividers
      (let ((prev-date nil))
        (dolist (row rows)
          (let ((product (nth 0 row))
                (date (nth 1 row))
                (count-str (nth 2 row))
                (discount-str (nth 3 row))
                (price-str (nth 4 row)))
            ;; Add hline between different dates
            (when (and prev-date (not (string= prev-date date)))
              (insert "|--------------------+------------+--------+----------+--------|\n"))
            (insert (format "| %s | %s | %s | %s | %s |\n"
                            product date count-str discount-str price-str))
            (setq prev-date date))))
      ;; Add TOTAL row with dividers if we have data
      (when (and rows total-row)
        (insert "|--------------------+------------+--------+----------+--------|\n")
        (insert (format "| %s | %s | %s | %s | %s |\n"
                        (nth 0 total-row) (nth 1 total-row)
                        (nth 2 total-row) (nth 3 total-row) (nth 4 total-row)))
        (insert "|--------------------+------------+--------+----------+--------|\n"))
      ;; Align the table
      (org-table-goto-line 1)
      (org-table-align))))

(defun org-shop--upsert-history (shop-file product count price &optional date discount)
  "Upsert purchase history entry for PRODUCT to SHOP-FILE.
DATE is the purchase date; defaults to today.
DISCOUNT is the discount rate (e.g., 0.3 for 30% off).
If entry for PRODUCT + date exists: update count/price/discount.
If count is 0 or empty: remove entry.
Otherwise: insert new row."
  (let* ((date (or date (format-time-string "%Y-%m-%d")))
         (count-num (if (and count (not (string-empty-p count)))
                        (string-to-number count)
                      1))
         (discount-rate (if (and discount (not (string-empty-p discount)))
                            (string-to-number discount)
                          0))
         (price-num (if (and price (not (string-empty-p price)))
                        (string-to-number price)
                      0))
         (dollars-saved (org-shop--round-price (* price-num discount-rate count-num)))
         (formatted-discount (if (> discount-rate 0)
                                 (format "$%.2f (%d%%)" dollars-saved (round (* 100 discount-rate)))
                               "")))
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
                (org-table-put nil 4 formatted-discount)
                (org-table-put nil 5 price)
                (org-table-align))
               ;; Entry exists and count = 0: remove it
               ((and existing-line (<= count-num 0))
                (goto-char (point-min))
                (forward-line (1- existing-line))
                (org-shop--delete-table-row))
               ;; No entry and count > 0: insert new row
               ((> count-num 0)
                ;; Find insertion point: before hline/TOTAL, after last data row
                (goto-char (point-min))
                (re-search-forward
                 (concat "^\\*+\\s-+" (regexp-quote org-shop-history-heading) "\\(\\s-\\|:\\)")
                 nil t)
                (re-search-forward "^\\s-*|" nil t)
                (beginning-of-line)
                ;; Skip header
                (forward-line 1)
                (let ((after-header-hline nil))
                  (when (org-at-table-hline-p)
                    (setq after-header-hline (point))
                    (forward-line 1))
                  ;; Find last data row (before hline or TOTAL)
                  (let ((insert-point nil))
                    ;; Handle empty table (no data rows)
                    (if (not (org-at-table-p))
                        ;; Table is empty, insert after the header hline
                        (when after-header-hline
                          (goto-char after-header-hline)
                          (end-of-line)
                          (setq insert-point (point)))
                      ;; Table has data rows, find last one
                      (while (and (org-at-table-p) (not (eobp)))
                        (if (or (org-at-table-hline-p)
                                (string= (string-trim (or (org-table-get nil 1) "")) "TOTAL"))
                            ;; Found hline or TOTAL - insert before this
                            (progn
                              (forward-line -1)
                              (end-of-line)
                              (setq insert-point (point))
                              (goto-char (point-max))) ; exit loop
                          (setq insert-point (progn (end-of-line) (point)))
                          (forward-line 1))))
                    (when insert-point
                      (goto-char insert-point)
                      (insert (format "\n| %s | %s | %s | %s | %s |"
                                      product
                                      date
                                      count-num
                                      formatted-discount
                                      price))
                      (org-table-align)))))
               ;; No entry and count = 0: do nothing
               (t nil)))
            ;; Reorganize table (sort by date descending, add dividers)
            (org-shop--reorganize-history-table)
            ;; Recalculate TOTAL row
            (org-shop--recalculate-history-total)))))))

;; Keep old name as alias for compatibility
(defalias 'org-shop--append-history 'org-shop--upsert-history)

(defun org-shop--recalculate-history-total ()
  "Recalculate TOTAL row in current history table.
Calculates: unique days shopped, total item count, total discount saved, total price."
  (when (org-at-table-p)
    (let ((dates (make-hash-table :test 'equal))
          (total-count 0)
          (total-discount 0.0)
          (total-price 0.0)
          (total-line nil)
          (table-start (point)))
      ;; First pass: collect stats and find TOTAL row
      (org-table-goto-line 1)
      (while (and (org-at-table-p) (not (eobp)))
        (unless (org-at-table-hline-p)
          (let ((product (string-trim (or (org-table-get nil 1) "")))
                (date (string-trim (or (org-table-get nil 2) "")))
                (count-str (string-trim (or (org-table-get nil 3) "")))
                (discount-str (string-trim (or (org-table-get nil 4) "")))
                (price-str (string-trim (or (org-table-get nil 5) ""))))
            (if (string= product "TOTAL")
                (setq total-line (org-table-current-line))
              ;; Data row (skip header by checking for valid date format)
              (when (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" date)
                ;; Track unique dates
                (puthash date t dates)
                ;; Sum counts
                (unless (string-empty-p count-str)
                  (setq total-count (+ total-count (string-to-number count-str))))
                ;; Sum discounts (parse $X.XX from "$X.XX (YY%)" format)
                (when (and discount-str (string-match "\\$\\([0-9.]+\\)" discount-str))
                  (setq total-discount (+ total-discount (string-to-number (match-string 1 discount-str)))))
                ;; Sum prices (price * count), rounded per item
                (unless (string-empty-p price-str)
                  (let ((count (if (string-empty-p count-str) 1
                                 (string-to-number count-str)))
                        (price (string-to-number price-str)))
                    (setq total-price (+ total-price
                                         (org-shop--round-price (* count price))))))))))
        (forward-line 1))
      ;; Calculate unique days
      (let ((unique-days (hash-table-count dates)))
        ;; Update or create TOTAL row
        (if total-line
            ;; Update existing TOTAL row
            (progn
              (org-table-goto-line total-line)
              (org-table-put nil 2 (number-to-string unique-days))
              (org-table-put nil 3 (number-to-string total-count))
              (org-table-put nil 4 (if (> total-discount 0)
                                       (format "$%.2f" total-discount)
                                     ""))
              (org-table-put nil 5 (format "%.2f" total-price)))
          ;; Create TOTAL row at end of table
          (goto-char table-start)
          (while (and (org-at-table-p) (not (eobp)))
            (forward-line 1))
          (forward-line -1)
          (end-of-line)
          (insert "\n|--------+------+-------+----------+-------|")
          (insert (format "\n| TOTAL | %d | %d | %s | %.2f |"
                          unique-days total-count
                          (if (> total-discount 0)
                              (format "$%.2f" total-discount)
                            "")
                          total-price))
          (insert "\n|--------+------+-------+----------+-------|"))
        (org-table-align)))))

;;;###autoload
(defun org-shop-recalculate-history ()
  "Recalculate TOTAL row in history table of current shop file.
Shows: unique days shopped, total item count, total spent."
  (interactive)
  (save-excursion
    (if (org-shop--goto-table-after-heading org-shop-history-heading)
        (progn
          (org-shop--recalculate-history-total)
          (message "History TOTAL recalculated"))
      (user-error "No history table found"))))

;;;###autoload
(defun org-shop-sync ()
  "Sync shopping list back to shop file.
For done items [X]: upserts purchase to history (product, date, count, discount, price).
For not-done items [ ]: removes from history if previously synced.
For items with new_price: updates price in shop file.
For new products (not in shop): adds them to inventory table.
Re-syncing is safe - updates existing entries instead of creating duplicates."
  (interactive)
  (unless (org-shop--at-table-p)
    (user-error "Not in an org table"))
  (let* ((date (org-shop--page-date))
         (shop-name (org-shop--resolve-shop))
         (shop-file (org-shop--find-shop-file shop-name))
         (rows (org-shop--parse-shopping-table))
         (logged 0)
         (removed 0)
         (price-updated 0)
         (products-added 0))
    (dolist (row rows)
      (let ((done (cdr (assoc "done" row)))
            (product (cdr (assoc "product" row)))
            (count (cdr (assoc "count" row)))
            (discount (cdr (assoc "discount" row)))
            (quantity (cdr (assoc "quantity" row)))
            (known-price (cdr (assoc "known_price" row)))
            (new-price (cdr (assoc "new_price" row)))
            (notes (cdr (assoc "notes" row))))
        ;; Skip Summary row
        (unless (string= product "Summary")
          ;; Determine effective price
          (let ((price (if (and new-price (not (string-empty-p new-price)))
                           new-price
                         known-price)))
            ;; Check if product exists in shop, add if new
            (unless (org-shop--product-exists-in-shop-p shop-file product)
              (org-shop--add-product-to-shop shop-file product price notes date)
              (cl-incf products-added))
            ;; Upsert to history based on done status
            (if (and done (string-match-p "X" done))
                ;; Done: upsert with count (or 1 if empty)
                (progn
                  (org-shop--upsert-history shop-file product count price date discount)
                  (cl-incf logged))
              ;; Not done: upsert with count=0 to remove from history
              (org-shop--upsert-history shop-file product "0" price date nil)
              (cl-incf removed))
            ;; Update price in shop file if we have an effective price
            ;; (updates even when new_price == known_price, since shop file might be empty)
            (when (and price (not (string-empty-p price)))
              (org-shop--update-price-in-shop shop-file product price)
              (cl-incf price-updated))
            ;; Update quantity in shop file if we have quantity
            (when (and quantity (not (string-empty-p quantity)))
              (org-shop--update-quantity-in-shop shop-file product quantity))
            ;; Update notes in shop file if we have notes
            (when (and notes (not (string-empty-p notes)))
              (org-shop--update-notes-in-shop shop-file product notes))))))
    ;; Save shop file
    (with-current-buffer (find-file-noselect shop-file)
      (save-buffer))
    ;; Recalculate summary row in shopping list
    (org-shop-recalculate)
    (message "Synced to %s: %d logged, %d removed, %d price(s) updated, %d new product(s)"
             shop-name logged removed price-updated products-added)))

;;; ============================================================================
;;; Keymap Setup
;;; ============================================================================

(defun org-shop--round-price (val)
  "Round VAL to 2 decimal places (nearest cent).
This matches how physical receipts round each line item individually."
  (/ (fround (* val 100.0)) 100.0))

(defun org-shop--parse-price (str)
  "Parse STR as a price value. Returns 0.0 for empty, \"-\", or non-numeric."
  (if (or (null str) (string-empty-p str) (string= str "-"))
      0.0
    (let ((n (string-to-number str)))
      (if (zerop n) 0.0 (float n)))))

;;;###autoload
(defun org-shop-recalculate ()
  "Recalculate the summary row in current shopping list table.
Summary known_price = total paid (effective_price * count - discounts).
Summary new_price = price diff (sum of (new - known) * count).
Formats all price cells to 2 decimal places."
  (interactive)
  (unless (org-at-table-p)
    (user-error "Not in an org table"))
  (save-excursion
    (let ((marked 0) (unmarked 0)
          (total-count 0)
          (total-discount 0.0)
          (total-paid 0.0)
          (total-diff 0.0)
          (summary-line nil)
          (data-lines (org-shop--table-data-lines)))
      ;; Pass 1: format price cells and collect stats
      (dolist (line-num data-lines)
        (org-table-goto-line line-num)
        (let ((product (or (org-shop--get-cell "product") ""))
              (done (or (org-shop--get-cell "done") ""))
              (count-str (or (org-shop--get-cell "count") ""))
              (disc-str (or (org-shop--get-cell "discount") ""))
              (known-str (or (org-shop--get-cell "known_price") ""))
              (new-str (or (org-shop--get-cell "new_price") "")))
          (if (string= product "Summary")
              (setq summary-line line-num)
            ;; Format price cells to .2f (skip "-" and empty)
            (let ((known-val (org-shop--parse-price known-str))
                  (new-val (org-shop--parse-price new-str)))
              (when (and (not (string-empty-p known-str))
                         (not (string= known-str "-"))
                         (not (zerop known-val)))
                (org-shop--set-cell "known_price" (format "%.2f" known-val)))
              (when (and (not (string-empty-p new-str))
                         (not (zerop new-val)))
                (org-shop--set-cell "new_price" (format "%.2f" new-val)))
              ;; Collect stats from data rows with checkboxes
              (when (string-match-p "\\[.\\]" done)
                (if (string-match-p "X" done)
                    (cl-incf marked)
                  (cl-incf unmarked))
                (let* ((count (if (string-empty-p count-str) 1
                                (string-to-number count-str)))
                       (effective-price (if (not (zerop new-val)) new-val known-val))
                       (disc-rate (if (string-empty-p disc-str) 0.0
                                    (string-to-number disc-str))))
                  (setq total-count (+ total-count count))
                  ;; new_price = receipt price (discount already factored in)
                  ;; known_price items: apply discount rate
                  (let* ((has-new (not (zerop new-val)))
                         ;; Round per item to match physical receipt rounding
                         (item-cost (org-shop--round-price
                                     (if has-new
                                         (* count new-val)
                                       (* count known-val (- 1.0 disc-rate)))))
                         (item-saved (org-shop--round-price
                                      (if has-new 0.0
                                        (* count known-val disc-rate)))))
                    (setq total-paid (+ total-paid item-cost))
                    ;; Dollars saved from discounts
                    (setq total-discount (+ total-discount item-saved))
                    ;; price_diff = Σ ((new - known) × count) for items with new_price
                    (when has-new
                      (setq total-diff (+ total-diff
                                          (org-shop--round-price
                                           (* count (- new-val known-val)))))))))))))
      ;; Update summary row
      (when summary-line
        (org-table-goto-line summary-line)
        (org-shop--set-cell "done" (format "%dU %dM" unmarked marked))
        (org-shop--set-cell "count" (if (zerop total-count) "" (number-to-string total-count)))
        (org-shop--set-cell "discount" (if (zerop total-discount) "" (format "%.2f" total-discount)))
        (org-shop--set-cell "known_price" (format "%.2f" total-paid))
        (org-shop--set-cell "new_price" (format "%s%.2f" (if (>= total-diff 0) "+" "") total-diff))
        (org-table-align)))))

;;;###autoload
(defun org-shop-recalculate-buffer ()
  "Recalculate the Summary row of every shop table in the current buffer.
Identifies shop tables by the presence of both `done' and `known_price'
columns in the header. Useful after pasting several staged receipt
blocks under different `*** [[id:...][Shop]]' headings in a daily file."
  (interactive)
  (let ((count 0)
        (errors 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*|" nil t)
        (when (org-at-table-p)
          (save-excursion
            (when (and (ignore-errors (org-table-goto-line 1) t)
                       (org-shop--has-column-p "done")
                       (org-shop--has-column-p "known_price"))
              (condition-case err
                  (progn (org-shop-recalculate) (cl-incf count))
                (error
                 (cl-incf errors)
                 (message "org-shop-recalculate-buffer: skipped table at line %d (%s)"
                          (line-number-at-pos)
                          (error-message-string err))))))
          (goto-char (org-table-end)))))
    (message "org-shop: recalculated %d shop table%s%s"
             count
             (if (= count 1) "" "s")
             (if (zerop errors) "" (format " (%d skipped)" errors)))))

;;; ============================================================================
;;; Product Completion Functions
;;; ============================================================================

(defun org-shop--current-column-name ()
  "Return the column name at point, or nil if not in a table cell."
  (when (org-at-table-p)
    (let* ((col-num (org-table-current-column))
           (columns (org-shop--get-table-columns)))
      (when (and col-num (> col-num 0) (<= col-num (length columns)))
        (nth (1- col-num) columns)))))

(defun org-shop--in-shopping-table-product-column-p ()
  "Return non-nil if point is in the product column of a shopping table.
Shopping tables are identified by having `done' and `known_price' columns."
  (and (org-at-table-p)
       (not (org-at-table-hline-p))
       (org-shop--has-column-p "done")
       (org-shop--has-column-p "known_price")
       (string-equal-ignore-case (or (org-shop--current-column-name) "") "product")))

(defun org-shop--get-all-products-for-completion ()
  "Get all products from the associated shop file for completion.
Returns list of product names.  Caches product data for auto-fill."
  (let* ((shop-name (org-shop--resolve-shop))
         (shop-file (when shop-name (org-shop--find-shop-file shop-name))))
    (when shop-file
      ;; Check cache validity
      (let ((cached (gethash shop-file org-shop--product-cache))
            (mod-time (file-attribute-modification-time (file-attributes shop-file))))
        (if (and cached (equal (car cached) mod-time))
            (cdr cached)  ; Return cached products
          ;; Rebuild cache
          (let ((products '())
                (data-map (make-hash-table :test 'equal)))
            (with-temp-buffer
              (insert-file-contents shop-file)
              (org-mode)
              (when (org-shop--goto-table-after-heading org-shop-source-heading)
                (let ((rows (org-shop--parse-table)))
                  (dolist (row rows)
                    (let ((product (cdr (assoc "product" row))))
                      ;; Exclude TOTAL, Summary, and empty products
                      (when (and product
                                 (not (string-empty-p product))
                                 (not (string-equal-ignore-case product "TOTAL"))
                                 (not (string-equal-ignore-case product "Summary")))
                        (push product products)
                        (puthash (downcase product) row data-map)))))))
            (puthash shop-file (cons mod-time (nreverse products)) org-shop--product-cache)
            (puthash shop-file data-map org-shop--product-data-cache)
            (cdr (gethash shop-file org-shop--product-cache))))))))

(defun org-shop--completion-annotate (candidate)
  "Return annotation for product CANDIDATE showing price and quantity."
  (when org-shop-complete-show-annotations
    (let* ((shop-name (org-shop--resolve-shop))
           (shop-file (when shop-name (org-shop--find-shop-file shop-name)))
           (data-map (when shop-file (gethash shop-file org-shop--product-data-cache)))
           (row (when data-map (gethash (downcase candidate) data-map))))
      (when row
        (let ((price (cdr (assoc "price" row)))
              (qty (cdr (assoc "quantity" row))))
          (format " %s%s"
                  (if (and price (not (string-empty-p price))) (format "$%s" price) "")
                  (if (and qty (not (string-empty-p qty))) (format " %s" qty) "")))))))

(defun org-shop--completion-exit-function (candidate status)
  "Auto-fill row with product data after completion.
CANDIDATE is the selected product, STATUS indicates completion type."
  (when (and org-shop-complete-auto-fill
             (eq status 'finished))
    (let* ((shop-name (org-shop--resolve-shop))
           (shop-file (when shop-name (org-shop--find-shop-file shop-name)))
           (data-map (when shop-file (gethash shop-file org-shop--product-data-cache)))
           (row (when data-map (gethash (downcase candidate) data-map))))
      (when row
        (let ((price (cdr (assoc "price" row)))
              (qty (cdr (assoc "quantity" row)))
              (notes (cdr (assoc "notes" row))))
          ;; Fill known_price from price
          (when (and price (not (string-empty-p price)))
            (org-shop--set-cell "known_price" price))
          ;; Fill quantity
          (when (and qty (not (string-empty-p qty)))
            (org-shop--set-cell "quantity" qty))
          ;; Fill notes
          (when (and notes (not (string-empty-p notes)))
            (org-shop--set-cell "notes" notes)))))))

(defun org-shop-complete-capf ()
  "Completion-at-point function for org-shop product completion."
  (when (org-shop--in-shopping-table-product-column-p)
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (or (car bounds) (point)))
           (end (or (cdr bounds) (point)))
           (candidates (org-shop--get-all-products-for-completion)))
      (when candidates
        (list start end candidates
              :exclusive 'no
              :annotation-function #'org-shop--completion-annotate
              :exit-function #'org-shop--completion-exit-function)))))

;;;###autoload
(define-minor-mode org-shop-complete-mode
  "Minor mode for fuzzy autocompletion of products in org-shop tables.
When enabled, typing in the product column of a shopping table
provides completion against all products in the associated shop file."
  :lighter " ShopC"
  :group 'org-shop
  (if org-shop-complete-mode
      (progn
        (add-hook 'completion-at-point-functions #'org-shop-complete-capf nil t)
        (setq-local completion-ignore-case t))
    (remove-hook 'completion-at-point-functions #'org-shop-complete-capf t)))

;;; ============================================================================
;;; Add Item Command
;;; ============================================================================

(defun org-shop--insert-item-row (product count discount quantity notes known-price new-price)
  "Insert a new item row before the summary line.
PRODUCT, COUNT, DISCOUNT, QUANTITY, NOTES, KNOWN-PRICE, and NEW-PRICE
are cell values.  Handles table formatting automatically."
  (save-excursion
    ;; Find the summary row
    (let ((summary-line nil))
      (dolist (line-num (org-shop--table-data-lines))
        (org-table-goto-line line-num)
        (when (string= (org-shop--get-cell "product") "Summary")
          (setq summary-line line-num)))
      (if summary-line
          (progn
            ;; Go to summary line and insert row above the hline
            (org-table-goto-line summary-line)
            (beginning-of-line)
            (forward-line -1)  ; Move to hline before summary
            ;; Insert new row BEFORE the hline (so hline stays before Summary)
            ;; Columns: product | done | count | discount | quantity | notes | known_price | new_price
            (insert (format "| %s | [X] | %s | %s | %s | %s | %s | %s |\n"
                            product
                            (if (string-empty-p count) "" count)
                            (or discount "")
                            (or quantity "")
                            (or notes "")
                            (if (and known-price (not (string-empty-p known-price)))
                                (format "%.2f" (string-to-number known-price))
                              "-")
                            (if (and new-price (not (string-empty-p new-price)))
                                (format "%.2f" (string-to-number new-price))
                              "")))
            (org-table-align))
        (user-error "No Summary row found in table")))))

;;;###autoload
(defun org-shop-add-item ()
  "Add a new item to the shopping table via minibuffer prompts.
Prompts for product (with fuzzy completion), count, discount, quantity,
notes, and prices.  Inserts row before the summary line and recalculates totals."
  (interactive)
  (unless (org-at-table-p)
    (user-error "Not in an org table"))
  (unless (org-shop--has-column-p "done")
    (user-error "Not in a shopping table (missing 'done' column)"))
  (let* ((candidates (org-shop--get-all-products-for-completion))
         ;; Product prompt with fuzzy completion
         (product (completing-read "Product: " candidates nil nil))
         ;; Look up existing data for auto-fill defaults
         (shop-name (org-shop--resolve-shop))
         (shop-file (when shop-name (org-shop--find-shop-file shop-name)))
         (data-map (when shop-file (gethash shop-file org-shop--product-data-cache)))
         (existing (when data-map (gethash (downcase product) data-map)))
         ;; Extract defaults from existing product data
         (default-price (when existing (cdr (assoc "price" existing))))
         (default-qty (when existing (cdr (assoc "quantity" existing))))
         (default-notes (when existing (cdr (assoc "notes" existing))))
         ;; Prompt for remaining fields with defaults
         (count (read-string "Count [1]: " nil nil "1"))
         (discount (read-string "Discount (e.g., 0.3 for 30% off): "))
         (quantity (read-string (format "Quantity%s: "
                                        (if default-qty (format " [%s]" default-qty) ""))
                                nil nil default-qty))
         (notes (read-string (format "Notes%s: "
                                     (if default-notes (format " [%s]" default-notes) ""))
                             nil nil default-notes))
         (known-price (read-string (format "Known price%s: "
                                           (if default-price (format " [%s]" default-price) ""))
                                   nil nil default-price))
         (new-price (read-string "New price (leave empty if same): ")))
    ;; Find summary line and insert before it
    (org-shop--insert-item-row product count discount quantity notes known-price new-price)
    ;; Recalculate summary
    (org-shop-recalculate)
    (message "Added: %s" product)))

(defun org-shop--get-previous-purchase-date (shop-file product current-date)
  "Get the most recent purchase date for PRODUCT before CURRENT-DATE.
Returns the date string or nil if no previous purchase exists."
  (with-current-buffer (find-file-noselect shop-file)
    (save-excursion
      (goto-char (point-min))
      (when (org-shop--goto-table-after-heading org-shop-history-heading)
        (let ((dates '()))
          ;; Collect all dates for this product
          (while (and (org-at-table-p) (not (eobp)))
            (unless (org-at-table-hline-p)
              (let ((row-product (string-trim (or (org-table-get nil 1) "")))
                    (row-date (string-trim (or (org-table-get nil 2) ""))))
                (when (and (string-equal-ignore-case row-product product)
                           (not (string-equal row-date current-date))
                           (not (string-equal-ignore-case row-product "TOTAL"))
                           (not (string-empty-p row-date)))
                  (push row-date dates))))
            (forward-line 1))
          ;; Return the most recent date (dates are in various order, sort descending)
          (when dates
            (car (sort dates #'string>))))))))

(defun org-shop--delete-history-entry (shop-file product date)
  "Delete the history entry for PRODUCT on DATE from SHOP-FILE."
  (with-current-buffer (find-file-noselect shop-file)
    (save-excursion
      (goto-char (point-min))
      (when (org-shop--goto-table-after-heading org-shop-history-heading)
        (let ((found nil))
          (while (and (not found) (org-at-table-p) (not (eobp)))
            (unless (org-at-table-hline-p)
              (let ((row-product (string-trim (or (org-table-get nil 1) "")))
                    (row-date (string-trim (or (org-table-get nil 2) ""))))
                (when (and (string-equal-ignore-case row-product product)
                           (string-equal row-date date))
                  (org-shop--delete-table-row)
                  (setq found t))))
            (unless found (forward-line 1)))
          (when found
            (org-shop--recalculate-history-total)
            (save-buffer))
          found)))))

(defun org-shop--update-last-bought (shop-file product new-date)
  "Update the last_bought column for PRODUCT in SHOP-FILE to NEW-DATE.
If NEW-DATE is nil, delete the product from the regular table."
  (with-current-buffer (find-file-noselect shop-file)
    (save-excursion
      (goto-char (point-min))
      (when (org-shop--goto-table-after-heading org-shop-source-heading)
        (let ((found nil))
          (while (and (not found) (org-at-table-p) (not (eobp)))
            (unless (org-at-table-hline-p)
              (let ((row-product (string-trim (or (org-shop--get-cell "product") ""))))
                (when (string-equal-ignore-case row-product product)
                  (if new-date
                      ;; Update last_bought to new date
                      (progn
                        (org-shop--set-cell "last_bought" new-date)
                        (setq found t))
                    ;; Delete the product row entirely
                    (org-shop--delete-table-row)
                    (setq found t)))))
            (unless found (forward-line 1)))
          (when found
            (save-buffer))
          found)))))

;;;###autoload
(defun org-shop-delete-item ()
  "Delete the current item from the shopping table.
Also removes the corresponding history entry and restores the previous
last_bought date.  If no previous purchase exists, deletes the product
from the shop's regular table entirely."
  (interactive)
  (unless (org-at-table-p)
    (user-error "Not in an org table"))
  (when (org-at-table-hline-p)
    (user-error "Cannot delete a separator line"))
  (let ((product (org-shop--get-cell "product")))
    (when (or (not product)
              (string-empty-p product)
              (string-equal-ignore-case product "Summary"))
      (user-error "Cannot delete this row"))
    (when (yes-or-no-p (format "Delete '%s' and remove from history? " product))
      (let* ((date (org-shop--page-date))
             (shop-name (org-shop--resolve-shop))
             (shop-file (when shop-name (org-shop--find-shop-file shop-name))))
        (when shop-file
          ;; Get previous purchase date before deleting history
          (let ((prev-date (org-shop--get-previous-purchase-date shop-file product date)))
            ;; Delete history entry
            (org-shop--delete-history-entry shop-file product date)
            ;; Update or delete from regular table
            (org-shop--update-last-bought shop-file product prev-date)
            (if prev-date
                (message "Deleted '%s', restored last_bought to %s" product prev-date)
              (message "Deleted '%s' from history and regular table" product))))
        ;; Delete the row from current shopping table
        (org-shop--delete-table-row)
        ;; Recalculate summary
        (org-shop-recalculate)))))

(defvar org-shop-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") #'org-shop-mark)
    (define-key map (kbd "g") #'org-shop-generate)
    (define-key map (kbd "s") #'org-shop-sync)
    (define-key map (kbd "r") #'org-shop-recalculate)
    (define-key map (kbd "R") #'org-shop-recalculate-buffer)
    (define-key map (kbd "c") #'org-shop-clear-marks)
    (define-key map (kbd "t") #'org-shop-complete-mode)
    (define-key map (kbd "a") #'org-shop-add-item)
    (define-key map (kbd "d") #'org-shop-delete-item)
    map)
  "Command map for org-shop.
\\{org-shop-command-map}")

;;;###autoload
(defun org-shop-setup ()
  "Setup org-shop with default keybindings.
Binds commands under `org-shop-keymap-prefix' (default C-c S):
  <prefix> m - Toggle mark (next in shop, done in daily)
  <prefix> g - Generate shopping list
  <prefix> s - Sync to shop (also recalculates summary)
  <prefix> r - Recalculate Summary row of table at point
  <prefix> R - Recalculate Summary in every shop table in buffer
  <prefix> c - Clear all marks
  <prefix> t - Toggle product completion mode
  <prefix> a - Add item via minibuffer prompts
  <prefix> d - Delete item and remove from history"
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
