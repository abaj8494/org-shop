# org-shop

Emacs package for managing shopping lists using org-tables.

## Running Tests

```sh
/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs --batch -l org -l org-shop.el -l test-org-shop.el -f ert-run-tests-batch-and-exit
```

## Architecture

Single-file Emacs Lisp package (`org-shop.el`) with tests in `test-org-shop.el`.

### Key concepts

- **Shop files**: Org files in `org-shop-directory` (default `~/org/shops/`) with an `* inventory` heading containing an org-table of products (columns: next, product, price, quantity, last_bought). May also have a `* next shop` heading with ad-hoc checklist items.
- **Daily/destination files**: Where the user invokes `C-c S g` to generate a shopping table from marked items.
- **Generate flow** (`org-shop-generate`): Reads marked rows from the shop file's inventory table + "next shop" checklist items, inserts a shopping table at point, optionally inserts seasonal produce, then clears marks/items in the source shop file.
- **Sync flow** (`org-shop-sync`): After shopping, syncs new prices from the destination table back to the shop file and records history.

### Internal conventions

- Public commands: `org-shop-*` (no double dash)
- Internal functions: `org-shop--*` (double dash)
- Shop file operations use `(with-current-buffer (find-file-noselect shop-file) ...)` pattern
- Table cell access via `org-shop--get-cell`/`org-shop--set-cell` using column names
- Customization variables are in the `org-shop` group

## Example shop file

See `/Users/aayushbajaj/Documents/new-site/content-org/private/shops/aldi.org` for a real example with both `* next shop` checklist and `* inventory` table.
