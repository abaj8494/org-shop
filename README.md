# org-shop

Org-table based shopping list management for Emacs.

## Overview

org-shop bridges shop inventory files with daily org files. Mark items in a shop file, generate a shopping table in your daily file, fill in prices at the store, sync back.

## Workflow

```
1. In aldi.org, mark items           C-c S m
2. In 2026-02-12.org, generate       C-c S g
3. At store, mark done + prices      C-c S m
4. Back home, sync                   C-c S s
```

## Keybindings

| Key | Command | Description |
|-----|---------|-------------|
| `C-c S m` | `org-shop-mark` | Toggle mark (next/done depending on table) |
| `C-c S g` | `org-shop-generate` | Generate shopping table from marked items |
| `C-c S s` | `org-shop-sync` | Sync prices and history back to shop file |
| `C-c S c` | `org-shop-clear-marks` | Clear all marks in current shop file |

---

<details>
<summary><strong>Shop File Structure</strong></summary>

Shop files live in `org-shop-directory` (default `~/org/shops/`). Each shop has a `regular` heading with an inventory table:

```org
* offerings
** regular

|-------+--------------------+-------+----------+--------------------------------------+-------------|
| next  | product            | price | quantity | notes                                | last_bought |
|-------+--------------------+-------+----------+--------------------------------------+-------------|
| [X]   | Frozen Prawn       | 12.99 | 500g     |                                      |  2026-02-12 |
| [X]   | Middle Bacon       |  3.79 | 250g     |                                      |  2026-02-12 |
| [ ]   | Cheese Slice Tasty |  5.49 | 250g     | the 500g pack is only 2 dollars more |  2026-02-12 |
| [ ]   | Baby Gem Lettuce   |  3.99 | 3pk      |                                      |  2026-02-12 |
|-------+--------------------+-------+----------+--------------------------------------+-------------|
```

The `[X]` in the `next` column indicates items marked for the next shopping trip.

</details>

<details>
<summary><strong>Generated Table</strong></summary>

Running `C-c S g` under a shop heading (e.g. `*** Aldi`) produces:

```org
*** Aldi

|--------------------+--------+--------+----------+----------+--------------------------------------+-------------+-----------|
| product            | done   |  count | discount | quantity | notes                                | known_price | new_price |
|--------------------+--------+--------+----------+----------+--------------------------------------+-------------+-----------|
| Frozen Prawn       | [X]    |      1 |          | 500g     |                                      |       12.99 |           |
| Middle Bacon       | [X]    |      1 |          | 250g     |                                      |        3.79 |           |
| Banana Cav         | [X]    |  1.259 |          | /kg      |                                      |        4.49 |           |
| Blackberries       | [X]    |      1 |          | 170g     |                                      |        4.49 |           |
|--------------------+--------+--------+----------+----------+--------------------------------------+-------------+-----------|
| Summary            | 0U 15M | 16.738 |          |          | -                                    |       92.34 |    +88.85 |
|--------------------+--------+--------+----------+----------+--------------------------------------+-------------+-----------|
```

**Columns:**
- `done` - toggle with `C-c S m`
- `count` - quantity purchased (for per-kg items, enter weight)
- `discount` - decimal discount rate (0.3 = 30% off)
- `known_price` - pulled from shop file
- `new_price` - actual price paid (enter if different)

The summary row shows unmarked/marked counts, totals, and price differences.

</details>

<details>
<summary><strong>Sync</strong></summary>

`C-c S s` does three things:

1. Updates `price` in the shop file if `new_price` differs
2. Appends to `*** history` table with date, count, price
3. Recalculates summary row

History tables are sorted by date descending with dividers between shopping trips.

</details>

<details>
<summary><strong>Seasonal Produce</strong></summary>

If `org-shop-seasons-file` points to a seasons file, `C-c S g` inserts a seasonal produce checklist. Only appears for shops with the `grocery` filetag.

```org
**** [[id:57B44173-...][seasonal]] fruit + veg                          :summer:

|------+--------------+------+--------------|
| done | fruit        | done | vegetable    |
|------+--------------+------+--------------|
| [ ]  | Banana       | [ ]  | Carrot       |
| [ ]  | Lemon        | [ ]  | Onion        |
|------+--------------+------+--------------|
| [ ]  | Apricot      | [ ]  | Beans        |
| [ ]  | Cherry       | [ ]  | Capsicum     |
| [ ]  | Mango        | [ ]  | Corn         |
|------+--------------+------+--------------|
```

- Season tag (`:summer:`, `:winter:`, etc.) from file date
- ID link to seasons.org in heading
- Horizontal separator between year-round and seasonal items

**seasons.org structure:**

```org
:PROPERTIES:
:ID: 57B44173-8479-4B84-BFE8-828A91901701
:END:
#+filetags: :shopping:grocery:

* Year-Round
** Fruit
*** Banana
** Vegetables
*** Carrot

* Summer (Dec-Feb)
** Fruit
*** Mango
** Vegetables
*** Tomato
```

**Shop file requirement:**

Add `#+filetags: :grocery:` to shop files that sell produce. Shops without this tag (e.g. `bunnings.org`) won't show seasonal tables.

</details>

---

## Configuration

```elisp
(require 'org-shop)

(setq org-shop-directory "~/org/shops/")
(setq org-shop-seasons-file "~/org/shops/seasons.org")  ; optional

(org-shop-setup)
```

<details>
<summary><strong>All Variables</strong></summary>

| Variable | Default | Description |
|----------|---------|-------------|
| `org-shop-directory` | `~/org/shops/` | Directory containing shop files |
| `org-shop-source-heading` | `"regular"` | Heading with inventory table |
| `org-shop-history-heading` | `"history"` | Heading for purchase history |
| `org-shop-detect-shop-from-heading` | `t` | Infer shop name from org heading |
| `org-shop-clear-marks-after-generate` | `t` | Clear marks in shop file after generate |
| `org-shop-seasons-file` | `nil` | Path to seasons.org (nil=disabled) |
| `org-shop-seasonal-heading` | `"seasonal fruit + veg"` | Subheading name for seasonal table |
| `org-shop-seasonal-required-tag` | `"grocery"` | Filetag required for seasonal table (nil=all shops) |
| `org-shop-keymap-prefix` | `"C-c S"` | Prefix for keybindings |

</details>

## Requirements

- Emacs 27.1+
- org-mode 9.0+

## License

GPL-3.0
