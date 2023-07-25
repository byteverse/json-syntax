# Revision history for json-syntax

## 0.2.5.0 -- 2023-07-25

* Add `object(13|14|15|16|17)`.
* Add `ToValue` instances for `Word`, `Text`, `Value`,
  `Scientific`, list (i.e. `[]`), the unit type (i.e. `()`),
* Add `text` and `shortText` for value construction.
  

## 0.2.4.0 -- 2023-06-27

* Add typeclass `ToValue` for encoding.
* Add functions `int`, `(int|word)(8|16|32|64)`, `bool` for constructing
  objects.

## 0.2.3.0 -- 2022-03-22

* Add `Json.Flatten` module.
* Drop support for GHCs older than 9.0.
* Replace integer-gmp with ghc-bignum.

## 0.2.2.0 -- 2022-07-15

* Build with GHC 9.2.3.
* Test suite now requires aeson >= 2.0 instead of < 2.0.

## 0.2.1.0 -- 2022-03-01

* Support Jackson's SMILE format as an encode target.
* Use `bytebuild`'s `rebuild` function for 2x perf improvement on encode.
* Bump bytebuild for buffer overflow fix.

## 0.2.0.0 -- 2021-03-22

* Switch from `Chunks` to `SmallArray` in the `Object` and `Array` data
  constructors. This makes the library simpler to use but it a breaking
  change.
* Expose `emptyArray` and `emptyObject`.
* Add `object(9|10|11|12)` as convenience helpers for construction.

## 0.1.2.0 -- 2020-11-18

* Add infix pattern synonym for `Member`.
* Add `object(1|2|3|4|5|6|7|8)` as convenience helpers for construction.

## 0.1.1.0 -- 2020-05-01

* Add `encode`.

## 0.1.0.0 -- 2020-01-20

* Initial release.
