## 2.3.1

- Allow unboxed `Atomic_array` on 5.3 (@polytypic)
- Support js_of_ocaml (@polytypic)

## 2.3.0

- Add `copy_as ~padded` for convenient optional padding (@polytypic)
- Add `multicore-magic-dscheck` package and library to help testing with DScheck
  (@lyrm, review @polytypic)

## 2.2.0

- Add (unboxed) `Atomic_array` (@polytypic)

## 2.1.0

- Added `instantaneous_domain_index` for the implementation of contention
  avoiding data structures. (@polytypic)
- Added `Transparent_atomic` module as a workaround to CSE issues in OCaml 5.0
  and OCaml 5.1 and also to allow more efficient arrays of atomics. (@polytypic)
- Fixed `fenceless_get` to not be subject to CSE. (@polytypic)

## 2.0.0

- Changed the semantics of `copy_as_padded` to not always copy and to not
  guarantee that `length_of_padded_array*` works with it. These semantic changes
  allow better use of the OCaml allocator to guarantee cache friendly alignment.
  (@polytypic)

## 1.0.1

- Ported the library to OCaml 4 (@polytypic)
- License changed to ISC from 0BSD (@tarides)

## 1.0.0

- Initial release (@polytypic)
