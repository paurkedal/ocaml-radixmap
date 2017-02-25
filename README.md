# radixmap - Map à la Binary Radix Tree

This OCaml library provides a pure map-like data structure which efficiently
represent piecewise constant functions over domains which can be binary
subdivided.  Suitable domains include bit strings, byte strings, and IP
addresses.  It is most efficient where the maps tend to be piecewise
constant over prefixes of the key type, with optional nested exceptions
which themselves have the same structure.

This package has an optional dependency on
[ipaddr](https://github.com/mirage/ocaml-ipaddr) and provides a related set
implementation `Ip_radixset` if present.

The core map, `Bitword_radixmap`, works on finite size `Bitword` sub-paths,
whereas longer paths are encoded in the tree itself.  This makes the maps
compact for common cases.  A higher level map or set can be implemented in
terms of `Bitword_radixmap` if keys may exceed 24 bits.
