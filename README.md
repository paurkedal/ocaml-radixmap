# radixmap - Map à la Binary Radix Tree

This OCaml library provides a pure map-like data structure which efficiently
represent piecewise constant functions over domains which can be binary
subdivided.  Suitable domains include bit strings, byte strings, and IP
addresses, in particular where the maps tend to be piecewise constant over
prefixes possibly with nested exceptional sub-pieces.
