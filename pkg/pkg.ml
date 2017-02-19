#! /usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let licenses = List.map Pkg.std_file ["COPYING.LESSER"; "COPYING"]

let () = Pkg.describe ~licenses "radixmap" @@ fun c ->
  Ok [
    Pkg.mllib "lib/radixmap.mllib";
    Pkg.test "tests/test_bitword";
  ]
