#! /usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let with_ipaddr = Conf.with_pkg "ipaddr"

let licenses = List.map Pkg.std_file ["COPYING.LESSER"; "COPYING"]

let () = Pkg.describe ~licenses "radixmap" @@ fun c ->
  Ok [
    Pkg.mllib "lib/radixmap.mllib";
    Pkg.mllib ~cond:(Conf.value c with_ipaddr) "lib/radixmap-ipaddr.mllib";
    Pkg.test "tests/test_bitword";
    Pkg.test "tests/test_bitword_radixmap";
    Pkg.test "tests/test_ip_radixset";
  ]
