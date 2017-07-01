#! /usr/bin/env ocaml
#use "topfind"
#require "topkg-jbuilder"

open Topkg

let licenses = List.map Pkg.std_file ["COPYING.LESSER"; "COPYING"]

let () = Topkg_jbuilder.describe ~licenses ()
