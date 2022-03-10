(* Copyright (C) 2017--2022  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

type t = int

(* Representation *)

(* TODO: Need a compile-time constant for efficient switch to 56 bit width for
 * 64 bit native compilation. *)
let length_width = 5
let max_length = 24
let floor_log2_halfwidth = 16

let bit0 = 1 lsl length_width
let length_mask = bit0 - 1
let bitmask = (1 lsl max_length - 1) lsl length_width

(* Helpers *)

(* Lifted from prime_int.ml. *)
let rec floor_log2_loop j n l =
  if j = 0 then l else
  if n lsr j = 0
    then floor_log2_loop (j / 2) n l
    else floor_log2_loop (j / 2) (n lsr j) (l + j)
let floor_log2 n = floor_log2_loop floor_log2_halfwidth n 0

(* Inspection and Slicing *)

let length p = p land length_mask

let bits p = p lsr length_width

(* Comparison *)

let equal (v : int) (w : int) = v = w

let compare p q =
  let m, n = length p, length q in
  let p' = p lsl (max_length - m) land bitmask in
  let q' = q lsl (max_length - n) land bitmask in
  if p' < q' then -1 else
  if p' > q' then  1 else
  if m  < n then -1 else if n > m then 1 else 0

let common_prefix_length p q =
  let m, n = length p, length q in
  if m < n
  then length_width - 1 + m - floor_log2 (p lxor q lsr (n - m) lor length_mask)
  else length_width - 1 + n - floor_log2 (q lxor p lsr (m - n) lor length_mask)

(* Construction *)

let is_empty p = p = 0
let is_full p = length p = max_length

let empty = 0
let c0 = 1
let c1 = 1 lor bit0
let c00 = 2
let c01 = 2 lor bit0
let c10 = 2 lor 2 lsl length_width
let c11 = 2 lor 3 lsl length_width

let make_unsafe n x = x lsl length_width lor n
let make n x =
  if n < 0 || n > max_length || x lsr n <> 0 then invalid_arg "Bitword.make";
  x lsl length_width lor n

let const_unsafe n x = (if x then (1 lsl n - 1) lsl length_width else 0) lor n
let const n x =
  if n < 0 || n > max_length then invalid_arg "Bitword.const" else
  const_unsafe n x

let random_uniform n =
  if n < 0 || n > max_length then invalid_arg "Bitword.random_uniform";
  let rec loop m acc =
    if m <= 0 then acc else
    loop (m - 30) (acc lsl 30 lor Random.bits ()) in
  (loop n 0 lsl length_width) land (1 lsl n - 1) lor n

(* Endian-Dependent Operations *)

module type ENDIAN_API = sig
  val init : int -> (int -> bool) -> t
  val get : t -> int -> bool

  val prefix : int -> t -> t
  val prefix_unsafe : int -> t -> t
  val suffix : int -> t -> t
  val suffix_unsafe : int -> t -> t

  val cut : int -> t -> t * t

  val push_exn : bool -> t -> t
  val push_c0_exn : t -> t
  val push_c0_unsafe : t -> t
  val push_c1_exn : t -> t
  val push_c1_unsafe : t -> t
  val drop_exn : t -> t
  val drop_unsafe : t -> t
  val pop_exn : t -> bool * t
  val pop_unsafe : t -> bool * t
end

module Be = struct

  let init n f =
    let rec loop i acc =
      if i = n then acc lsl length_width lor n else
      loop (i + 1) (acc lsl 1 lor (if f i then 1 else 0)) in
    if n < 0 || n > max_length then invalid_arg "Bitword.Be.init" else
    loop 0 0

  let get p i = p lsr (length p - 1 - i) land bit0 <> 0

  let prefix_unsafe k p =
    let n = length p in
    (p lsr (n - k) land bitmask) lor k

  let prefix k p =
    let n = length p in
    if k < 0 || k > n then invalid_arg "Bitword.Be.prefix" else
    (p lsr (n - k) land bitmask) lor k

  let suffix_unsafe k p =
    let n = length p in
    p land (1 lsl (n - k) - 1) lsl length_width lor (n - k)

  let suffix k p =
    let n = length p in
    if k < 0 || k > n then invalid_arg "Bitword.Be.suffix" else
    p land (1 lsl (n - k) - 1) lsl length_width lor (n - k)

  let cut k p =
    let n = length p in
    if k < 0 || k > n then invalid_arg "Bitword.Be.cut" else
    (prefix_unsafe k p, suffix_unsafe k p)

  let push_exn x p =
    let n = length p in
    if n >= max_length then invalid_arg "Bitword.Be.push_exn" else
    p lor (if x then bit0 lsl n else 0) + 1

  let push_c0_unsafe p = p + 1
  let push_c0_exn p =
    if length p >= max_length then invalid_arg "Bitword.Be.push_c0_exn" else
    push_c0_unsafe p

  let push_c1_unsafe p = p lor bit0 lsl length p + 1
  let push_c1_exn p =
    if length p >= max_length then invalid_arg "Bitword.Be.push_c1_exn" else
    push_c1_unsafe p

  let drop_unsafe p =
    let n = length p in
    p land (bit0 lsl (n - 1) - 1) - 1

  let drop_exn p =
    let n = length p in
    if n = 0 then invalid_arg "Bitword.Be.drop_exn" else
    p land (bit0 lsl (n - 1) - 1) - 1

  let pop_unsafe p =
    let n = length p in
    let bit = bit0 lsl (n - 1) in
    (p land bit <> 0, p land (lnot bit) - 1)

  let pop_exn p =
    let n = length p in
    if n = 0 then invalid_arg "Bitword.Be.pop_exn" else
    let bit = bit0 lsl (n - 1) in
    (p land bit <> 0, p land (lnot bit) - 1)
end

module Le = struct

  let init n f =
    let rec loop i acc =
      if i = n then acc lsl length_width lor n else
      loop (i + 1) (if f i then acc lor 1 lsl i else acc) in
    if n < 0 || n > max_length then invalid_arg "Bitword.Le.init" else
    loop 0 0

  let get p i = p lsr i land bit0 <> 0

  let prefix_unsafe k p =
    let n = length p in
    p lsr k land bitmask lor (n - k)

  let prefix k p =
    let n = length p in
    if k < 0 || k > n then invalid_arg "Bitword.Le.prefix" else
    p lsr k land bitmask lor (n - k)

  let suffix_unsafe k p =
    p land (1 lsl k - 1) lsl length_width lor k

  let suffix k p =
    let n = length p in
    if k < 0 || k > n then invalid_arg "Bitword.Le.suffix" else
    suffix_unsafe k p

  let cut k p =
    let n = length p in
    if k < 0 || k > n then invalid_arg "Bitword.Le.cut" else
    (p lsr k land bitmask lor (n - k), suffix_unsafe k p)

  let push_exn x p =
    let n = length p in
    if n >= max_length then invalid_arg "Bitword.Le.push_exn" else
    (p land bitmask) lsl 1 lor (if x then bit0 else 0) lor (n + 1)

  let push_c0_unsafe p = (p land bitmask) lsl 1 lor (length p + 1)
  let push_c0_exn p =
    if length p >= max_length then invalid_arg "Bitword.Le.push_c0_exn" else
    push_c0_unsafe p

  let push_c1_unsafe p = (p land bitmask) lsl 1 lor bit0 lor (length p + 1)
  let push_c1_exn p =
    if length p >= max_length then invalid_arg "Bitword.Le.push_c1_exn" else
    push_c1_unsafe p

  let drop_unsafe p = p lsr 1 land bitmask lor (length p - 1)
  let drop_exn p =
    if length p = 0 then invalid_arg "Bitword.Le.drop_exn" else
    drop_unsafe p

  let pop_unsafe p = (p land bit0 <> 0, drop_unsafe p)
  let pop_exn p =
    if length p = 0 then invalid_arg "Bitword.Le.pop_exn" else
    pop_unsafe p
end

(* Slice and Concatenate *)

let cat_exn p q =
  let m = length p in
  let n = length q in
  if n + m > max_length then invalid_arg "Bitword.cat_exn" else
  (p land bitmask) lsl n lor q + m

let cat_rem p q =
  let m = length p in
  let n = length q in
  let k = min m (max_length - n) in
  let p' = (p lsr k land bitmask) + (m - k) in
  let q' = ((p land bitmask) lsl n land bitmask) lor q + k in
  (p', q')

(* Conversion and Pretty-Printing *)

let pp_set fmtr w =
  let rec loop i u =
    if u <> 0 then begin
      if u land 1 <> 0 then begin
        Format.pp_print_int fmtr i;
        Format.pp_print_string fmtr ", "
      end;
      loop (i + 1) (u lsr 1)
    end in
  Format.pp_print_char fmtr '{';
  loop 0 (w lsr length_width);
  Format.pp_print_char fmtr '}'

let to_base2_string p =
  let n = length p in
  String.init n (fun i -> if Le.get p (n - i - 1) then '1' else '0')

let pp_base2 fmtr p = Format.pp_print_string fmtr (to_base2_string p)

let of_base2_string_exn s =
  let n = String.length s in
  if n > max_length then invalid_arg "Bitword.of_base2_string_exn";
  Le.init n
    (fun i ->
      (match s.[i] with
       | '0' -> false
       | '1' -> true
       | _ -> invalid_arg "Bitword.of_base2_string_exn"))
