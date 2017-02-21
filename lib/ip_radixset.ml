(* Copyright (C) 2017  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

module type BASE = sig
  type t
  type address
  type network

  val is_full : t -> bool
  val of_network : network -> t
  val contains_address : t -> address -> bool
  val contains_network : t -> network -> bool
  val make_index : int -> Bytes.t -> network
end

module Bool_equal = struct
  type t = bool
  let equal (x : t) (y : t) = x = y
end

module M = Bitword_radixmap.Make (Bool_equal)
open M

module V4_base = struct
  type t = M.t
  type address = Ipaddr.V4.t
  type network = Ipaddr.V4.Prefix.t

  let is_full s = value s = Some true

  let of_network pfx =
    let l = Ipaddr.V4.Prefix.bits pfx in
    let netH, netL = Ipaddr.V4.Prefix.network pfx |> Ipaddr.V4.to_int16 in
    if l <= 16 then
      const true |> unzoom false (Bitword.make l (netH lsr (16 - l)))
    else
      const true |> unzoom false (Bitword.make (l - 16) (netL lsr (32 - l)))
                 |> unzoom false (Bitword.make 16 netH)

  let contains_address s addr =
    let aH, aL = Ipaddr.V4.to_int16 addr in
    s |> zoom (Bitword.make 16 aH)
      |> zoom (Bitword.make 16 aL)
      |> is_full

  let contains_network s pfx =
    let l = Ipaddr.V4.Prefix.bits pfx in
    let aH, aL = Ipaddr.V4.Prefix.network pfx |> Ipaddr.V4.to_int16 in
    if l <= 16 then
      s |> zoom (Bitword.make l (aH lsr (16 - l)))
        |> is_full
    else
      s |> zoom (Bitword.make 16 aH)
        |> zoom (Bitword.make (l - 16) (aL lsr (32 - l)))
        |> is_full

  let make_index plen pbuf =
    Ipaddr.V4.Prefix.make plen (Ipaddr.V4.of_bytes_exn pbuf)
end

module V6_base = struct
  type t = M.t
  type address = Ipaddr.V6.t
  type network = Ipaddr.V6.Prefix.t

  let is_full s = value s = Some true

  let of_network pfx =
    let l = Ipaddr.V6.Prefix.bits pfx in
    let net = Ipaddr.V6.Prefix.network pfx |> Ipaddr.V6.to_bytes in
    let rec loop i acc =
      if i < 0 then acc else
      loop (i - 1) (acc |> unzoom false (Bitword.make 8 (Char.code net.[i]))) in
    let lr = l mod 8 in
    if lr = 0 then
      loop (l / 8 - 1) (const true)
    else
      let w = Bitword.make lr (Char.code net.[l / 8] lsr (8 - lr)) in
      loop (l / 8 - 1) (const true |> unzoom false w)

  let contains_address s addr =
    let a0, a1, a2, a3, a4, a5, a6, a7 = Ipaddr.V6.to_int16 addr in
    s |> zoom (Bitword.make 16 a7) |> zoom (Bitword.make 16 a6)
      |> zoom (Bitword.make 16 a5) |> zoom (Bitword.make 16 a4)
      |> zoom (Bitword.make 16 a3) |> zoom (Bitword.make 16 a2)
      |> zoom (Bitword.make 16 a1) |> zoom (Bitword.make 16 a0)
      |> is_full

  let contains_network s pfx =
    let l = Ipaddr.V6.Prefix.bits pfx in
    let a = Ipaddr.V6.to_bytes (Ipaddr.V6.Prefix.network pfx) in
    let rec loop k acc =
      if k = l then acc else
      if k + 8 >= l then
        let lr = l mod 8 in
        acc |> zoom (Bitword.make lr (Char.code a.[k / 8] lsr (8 - lr)))
      else
        loop (k + 8) (acc |> zoom (Bitword.make 8 (Char.code a.[k / 8]))) in
    loop 0 s |> is_full

  let make_index plen pbuf =
    Ipaddr.V6.Prefix.make plen (Ipaddr.V6.of_bytes_exn pbuf)
end

module Make (Base : BASE) = struct
  include Base

  let empty = const false
  let is_empty s = value s = Some false
  let full = const true

  let equal = M.equal

  let union sA sB = merge (||) sA sB
  let inter sA sB = merge (&&) sA sB
  let compl sX sU = merge (fun x y -> not x && y) sX sU

  let catai ~const ~appose ~unzoom s =
    catai_bytes ~index_buffer_size:4 ~make_index ~const ~appose ~unzoom s

  let rec is_network s =
    recurse
      ~const:(fun _ -> false)
      ~appose:(fun _ _ -> false)
      ~unzoom:(fun x p s' -> not x && is_network s') s
end

module V4 = Make (V4_base)
module V6 = Make (V6_base)
