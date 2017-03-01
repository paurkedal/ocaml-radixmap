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

module Poly_v4 = struct
  type ('a, 'id) t = ('a option, 'id) Bitword_radixmap.Poly.t
end
module Poly_v6 = Poly_v4

module type BASE = sig
  type address
  type network
  type ('a, 'id) poly

  val max_length : int
  val bytes_of_address : address -> string
  val bytes_of_network : network -> string
  val length_of_network : network -> int

  val make_index : int -> Bytes.t -> network
end

module V4_base = struct
  type address = Ipaddr.V4.t
  type network = Ipaddr.V4.Prefix.t
  type ('a, 'id) poly = ('a, 'id) Poly_v4.t

  let max_length = 32
  let bytes_of_address addr = Ipaddr.V4.to_bytes addr
  let bytes_of_network net = Ipaddr.V4.to_bytes (Ipaddr.V4.Prefix.network net)
  let length_of_network net = Ipaddr.V4.Prefix.bits net

  let make_index plen pbuf =
    Ipaddr.V4.Prefix.make plen (Ipaddr.V4.of_bytes_exn pbuf)
end

module V6_base = struct
  type address = Ipaddr.V6.t
  type network = Ipaddr.V6.Prefix.t
  type ('a, 'id) poly = ('a, 'id) Poly_v6.t

  let max_length = 128
  let bytes_of_address addr = Ipaddr.V6.to_bytes addr
  let bytes_of_network net = Ipaddr.V6.to_bytes (Ipaddr.V6.Prefix.network net)
  let length_of_network net = Ipaddr.V6.Prefix.bits net

  let make_index plen pbuf =
    Ipaddr.V6.Prefix.make plen (Ipaddr.V6.of_bytes_exn pbuf)
end

module Make (Base : BASE) (Cod : Bitword_radixmap_sig.EQUAL) = struct
  include Base

  module Cod_option = struct
    type t = Cod.t option
    let equal (x : t) (y : t) =
      (match x, y with
       | None, None -> true
       | Some x, Some y -> Cod.equal x y
       | None, Some _ | Some _, None -> false)
  end
  module M = Bitword_radixmap.Make (Cod_option)

  type cod = Cod.t
  type t_id = M.t_id
  type t = M.t

  let empty = M.const None
  let is_empty m = M.value m = Some None

  let equal = M.equal

  let get_address m addr =
    match
      m |> M.zoom_string max_length (bytes_of_address addr)
        |> M.value
    with
     | None -> None
     | Some x -> x

  let get_network m netw =
    match
      m |> M.zoom_string (length_of_network netw) (bytes_of_network netw)
        |> M.value
    with
     | None -> None
     | Some x -> x

  let add_address addr x m =
    M.modify_string max_length (bytes_of_address addr)
                    (fun _ -> M.const (Some x)) m

  let add_network netw x m =
    M.modify_string (length_of_network netw) (bytes_of_network netw)
                    (fun _ -> M.const (Some x)) m

  let remove_address addr m =
    M.modify_string max_length (bytes_of_address addr)
                    (fun _ -> M.const None) m

  let remove_network netw m =
    M.modify_string (length_of_network netw) (bytes_of_network netw)
                    (fun _ -> M.const None) m

  let map f = M.map (function None -> None | Some x -> Some (f x))
  let filter f = M.map (function Some x as xopt when f x -> xopt | _ -> None)

  let catai ~const ~appose ~unzoom m =
    M.catai_bytes
      ~index_buffer_size:(max_length / 8) ~make_index
      ~const ~appose ~unzoom m

  let merge = M.merge
end

module Make_v4 = Make (V4_base)
module Make_v6 = Make (V6_base)
