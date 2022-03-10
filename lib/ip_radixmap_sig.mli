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

(** Signature for {!Ip_radixmap}. *)

module type S = sig
  type address
  type network
  type cod
  type ('a, 'id) poly
  type t_id
  type t = (cod, t_id) poly

  val empty : t
  val is_empty : t -> bool

  val equal : t -> t -> bool

  val get_address : t -> address -> cod option
  val get_network : t -> network -> cod option

  val add_address : address -> cod -> t -> t
  val add_network : network -> cod -> t -> t
  val remove_address : address -> t -> t
  val remove_network : network -> t -> t
  val inter_network : network -> t -> t

  val map : ('a -> cod) -> ('a, _) poly -> t
  val filter : (cod -> bool) -> (cod, _) poly -> t

  val merge : ('a option -> 'b option -> cod option) ->
              ('a, _) poly -> ('b, _) poly -> t

  val catai :
    const: (network -> cod option -> 'a) ->
    appose: (network -> 'a -> 'a -> 'a) ->
    unzoom: (cod option -> int -> network -> 'a -> 'a) ->
    t -> 'a
end
