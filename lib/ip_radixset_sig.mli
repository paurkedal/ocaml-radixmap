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

(** Signature for {!Ip_radixset}. *)

module type S = sig
  type t
  type address
  type network

  val empty : t
  val is_empty : t -> bool
  val full : t
  val is_full : t -> bool

  val equal : t -> t -> bool

  val of_network : network -> t
  val is_network : t -> bool

  val contains_address : t -> address -> bool
  val contains_network : t -> network -> bool
  val overlaps_network : t -> network -> bool

  val add_address : address -> t -> t
  val remove_address : address -> t -> t
  val add_network : network -> t -> t
  val remove_network : network -> t -> t
  val inter_network : network -> t -> t

  val union : t -> t -> t
  val inter : t -> t -> t
  val compl : t -> t -> t

  val cata :
    const: (network -> bool -> 'a) ->
    appose: (network -> 'a -> 'a -> 'a) ->
    unzoom: (bool -> int -> network -> 'a -> 'a) ->
    t -> 'a

  (**/**)
  val valid : t -> bool
end
