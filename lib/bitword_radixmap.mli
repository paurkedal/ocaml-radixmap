(* Copyright (C) 2017  Petter Urkedal <paurkedal@gmail.com>
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

(** Core radix map using bitword keys.

    This is the implementation by with the others are based. It is somewhat
    low-level in that keys are transferred in chunks of a limited maximum size.
    The underlying is scale invaniant, and keys of arbitrary length can be used
    by iteratively zooming or combining subtrees.

    The map represent a complete function; to represent the more common kind of
    map, an option type may be used for the codomain. *)

open Bitword_radixmap_sig

module Poly : sig
  type path = Bitword.t
  type ('a, 'id) t

  val const : 'a -> ('a, 'id) t
  val is_const : ('a, _) t -> bool
  val value : ('a, _) t -> 'a option
  val zoom : path -> ('a, 'id) t -> ('a, 'id) t

  val recurse :
    const: ('a -> 'b) ->
    appose: (('a, 'id) t -> ('a, 'id) t -> 'b) ->
    unzoom: ('a -> path -> ('a, 'id) t -> 'b) ->
    ('a, 'id) t -> 'b

  val cata :
    const: ('a -> 'b) ->
    appose: ('b -> 'b -> 'b) ->
    unzoom: ('a -> path -> 'b -> 'b) ->
    ('a, 'id) t -> 'b

  val pp : (Format.formatter -> 'a -> unit) ->
           Format.formatter -> ('a, _) t -> unit
end

module Make (Cod : EQUAL) : S
  with type cod = Cod.t
   and type ('a, 'id) poly := ('a, 'id) Poly.t
