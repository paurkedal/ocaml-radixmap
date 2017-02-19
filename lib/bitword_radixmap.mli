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

module type EQUAL = sig
  type t
  val equal : t -> t -> bool
end

module Poly : sig
  type path = Bitword.t
  type 'a t

  val const : 'a -> 'a t
  val is_const : 'a t -> bool
  val value : 'a t -> 'a option
  val zoom : path -> 'a t -> 'a t

  val head :
    const: ('a -> 'b) ->
    appose: ('a t -> 'a t -> 'b) ->
    unzoom: ('a -> path -> 'a t -> 'b) ->
    'a t -> 'b

  val cata :
    const: ('a -> 'b) ->
    appose: ('b -> 'b -> 'b) ->
    unzoom: ('a -> path -> 'b -> 'b) ->
    'a t -> 'b

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module Make (Cod : EQUAL) : sig
  type path = Bitword.t
  type cod = Cod.t
  type t = cod Poly.t

  val equal : t -> t -> bool

  val const : cod -> t
  val is_const : t -> bool
  val value : t -> cod option
  val appose : t -> t -> t
  val unzoom : cod -> path -> t -> t
  val zoom : path -> t -> t

  val modify : path -> (t -> t) -> t -> t
  val map : ('a -> cod) -> 'a Poly.t -> t
  val mapi : (path list -> 'a -> cod) -> 'a Poly.t -> t
  val merge : ('a -> 'b -> cod) -> 'a Poly.t -> 'b Poly.t -> t

  val head :
    const: (cod -> 'b) ->
    appose: (t -> t -> 'b) ->
    unzoom: (cod -> path -> t -> 'b) ->
    t -> 'b

  val cata :
    const: (cod -> 'b) ->
    appose: ('b -> 'b -> 'b) ->
    unzoom: (cod -> path -> 'b -> 'b) ->
    t -> 'b
end
