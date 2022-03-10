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

(** Signature for {!Bitword_radixmap}. *)

module type EQUAL = sig
  type t
  val equal : t -> t -> bool
end

module type S = sig
  type path = Bitword.t
  type cod
  type ('a, 'id) poly
  type t_id
  type t = (cod, t_id) poly

  val equal : t -> t -> bool

  val const : cod -> t
  val is_const : t -> bool
  val value : t -> cod option
  val appose : t -> t -> t
  val unzoom : cod -> path -> t -> t
  val unzoom_string : cod -> int -> string -> t -> t
  val zoom : path -> t -> t
  val zoom_string : int -> string -> t -> t

  val modify : path -> (t -> t) -> t -> t
  val modify_string : int -> string -> (t -> t) -> t -> t
  val map : ('a -> cod) -> ('a, _) poly -> t
  val mapi : (path list -> 'a -> cod) -> ('a, _) poly -> t
  val merge : ('a -> 'b -> cod) -> ('a, _) poly -> ('b, _) poly -> t

  val recurse :
    const: (cod -> 'b) ->
    appose: (t -> t -> 'b) ->
    unzoom: (cod -> path -> t -> 'b) ->
    t -> 'b

  val cata :
    const: (cod -> 'b) ->
    appose: ('b -> 'b -> 'b) ->
    unzoom: (cod -> path -> 'b -> 'b) ->
    t -> 'b

  val catai_bytes :
    ?index_buffer_size: int -> make_index: (int -> Bytes.t -> 'i) ->
    const: ('i -> cod -> 'b) ->
    appose: ('i -> 'b -> 'b -> 'b) ->
    unzoom: (cod -> int -> 'i -> 'b -> 'b) ->
    t -> 'b

  val valid : t -> bool
end
