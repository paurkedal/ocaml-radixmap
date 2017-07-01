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

(** Limited-length bit vectors represented by a unboxed integers.

    This module provides bit vectors up to a maximum length, limited by what can
    be stored in an OCaml [int].  The maximum length is architecture-dependent
    but guaranteed to be a multiple of 8 and at least 24.
 *)

type t = private int

(** {2 Inspection and Slicing} *)

val max_length : int

val length : t -> int

val bits : t -> int

(** {2 Comparison} *)

val equal : t -> t -> bool

val compare : t -> t -> int

val common_prefix_length : t -> t -> int

(** {2 Construction} *)

val is_empty : t -> bool
val is_full : t -> bool

val empty : t
val c0 : t
val c1 : t
val c00 : t
val c01 : t
val c10 : t
val c11 : t

val const : int -> bool -> t
val const_unsafe : int -> bool -> t

val make : int -> int -> t
val make_unsafe : int -> int -> t

val cat_exn : t -> t -> t
val cat_rem : t -> t -> t * t

val random_uniform : int -> t

(** {2 Endian-Dependent Operations} *)

module type ENDIAN_API = sig
  val init : int -> (int -> bool) -> t
  val get : t -> int -> bool

  (** {2 Parts} *)

  val prefix : int -> t -> t
  val prefix_unsafe : int -> t -> t
  val suffix : int -> t -> t
  val suffix_unsafe : int -> t -> t
  val cut : int -> t -> t * t

  (** {2 Queue-Like Operations}

      These functions operate around index 0.  That is, for big-endian it
      changes the prefix bit and for low-endian the suffix bit. *)

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

module Be : ENDIAN_API
module Le : ENDIAN_API

(** {2 Parsing and Pretty-Printing} *)

val pp_set : Format.formatter -> t -> unit
val pp_base2 : Format.formatter -> t -> unit

val to_base2_string : t -> string

val of_base2_string_exn : string -> t
