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

(** Radix map over IP networks. *)

module Poly_v4 : sig
  type ('a, 'id) t
end

module Poly_v6 : sig
  type ('a, 'id) t
end

module Make_v4 (Cod : Bitword_radixmap_sig.EQUAL) : Ip_radixmap_sig.S
  with type address = Ipaddr.V4.t
   and type network = Ipaddr.V4.Prefix.t
   and type cod = Cod.t

module Make_v6 (Cod : Bitword_radixmap_sig.EQUAL) : Ip_radixmap_sig.S
  with type address = Ipaddr.V6.t
   and type network = Ipaddr.V6.Prefix.t
   and type cod = Cod.t
