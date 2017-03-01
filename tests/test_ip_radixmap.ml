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

module Int_equal = struct
  type t = int
  let equal (x : t) (y : t) = x = y
end

module type S = sig
  include Ip_radixmap_sig.S with type cod = int

  val max_length : int
  val random_address : unit -> address
  val random_network : unit -> network

  val subset : subnet: network -> network: network -> bool

  module Infix : sig
    val (=@) : address -> address -> bool
    val (<:@) : address -> network -> bool
  end
end

module V4 = struct
  include Ip_radixmap.Make_v4 (Int_equal)

  let max_length = 32

  let random_address () =
    let a0 = Random.bits () land 0xffff in
    let a1 = Random.bits () land 0xffff in
    Ipaddr.V4.of_int16 (a0, a1)

  let random_network () =
    Ipaddr.V4.Prefix.make (Random.int 33) (random_address ())

  let subset = Ipaddr.V4.Prefix.subset

  module Infix = struct
    let (=@) a0 a1 = Ipaddr.V4.compare a0 a1 = 0
    let (<:@) addr netw = Ipaddr.V4.Prefix.mem addr netw
  end
end

module V6 = struct
  include Ip_radixmap.Make_v6 (Int_equal)

  let max_length = 128

  let random_address () =
    let s = String.init 16 (fun _ -> Char.chr (Random.int 0x100)) in
    Ipaddr.V6.of_bytes_exn s

  let random_network () =
    Ipaddr.V6.Prefix.make (Random.int 129) (random_address ())

  let subset = Ipaddr.V6.Prefix.subset

  module Infix = struct
    let (=@) a0 a1 = Ipaddr.V6.compare a0 a1 = 0
    let (<:@) addr netw = Ipaddr.V6.Prefix.mem addr netw
  end
end

module Make (M : S) = struct
  open M.Infix
  let (=%) mA mB = M.equal mA mB
  let (<>%) mA mB = not (M.equal mA mB)

  module Ip_assoc = struct

    type 'a t =
      | Empty
      | Add_address of M.address * 'a * 'a t
      | Add_network of M.network * 'a * 'a t
      | Remove_address of M.address * 'a t
      | Remove_network of M.network * 'a t

    let empty = Empty
    let add_address addr x ipa = Add_address (addr, x, ipa)
    let remove_address addr ipa = Remove_address (addr, ipa)
    let add_network netw x ipa = Add_network (netw, x, ipa)
    let remove_network netw ipa = Remove_network (netw, ipa)

    let rec get_address ipa addr =
      (match ipa with
       | Empty -> None
       | Add_address (addr', v', ipa') ->
          if addr =@ addr' then Some v' else get_address ipa' addr
       | Remove_address (addr', ipa') ->
          if addr =@ addr' then None else get_address ipa' addr
       | Add_network (netw', v', ipa') ->
          if addr <:@ netw' then Some v' else get_address ipa' addr
       | Remove_network (netw', ipa') ->
          if addr <:@ netw' then None else get_address ipa' addr)

  end

  let random_map range =
    let rec loop n m =
      if n = 0 then m else
      let k = M.random_network () in
      let v = Random.int range in
      loop (n - 1) (M.add_network k v m) in
    loop (1 lsl (Random.int 12)) M.empty

  let left x y =
    (match x, y with
     | None, None -> None
     | Some x, _ -> Some x
     | None, Some y -> Some y)

  let test () =
    assert (M.is_empty M.empty);
    assert (M.empty =% M.empty);

    for _ = 1 to 1_000 do
      let rec loop k m ipa =
        for _ = 1 to 10 do
          let addr = M.random_address () in
          assert (M.get_address m addr = Ip_assoc.get_address ipa addr)
        done;
        if k = 0 then () else
        (match Random.int 4 with
         | 0 ->
            let addr = M.random_address () in
            let m' = M.add_address addr k m in
            let ipa' = Ip_assoc.add_address addr k ipa in
            assert (M.get_address m' addr = Some k);
            assert (m <>% m');
            loop (k - 1) m' ipa'
         | 1 ->
            let netw = M.random_network () in
            let m' = M.add_network netw k m in
            let ipa' = Ip_assoc.add_network netw k ipa in
            assert (M.get_network m' netw = Some k);
            assert (m <>% m');
            loop (k - 1) m' ipa'
         | 2 ->
            let addr = M.random_address () in
            let m' = M.remove_address addr m in
            let ipa' = Ip_assoc.remove_address addr ipa in
            assert (M.get_address m' addr = None);
            loop (k - 1) m' ipa'
         | 3 ->
            let netw = M.random_network () in
            let m' = M.remove_network netw m in
            let ipa' = Ip_assoc.remove_network netw ipa in
            assert (M.get_network m' netw = None);
            loop (k - 1) m' ipa'
         | _ -> assert false) in
      loop 100 M.empty Ip_assoc.empty
    done;

    for _ = 1 to 1_000 do
      let sA = random_map 1000 in
      let sB = M.filter (fun x -> x <= 700) sA in
      let sC = M.filter (fun x -> x >= 300) sA in
      assert (M.merge left sB sC =% sA)
    done
end

module Test_v4 = Make (V4)
module Test_v6 = Make (V6)

let () =
  Testkit.init "test_ip_radixmap";
  Test_v4.test ();
  Test_v6.test ()
