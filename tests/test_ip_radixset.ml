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

module type S = sig
  include Ip_radixset_sig.S

  val max_length : int
  val random_address : unit -> address
  val random_network : unit -> network
end

module V4 = struct
  include Ip_radixset.V4

  let max_length = 32

  let random_address () =
    let a0 = Random.bits () land 0xffff in
    let a1 = Random.bits () land 0xffff in
    Ipaddr.V4.of_int16 (a0, a1)

  let random_network () =
    Ipaddr.V4.Prefix.make (Random.int 33) (random_address ())
end

module V6 = struct
  include Ip_radixset.V6

  let max_length = 128

  let random_address () =
    let s = String.init 16 (fun _ -> Char.chr (Random.int 0x100)) in
    Ipaddr.V6.of_bytes_exn s

  let random_network () =
    Ipaddr.V6.Prefix.make (Random.int 129) (random_address ())
end

module Make (M : S) = struct

  let sO = M.empty
  let sU = M.full

  let (=%) sA sB =
      assert (M.valid sA);
      assert (M.valid sB);
      M.equal sA sB

  let (<>%) sA sB = not (M.equal sA sB)

  let random_set () =
    let rec loop n s =
      if n = 0 then s else
      loop (n - 1) (M.add_network (M.random_network ()) s) in
    loop (1 lsl (Random.int 12)) sO

  let test () =
    assert (M.is_empty sO);
    assert (M.is_full sU);
    assert (not (M.is_empty sU));
    assert (not (M.is_full sO));
    assert (sO =% sO);
    assert (sU =% sU);
    assert (sO <>% sU);
    assert (sU <>% sO);
    assert (M.is_empty (M.compl sO sO));
    assert (M.is_empty (M.compl sU sO));
    assert (M.is_empty (M.compl sU sU));
    assert (M.is_full  (M.compl sO sU));
    assert (M.is_empty (M.union sO sO));
    assert (M.is_full  (M.union sO sU));
    assert (M.is_full  (M.union sU sO));
    assert (M.is_full  (M.union sU sU));
    assert (M.is_empty (M.inter sO sO));
    assert (M.is_empty (M.inter sO sU));
    assert (M.is_empty (M.inter sU sO));
    assert (M.is_full  (M.inter sU sU));

    let rec loop_mutate n s =
      if n > 0 then
      (match 2 + Random.int 1 with
       | 0 ->
          let addr = M.random_address () in
          let s' = M.add_address addr s in
          assert (M.contains_address s' addr);
          assert (M.equal s s' = M.contains_address s addr);
          loop_mutate (n - 1) s'
       | 1 ->
          let addr = M.random_address () in
          let s' = M.remove_address addr s in
          assert (not (M.contains_address s' addr));
          assert (M.equal s s' <> M.contains_address s addr);
          loop_mutate (n - 1) s'
       | 2 ->
          let net = M.random_network () in
          let s' = M.add_network net s in
          assert (M.contains_network s' net);
          assert (M.equal s s' || not (M.contains_network s net));
          loop_mutate (n - 1) s'
       | 3 ->
          let net = M.random_network () in
          let s' = M.remove_network net s in
          assert (not (M.contains_network s' net));
          assert (not (M.equal s s') || not (M.contains_network s net));
          loop_mutate (n - 1) s'
       | _ -> assert false)
    in
    loop_mutate 100_000 sO;

    for _ = 1 to 1_000 do
      let sA = random_set () in
      let sB = random_set () in
      let sX = random_set () in
      let sAB = M.inter sA sB and uAB = M.union sA sB in
      let cXA = M.compl sX sA and cAX = M.compl sA sX in
      let cXB = M.compl sX sB and cBX = M.compl sB sX in

      (* idempotence and dominans *)
      assert (sA =% (M.union sA sO));
      assert (sA =% (M.union sO sA));
      assert (sA =% (M.union sA sA));
      assert (sO =% (M.inter sA sO));
      assert (sO =% (M.inter sO sA));
      assert (sA =% (M.inter sA sA));
      assert (sO =% (M.compl sA sO));
      assert (sA =% (M.compl sO sA));
      assert (sO =% (M.compl sA sA));
      assert (cXA =% (M.compl sX cXA));

      (* complement *)
      assert (M.is_empty (M.inter sA (M.compl sA sU)));
      assert (M.is_full  (M.union sA (M.compl sA sU)));
      assert (M.inter sA cAX =% sO);
      assert (M.union sA cAX =% M.union sA sX);
      assert (sAB =% M.compl (M.compl sB sA) sA);

      (* commutativity *)
      assert (sAB =% (M.inter sB sA));
      assert (uAB =% (M.union sB sA));

      (* associativity *)
      assert (M.union sA (M.union sB sX) =% M.union uAB sX);
      assert (M.inter sA (M.inter sB sX) =% M.inter sAB sX);

      (* distributivity *)
      assert (M.union sAB sX =% M.inter (M.union sA sX) (M.union sB sX));
      assert (M.inter uAB sX =% M.union (M.inter sA sX) (M.inter sB sX));

      (* distributivity: (A ∖ X) ∪ (B ∖ X) = (A ∪ B) ∖ X *)
      assert (M.union cXA cXB =% M.compl sX uAB);

      (* distributivity: (A ∖ X) ∩ B = A ∩ (B ∖ X) = (A ∩ B) ∖ X *)
      assert (M.inter cXA sB =% M.compl sX sAB);
      assert (M.inter sA cXB =% M.compl sX sAB);

      (* X ∖ (A ∪ B) = (X ∖ A) ∩ (X ∖ B) *)
      assert (M.compl uAB sX =% M.inter cAX cBX)
    done
end

module Test_V4 = Make (V4)
module Test_V6 = Make (V6)

let () =
  Testkit.init "test_ip_radixset";
  Test_V4.test ();
  Test_V6.test ()
