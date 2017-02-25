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

module P = Bitword_radixmap.Poly
module M = Bitword_radixmap.Make
  (struct
    type t = int
    let equal (x : int) (y : int) = x = y
  end)

let pp = P.pp Format.pp_print_int

let size = 100

let rec modify_random m =
  M.modify (Bitword.random_uniform (Random.int 24))
    (fun m' ->
      if Random.bool () then modify_random m' else
      M.const (Random.int size))
    m

let rec random_map () =
  let rec loop n acc =
    if n = 0 then acc else
    loop (n - 1) (modify_random acc) in
  loop (Random.int size) (M.const (Random.int size))

let check_path path ws =
  let rec loop i = function
   | [] -> ()
   | w :: ws ->
      let l = Bitword.length w in
      for k = 0 to l - 1 do
        let byte = Char.code path.[(i + k) / 8] in
        assert (Bitword.Be.get w k = (byte lsr (7 - (i + k) mod 8) land 1 <> 0))
      done;
      loop (i + l) ws in
  loop 0 (List.rev ws)

let test_catai_bytes () =
  let make_index plen pbuf = (plen, pbuf) in
  let const (plen, pbuf) _ =
    let path = Bytes.sub_string pbuf 0 ((plen + 7) / 8) in
    fun ws ->
      check_path path ws in
  let appose (plen, pbuf) f0 f1 =
    let path = Bytes.sub_string pbuf 0 ((plen + 7) / 8) in
    fun ws ->
      check_path path ws;
      f0 (Bitword.c0 :: ws);
      f1 (Bitword.c1 :: ws) in
  let unzoom (plen, pbuf) x w f =
    let path = Bytes.sub_string pbuf 0 ((plen + 7) / 8) in
    fun ws ->
      check_path path ws;
      f (w :: ws) in
  for _ = 1 to 10000 do
    M.catai_bytes ~make_index ~const ~appose ~unzoom (random_map ()) []
  done

let (=%) mA mB =
  assert (M.valid mA);
  assert (M.valid mB);
  M.equal mA mB

let (<>%) mA mB = not (mA =% mB)

let () =
  let mC = M.const 211 in
  assert (M.equal mC mC);
  assert (M.is_const mC);
  assert (M.value mC = Some 211);

  for _ = 1 to 10000 do
    let mA = random_map () in
    let mB = random_map () in
    let np = Random.int 24 + 1 in
    let p = Bitword.random_uniform np in
    let p', p'' = Bitword.Le.cut (Random.int (np + 1)) p in

    assert (mC =% M.zoom p mC);
    assert (mC =% M.unzoom 211 p mC);
    assert (mA =% M.zoom Bitword.empty mA);
    assert (mA =% M.unzoom (Random.int 100) Bitword.empty mA);
    assert (mA =% M.zoom p (M.unzoom (Random.int 100) p mA));
    assert (mA <>% M.unzoom (-1) p mA);
    assert (mA =% M.zoom Bitword.c0 (M.appose mA mB));
    assert (mB =% M.zoom Bitword.c1 (M.appose mA mB));

    assert (M.zoom p mA =% (mA |> M.zoom p' |> M.zoom p''));
    assert (M.unzoom 0 p mA =% M.unzoom 0 p' (M.unzoom 0 p'' mA));
    assert (M.modify p (fun _ -> mB) mA =%
            M.modify p' (M.modify p'' (fun _ -> mB)) mA);
    assert (mA =% (mA |> M.map lnot |> M.map lnot));
    assert (M.modify p (M.merge max mB) mA =% M.merge max mA (M.unzoom 0 p mB));
    assert (M.merge max mA mB =%
            M.map (~-) (M.merge min (M.map (~-) mA) (M.map (~-) mB)))
  done;
  test_catai_bytes ()
