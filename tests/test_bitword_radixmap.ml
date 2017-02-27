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

module Bitwords_map = Bitword_radixmap.Make
  (struct
    type t = Bitword.t list
    let equal (x : t) (y : t) = x = y
  end)

let pp = P.pp Format.pp_print_int

let size = 100

module Path = struct
  type t = string

  let make l buf = (l, Bytes.sub_string buf 0 ((l + 7) / 8))

  let empty = (0, "")

  let length (l, _) = l

  let get (l, s) k =
    assert (0 <= k && k < l);
    Char.code s.[k / 8] lsr (7 - k mod 8) land 1 <> 0

  let prefix l' (l, s) =
    let s' =
      String.init ((l' + 7) / 8)
        (fun i ->
          if 8 * i + 8 <= l' then s.[i] else
          Char.chr (Char.code s.[i] land 0xff lsl (8 - l' mod 8) land 0xff)) in
    (l', s')

  let pp fmtr (l, s) = Format.printf "(%d,%S)" l s
end

let rec random_bitwords_map ws =
  (match Random.int 7 with
   | 0 | 2 ->
      Bitwords_map.appose (random_bitwords_map (Bitword.c0 :: ws))
                          (random_bitwords_map (Bitword.c1 :: ws))
   | 1 | 3 ->
      let w = Bitword.random_uniform (Random.int Bitword.max_length + 1) in
      Bitwords_map.unzoom ws w (random_bitwords_map (w :: ws))
   | _ ->
      Bitwords_map.const ws)

let check_const_path p ws =
  let rec loop i = function
   | [] -> ()
   | w :: ws ->
      let l = Bitword.length w in
      for k = 0 to l - 1 do
        assert (Bitword.Be.get w k = Path.get p (i + k))
      done;
      loop (i + l) ws in
  loop 0 (List.rev ws)

let check_unzoom_path p ws =
  let rec loop i = function
   | [] -> ()
   | w :: ws ->
      let lw = Bitword.length w in
      let lr = Path.length p - i in
      assert (lr + 1 >= lw);
      let l = min lw lr in
      if l > 0 then begin
        for k = 0 to l - 1 do
          assert (Bitword.Be.get w k = Path.get p (i + k))
        done;
        loop (i + l) ws
      end in
  loop 0 (List.rev ws)

let test_catai_bytes () =
  let make_index plen pbuf = Path.make plen pbuf in
  let const p ws = check_const_path p ws; p in
  let appose p p0 p1 =
    assert (Path.length p0 = Path.length p + 1);
    assert (Path.length p1 = Path.length p + 1);
    assert (Path.get p0 (Path.length p) = false);
    assert (Path.get p1 (Path.length p) = true);
    p in
  let unzoom ws depth pN pN' =
    assert (pN = pN');
    let p = Path.prefix (Path.length pN - depth) pN in
    check_unzoom_path p ws;
    p in
  for _ = 1 to 10000 do
    let m = random_bitwords_map [] in
    let p = Bitwords_map.catai_bytes ~make_index ~const ~appose ~unzoom m in
    assert (p = Path.empty);
  done

let (=%) mA mB =
  assert (M.valid mA);
  assert (M.valid mB);
  M.equal mA mB

let (<>%) mA mB = not (mA =% mB)

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

let () =
  Testkit.init "test_bitword_radixmap";

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
