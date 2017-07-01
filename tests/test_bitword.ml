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

module Test_endian (Xe : sig include Bitword.ENDIAN_API val is_be : bool end) =
struct
  let test_one w =
    let n = Bitword.length w in
    assert (w = Xe.init n (Xe.get w));
    for i = 0 to n do
      let pfx = Xe.prefix i w in
      let sfx = Xe.suffix i w in
      let pfx', sfx' = Xe.cut i w in
      assert (Bitword.length pfx = (if Xe.is_be then i else n - i));
      assert (Bitword.length sfx = (if Xe.is_be then n - i else i));
      assert (pfx = pfx');
      assert (sfx = sfx');
      assert (Bitword.cat_exn pfx sfx = w)
    done;
    let w' = Xe.drop_exn w in
    assert (Xe.drop_unsafe w = w');
    let b, v = Xe.pop_exn w in
    let b', v' = Xe.pop_unsafe w in
    assert (b = b');
    assert (v = v');
    assert (v = w');
    assert (Xe.push_exn b v = w);
    let push_cb_exn, push_cb_unsafe =
      if b then Xe.(push_c1_exn, push_c1_unsafe)
           else Xe.(push_c0_exn, push_c0_unsafe) in
    assert (push_cb_exn v = w);
    assert (push_cb_unsafe v = w)

  let test () =
    for n = 1 to Bitword.max_length do
      for _ = 1 to Pervasives.(min 10000 (1 lsl n)) do
        test_one (Bitword.random_uniform n)
      done
    done
end

module Test_be = Test_endian (struct include Bitword.Be let is_be = true end)
module Test_le = Test_endian (struct include Bitword.Le let is_be = false end)

let () =
  Testkit.init "test_bitword";

  assert (Bitword.max_length mod 8 = 0);
  assert (Bitword.max_length >= 24);
  assert (Bitword.length Bitword.c0 = 1);
  assert (Bitword.length Bitword.c1 = 1);
  assert (Bitword.Le.get Bitword.c0 0 = false);
  assert (Bitword.Le.get Bitword.c1 0 = true);
  assert (Bitword.bits Bitword.c0 = 0);
  assert (Bitword.bits Bitword.c1 = 1);
  assert (Bitword.equal Bitword.c0 Bitword.c0);
  assert (Bitword.equal Bitword.c1 Bitword.c1);
  assert (not (Bitword.equal Bitword.c0 Bitword.c1));
  assert (not (Bitword.equal Bitword.c1 Bitword.c0));
  assert (Bitword.compare Bitword.c0 Bitword.c0 = 0);
  assert (Bitword.compare Bitword.c1 Bitword.c1 = 0);
  assert (Bitword.compare Bitword.c0 Bitword.c1 = -1);
  assert (Bitword.compare Bitword.c1 Bitword.c0 = 1);
  assert (Bitword.empty <> Bitword.c0);
  assert (Bitword.empty <> Bitword.c1);

  assert (Bitword.const 0 false = Bitword.empty);
  assert (Bitword.const 0 true  = Bitword.empty);
  assert (Bitword.const 1 false = Bitword.c0);
  assert (Bitword.const 1 true  = Bitword.c1);
  assert (Bitword.const_unsafe 0 false = Bitword.empty);
  assert (Bitword.const_unsafe 0 true  = Bitword.empty);
  assert (Bitword.const_unsafe 1 false = Bitword.c0);
  assert (Bitword.const_unsafe 1 true  = Bitword.c1);

  for n = 0 to Bitword.max_length do
    let w0 = Bitword.const n false in
    let w1 = Bitword.const n true in
    assert (Bitword.length w0 = n);
    assert (Bitword.length w1 = n);
    assert (w0 = Bitword.Le.init n (fun _ -> false));
    assert (w1 = Bitword.Le.init n (fun _ -> true));
    assert (w0 = Bitword.make n 0);
    assert (w1 = Bitword.make n Pervasives.(1 lsl n - 1));
    for _ = 0 to Pervasives.(min 10000 (1 lsl n)) do
      let w = Bitword.random_uniform n in
      assert (Bitword.length w = n);
      assert (w = Bitword.make n (Bitword.bits w));

      (* common prefix *)
      let v = Bitword.random_uniform (Random.int (n + 1)) in
      let l = Bitword.common_prefix_length w v in
      assert (Bitword.Be.prefix l w = Bitword.Be.prefix l v);
      assert (l = Bitword.length v || Bitword.Be.get v l <> Bitword.Be.get w l);

      (* cat_rem *)
      let v = Bitword.random_uniform (Random.int (Bitword.max_length + 1)) in
      let v', w' = Bitword.cat_rem v w in
      assert Bitword.(length v' = 0 || length w' = max_length);
      assert Bitword.(length v' + length w' = length v + length w);
      let nw = Bitword.length w in
      let nv = Bitword.length v in
      for i = 0 to nw - 1 do
        assert (Bitword.Le.get w i = Bitword.Le.get w' i)
      done;
      let n' = min nv (Bitword.max_length - nw) in
      for i = 0 to n' - 1 do
        assert (Bitword.Le.get v i = Bitword.Le.get w' (nw + i))
      done;
      for i = n' to nv - 1 do
        assert (Bitword.Le.get v i = Bitword.Le.get v' (i - n'))
      done
    done
  done;

  Test_be.test ();
  Test_le.test ()
