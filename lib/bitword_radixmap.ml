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

open Bitword_radixmap_sig

let cat_bitword p pN ps =
  let p', pN' = Bitword.cat_rem p pN in
  if Bitword.is_empty p' then (pN', ps) else (pN', (p' :: ps))

module Poly = struct
  type path = Bitword.t
  type 'a t =
    | Const of 'a
    | Appose of 'a t * 'a t
    | Unzoom of 'a * Bitword.t * 'a t

  let rec max_depth = function
   | Const x -> 0
   | Appose (h0, h1) -> 1 + max (max_depth h0) (max_depth h1)
   | Unzoom (x, p, h) -> Bitword.length p + max_depth h

  let head ~const ~appose ~unzoom = function
   | Const x -> const x
   | Appose (h0, h1) -> appose h0 h1
   | Unzoom (x, p, h) -> unzoom x p h

  let rec cata ~const ~appose ~unzoom = function
   | Const x ->
      const x
   | Appose (h0, h1) ->
      appose (cata ~const ~appose ~unzoom h0) (cata ~const ~appose ~unzoom h1)
   | Unzoom (x, p, h) ->
      unzoom x p (cata ~const ~appose ~unzoom h)

  let catai_bytes ?index_buffer_size ~make_index
                  ~const ~appose ~unzoom h =
    let pbuf =
      Bytes.make
        (match index_buffer_size with
         | None -> ((max_depth h + 7) / 8)
         | Some n -> n) '\x00' in
    let rec loop plen = function
     | Const x ->
        const (make_index plen pbuf) x
     | Appose (h0, h1) ->
        let byte = Char.code (Bytes.get pbuf (plen / 8)) in
        let bit = 0x80 lsr (plen mod 8) in
        let acc0 = loop (plen + 1) h0 in
        Bytes.set pbuf (plen / 8) (Char.chr (byte lor bit));
        let acc1 = loop (plen + 1) h1 in
        Bytes.set pbuf (plen / 8) (Char.chr byte);
        appose (make_index plen pbuf) acc0 acc1
     | Unzoom (x, p, h) ->
        let bp = Bitword.bits p in
        let lp = Bitword.length p in
        let first_byte = Char.code (Bytes.get pbuf (plen / 8)) in
        let acc =
          if lp <= 8 - plen mod 8 then begin
            let first_bits = bp lsl (8 - plen mod 8 - lp) in
            Bytes.set pbuf (plen / 8) (Char.chr (first_byte lor first_bits));
            let acc = loop (plen + lp) h in
            Bytes.set pbuf (plen / 8) (Char.chr first_byte);
            acc
          end else begin
            let first_bits = bp lsr (lp - 8 + plen mod 8) in
            Bytes.set pbuf (plen / 8) (Char.chr (first_byte lor first_bits));
            let rec ext_pbuf l j =
              if l >= 8 then begin
                Bytes.set pbuf j (Char.chr (bp lsr (l - 8) land 0xff));
                ext_pbuf (l - 8) (j + 1)
              end else if l > 0 then begin
                Bytes.set pbuf j (Char.chr (bp lsl (8 - l) land 0xff))
              end in
            let lp' = lp - 8 + plen mod 8 in
            ext_pbuf lp' (plen / 8 + 1);
            let acc = loop (plen + lp) h in
            Bytes.fill pbuf (plen / 8 + 1) ((lp' + 7) / 8) '\x00';
            Bytes.set pbuf (plen / 8) (Char.chr first_byte);
            acc
          end in
        unzoom (make_index plen pbuf) x p acc in
    loop 0 h

  let const x = Const x
  let is_const = function Const _ -> true | _ -> false
  let value = function Const x -> Some x | _ -> None

  let rec zoom pZ h =
    if Bitword.is_empty pZ then h else
    (match h with
     | Const _ -> h
     | Appose (h0, h1) ->
        let kZ, pZ' = Bitword.Be.pop_exn pZ in
        zoom pZ' (if kZ then h1 else h0)
     | Unzoom (xH, pN, hN) ->
        let nZ = Bitword.length pZ in
        let nN = Bitword.length pN in
        if nZ > nN then
          let pC, pZ' = Bitword.Be.cut nN pZ in
          if Bitword.equal pC pN then zoom pZ' hN else (Const xH)
        else if nZ = nN then
          if Bitword.equal pZ pN then hN else (Const xH)
        else
          let pC, pN' = Bitword.Be.cut nZ pN in
          if Bitword.equal pC pZ then Unzoom (xH, pN', hN) else (Const xH))

  let rec pp f fmtr m =
    Format.pp_print_char fmtr '{';
    (match m with
     | Const x ->
        Format.pp_print_string fmtr "_ ↦ ";
        f fmtr x
     | Appose (m0, m1) ->
        Format.pp_print_string fmtr "0 ↦ ";
        pp f fmtr m0;
        Format.pp_print_string fmtr ", 1 ↦ ";
        pp f fmtr m1;
     | Unzoom (x, p, mN) ->
        Bitword.pp_base2 fmtr p;
        Format.pp_print_string fmtr " ↦ ";
        pp f fmtr mN;
        Format.pp_print_string fmtr ", _ ↦ ";
        f fmtr x);
    Format.pp_print_char fmtr '}'
end

module Make (Cod : EQUAL) = struct
  open Poly

  type path = Bitword.t
  type cod = Cod.t
  type t = cod Poly.t

  let const = Poly.const
  let is_const = Poly.is_const
  let value = Poly.value
  let zoom = Poly.zoom
  let head = Poly.head
  let cata = Poly.cata
  let catai_bytes = Poly.catai_bytes

  let rec valid = function
   | Const _ -> true
   | Appose (Const x0, Const x1) -> not (Cod.equal x0 x1)
   | Appose (Const _, _) | Appose (_, Const _) -> false
   | Appose (h0, h1) -> valid h0 && valid h1
   | Unzoom (xH, p, Const xN) when Cod.equal xH xN -> false
   | Unzoom (xH, p, h) -> not (Bitword.is_empty p) && valid_hcont xH h
  and valid_hcont xHH = function
   | Unzoom (xH, p, h) ->
      (Bitword.is_full p || not (Cod.equal xHH xH)) && valid_hcont xH h
   | h -> valid h

  let rec equal hA hB =
    (match hA, hB with
     | Const xA, Const xB -> Cod.equal xA xB
     | Const _, _ | _, Const _ -> false
     | Appose (h0A, h1A), Appose (h0B, h1B) -> equal h0A h0B && equal h1A h1B
     | Appose _, _ | _, Appose _ -> false
     | Unzoom (xA, pA, hAN), Unzoom (xB, pB, hBN) ->
        Cod.equal xA xB && Bitword.equal pA pB && equal hAN hBN)

  let unzoom xA pA h =
    if Bitword.is_empty pA then h else
    (match h with
     | Const x when Cod.equal x xA -> h
     | Unzoom (xH, pN, hN) when Cod.equal xH xA ->
        let pA', pN' = Bitword.cat_rem pA pN in
        if Bitword.is_empty pA'
        then Unzoom (xH, pN', hN)
        else Unzoom (xH, pA', Unzoom (xH, pN', hN))
     | Const _ | Appose _ | Unzoom _ -> Unzoom (xA, pA, h))

  let appose h0 h1 =
    (match h0, h1 with
     | Const x0, Const x1 ->
        if Cod.equal x0 x1 then Const x0 else Appose (h0, h1)
     | Unzoom (xH, pN, hN), Const xC
       when Cod.equal xC xH && not (Bitword.is_full pN) ->
        Unzoom (xH, Bitword.Be.push_c0_exn pN, hN)
     | Const xC, Unzoom (xH, pN, hN)
       when Cod.equal xC xH && not (Bitword.is_full pN) ->
        Unzoom (xH, Bitword.Be.push_c1_exn pN, hN)
     | _, Const x1 -> Unzoom (x1, Bitword.c0, h0)
     | Const x0, _ -> Unzoom (x0, Bitword.c1, h1)
     | (Appose _ | Unzoom _), (Appose _ | Unzoom _) -> Appose (h0, h1))

  let rec modify pA f h =
    if Bitword.is_empty pA then f h else
    (match h with
     | Const xH ->
        (match f h with
         | Const xN when Cod.equal xH xN -> h
         | hN -> unzoom xH pA hN)
     | Appose (h0, h1) ->
        let k, pA' = Bitword.Be.pop_exn pA in
        if k then appose h0 (modify pA' f h1)
             else appose (modify pA' f h0) h1
     | Unzoom (xH, pN, hN) ->
        let nA = Bitword.length pA in
        let nN = Bitword.length pN in
        let nC = Bitword.common_prefix_length pN pA in
        if nC = nA then
          let pN' = Bitword.Be.suffix nC pN in
          unzoom xH pA (f (unzoom xH pN' hN))
        else if nC = nN then
          let pA' = Bitword.Be.suffix nC pA in
          unzoom xH pN (modify pA' f hN)
        else
          let pC = Bitword.Be.prefix nC pN in
          let kA = Bitword.Be.get pA nC in
          let pA' = Bitword.Be.suffix (nC + 1) pA in
          let pN' = Bitword.Be.suffix (nC + 1) pN in
          let hA' = unzoom xH pA' (f (Const xH)) in
          let hN' = unzoom xH pN' hN in
          unzoom xH pC (if kA then appose hN' hA' else appose hA' hN'))

  let rec map f = function
   | Const x -> Const (f x)
   | Appose (h0, h1) -> appose (map f h0) (map f h1)
   | Unzoom (x, p, h) -> unzoom (f x) p (map f h)

  let mapi f h =
    let rec loop p ps = function
     | Const x ->
        Const (f (p :: ps) x)
     | Appose (h0, h1) ->
        if Bitword.is_full p then
          appose (loop Bitword.c0 (p :: ps) h0)
                 (loop Bitword.c1 (p :: ps) h1)
        else
          appose (loop (Bitword.Be.push_c0_exn p) ps h0)
                 (loop (Bitword.Be.push_c1_exn p) ps h1)
     | Unzoom (x, pN, hN) ->
        let p', ps' = cat_bitword p pN ps in
        unzoom (f (p :: ps) x) p (loop p' ps' hN) in
    (match h with
     | Const x -> Const (f [] x)
     | Appose (h0, h1) -> appose (loop Bitword.c0 [] h0) (loop Bitword.c1 [] h1)
     | Unzoom (x, pN, hN) -> unzoom (f [] x) pN (loop pN [] hN))

  let rec merge f hA hB =
    (match hA, hB with
     | Const xA, Const xB ->
        Const (f xA xB)
     | Const xA, Appose (h0B, h1B) ->
        let g xB = f xA xB in appose (map g h0B) (map g h1B)
     | Appose (h0A, h1A), Const xB ->
        let g xA = f xA xB in appose (map g h0A) (map g h1A)
     | Const xA, Unzoom (xB, pB, hNB) ->
        unzoom (f xA xB) pB (map (fun x -> f xA x) hNB)
     | Unzoom (xA, pA, hNA), Const xB ->
        unzoom (f xA xB) pA (map (fun x -> f x xB) hNA)
     | Appose (h0A, h1A), Appose (h0B, h1B) ->
        appose (merge f h0A h0B) (merge f h1A h1B)
     | Appose (h0A, h1A), Unzoom (xB, pB, hNB) ->
        let kB, pB' = Bitword.Be.pop_exn pB in
        let hB' = if Bitword.is_empty pB' then hNB else Unzoom (xB, pB', hNB) in
        if kB then appose (map (fun x -> f x xB) h0A) (merge f h1A hB')
              else appose (merge f h0A hB') (map (fun x -> f x xB) h1A)
     | Unzoom (xA, pA, hNA), Appose (h0B, h1B) ->
        let kA, pA' = Bitword.Be.pop_exn pA in
        let hA' = if Bitword.is_empty pA' then hNA else Unzoom (xA, pA', hNA) in
        if kA then appose (map (fun x -> f xA x) h0B) (merge f hA' h1B)
              else appose (merge f hA' h0B) (map (fun x -> f xA x) h1B)
     | Unzoom (xA, pA, hNA), Unzoom (xB, pB, hNB) ->
        let xAB = f xA xB in
        let nA = Bitword.length pA in
        let nB = Bitword.length pB in
        let nC = Bitword.common_prefix_length pA pB in
        if nC = nA then
          if nC = nB then unzoom xAB pA (merge f hNA hNB) else
          let hB' = Unzoom (xB, Bitword.Be.suffix nC pB, hNB) in
          unzoom xAB pA (merge f hNA hB')
        else if nC = nB then
          let hA' = Unzoom (xA, Bitword.Be.suffix nC pA, hNA) in
          unzoom xAB pB (merge f hA' hNB)
        else
          let pC = Bitword.Be.prefix nC pA in
          let kA = Bitword.Be.get pA nC in
          let pA' = Bitword.Be.suffix (nC + 1) pA in
          let pB' = Bitword.Be.suffix (nC + 1) pB in
          let hA' = unzoom xAB pA' (map (fun x -> f x xB) hNA) in
          let hB' = unzoom xAB pB' (map (fun x -> f xA x) hNB) in
          unzoom xAB pC (if kA then appose hB' hA' else appose hA' hB'))
end
