(**************************************************************************)
(*  This file is part of the Codex semantics library                      *)
(*    (Union-find lattice subcomponent).                                  *)
(*                                                                        *)
(*  Copyright (C) 2026                                                    *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 3.0                 *)
(*  for more details (enclosed in the file LICENSE).                      *)
(*                                                                        *)
(**************************************************************************)

type 'a t = { mutable cell:'a data; version: int }
and 'a data =
  | Arr of 'a array
  | Diff of int * 'a * 'a t

(** {1 Array creation}                                                        *)
(******************************************************************************)

let mk x = { cell=Arr x; version=0 }
let init n f = Array.init n f |> mk
let make n x = Array.make n x |> mk
let of_array arr = Array.copy arr |> mk
let of_list l = Array.of_list l |> mk

(** {1 Array operations}                                                      *)
(******************************************************************************)

(* let rec reroot t =
  match !t with
  | Arr arr -> ()
  | Diff (i, v, t') ->
      reroot t';
      match !t' with
      | Arr a as n ->
          t' := Diff (i, a.(i), t);
          a.(i) <- v;
          t := n
      | Diff _ -> assert false *)

(** Rewrote reroot in continuation passing style, to avoid stack-overflows on large reroots *)
let rec reroot t k =
  match t.cell with
  | Arr _ -> k t
  | Diff (i, v, t') ->
      reroot t' (function t' -> match t'.cell with
        | Arr a as n ->
            t'.cell <- Diff (i, a.(i), t);
            a.(i) <- v;
            t.cell <- n;
            k t
        | Diff _ -> assert false)
let reroot t = reroot t ignore

let size t =
  reroot t;
  match t.cell with
  | Arr arr -> Array.length arr
  | _ -> assert false

let get t i =
  reroot t;
  match t.cell with
  | Arr arr -> arr.(i)
  | Diff _ -> assert false

let set t i v =
  reroot t;
  match t.cell with
  | Arr arr ->
      if arr.(i) = v then t (* No-op update *)
      else { cell=Diff(i, v, t); version=t.version + 1 }
  | Diff _ -> assert false

module IntHashtable = Hashtbl.Make(struct
  type t = int
  let hash = Fun.id
  let equal = Int.equal
end)

let diff t1 t2 =
  reroot t1;
  match t1.cell with
  | Diff _ -> assert false
  | Arr a as n ->
      let table = IntHashtable.create 100 in (* MAYBE: select size based on Array.length a? *)
      let rec iterate minv = function
        | Arr _ as n' -> assert (n == n'); minv (* Check the roots match *)
        | Diff(i, v, next) ->
            begin match IntHashtable.find table i with
            | _ -> () (* We already encountered a change at i from t2 to t1, ignore this older change *)
            | exception Not_found -> IntHashtable.add table i (a.(i), v)
            end;
            let minv = if minv.version > next.version then next else minv in
            iterate minv next.cell
      in
      let minv = iterate t2 t2.cell in
      table, Some minv

let diff_key t1 t2 =
  reroot t1;
  match t1.cell with
  | Diff _ -> assert false
  | Arr a as n ->
      let table = IntHashtable.create 100 in (* MAYBE: select size based on Array.length a? *)
      let rec iterate minv = function
        | Arr _ as n' -> assert (n == n'); minv (* Check the roots match *)
        | Diff(i, v, next) ->
            begin match IntHashtable.find table i with
            | () -> () (* We already encountered a change at i from t2 to t1, ignore this older change *)
            | exception Not_found -> IntHashtable.add table i ()
            end;
            let minv = if minv.version > next.version then next else minv in
            iterate minv next.cell
      in
      let minv = iterate t2 t2.cell in
      table, Some minv

let pretty ?(pp_sep=(fun fmt () -> Format.fprintf fmt ";@ ")) pp_elt fmt t =
  reroot t;
  match t.cell with
  | Diff _ -> assert false
  | Arr a -> Format.pp_print_list ~pp_sep pp_elt fmt (Array.to_list a)

(** {1 Array resizing}                                                        *)
(******************************************************************************)

let append t arr =
  reroot t;
  match t.cell with
  | Diff _ -> assert false
  | Arr a -> t.cell <- Arr (Array.append a arr)

let extend t n x =
  if n < 0 then raise (Invalid_argument "PersistentArray.extend by negative amount");
  reroot t;
  match t.cell with
  | Diff _ -> assert false
  | Arr a ->
      let len = Array.length a in
      t.cell <- Arr (Array.init (len+n) (fun i -> if i < len then Array.unsafe_get a i else x))

(** {1 Iterators}                                                             *)
(******************************************************************************)

let map f t =
  reroot t;
  match t.cell with
  | Diff _ -> assert false
  | Arr a -> { cell=Arr (Array.map f a); version=0 }

let iter f t =
  reroot t;
  match t.cell with
  | Diff _ -> assert false
  | Arr a -> Array.iter f a

let fold f init t =
  reroot t;
  match t.cell with
  | Diff _ -> assert false
  | Arr a -> Array.fold_left f init a
