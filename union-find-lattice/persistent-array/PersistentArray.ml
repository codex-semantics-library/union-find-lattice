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

module IntHashtable = VersionedPersistentArray.IntHashtable

module type S = sig
  type 'a t

  val init: int -> (int -> 'a) -> 'a t
  val make: int -> 'a -> 'a t
  val of_array: 'a array -> 'a t
  val of_list: 'a list -> 'a t

  val size: 'a t -> int
  val get: 'a t -> int -> 'a
  val set: 'a t -> int -> 'a -> 'a t
  val pretty:
      ?pp_sep:(Format.formatter -> unit -> unit) ->
      (Format.formatter -> 'a -> unit) ->
      Format.formatter -> 'a t -> unit

  val diff: 'a t -> 'a t -> ('a * 'a) IntHashtable.t * 'a t option
  val diff_key: 'a t -> 'a t -> unit IntHashtable.t * 'a t option

  val append: 'a t -> 'a array -> unit
  val extend: 'a t -> int -> 'a -> unit

  val map: ('a -> 'b) -> 'a t -> 'b t
  val iter: ('a -> unit) -> 'a t -> unit
  val fold: ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
end

type 'a t = 'a data ref
and 'a data =
  | Arr of 'a array (* we could also use Dynarray.t *)
  | Diff of int * 'a * 'a t

(** {1 Array creation}                                                        *)
(******************************************************************************)

let mk x = ref (Arr x)
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
  match !t with
  | Arr _ -> k t
  | Diff (i, v, t') ->
      reroot t' (function t' -> match !t' with
        | Arr a as n ->
            t' := Diff (i, a.(i), t);
            a.(i) <- v;
            t := n;
            k t
        | Diff _ -> assert false)
let reroot t = reroot t ignore

let size t =
  reroot t;
  match !t with
  | Arr arr -> Array.length arr
  | _ -> assert false

let get t i =
  reroot t;
  match !t with
  | Arr arr -> arr.(i)
  | Diff _ -> assert false

let set t i v =
  reroot t;
  match !t with
  | Arr arr ->
      if arr.(i) = v then t (* No-op update *)
      else ref (Diff(i, v, t))
  | Diff _ -> assert false

let diff t1 t2 =
  reroot t1;
  match !t1 with
  | Diff _ -> assert false
  | Arr a as n ->
      let table = IntHashtable.create 100 in (* MAYBE: select size based on Array.length a? *)
      let rec iterate = function
        | Arr _ as n' -> assert (n == n') (* Check the roots match *)
        | Diff(i, v, next) ->
            begin match IntHashtable.find table i with
            | _ -> () (* We already encountered a change at i from t2 to t1, ignore this older change *)
            | exception Not_found -> IntHashtable.add table i (a.(i), v)
            end;
            iterate !next
      in iterate !t2;
      table, None

let diff_key t1 t2 =
  reroot t1;
  match !t1 with
  | Diff _ -> assert false
  | Arr a as n ->
      let table = IntHashtable.create 100 in (* MAYBE: select size based on Array.length a? *)
      let rec iterate = function
        | Arr _ as n' -> assert (n == n') (* Check the roots match *)
        | Diff(i, v, next) ->
            begin match IntHashtable.find table i with
            | () -> () (* We already encountered a change at i from t2 to t1, ignore this older change *)
            | exception Not_found -> IntHashtable.add table i ()
            end;
            iterate !next
      in iterate !t2;
      table, None

let pretty ?(pp_sep=(fun fmt () -> Format.fprintf fmt ";@ ")) pp_elt fmt t =
  reroot t;
  match !t with
  | Diff _ -> assert false
  | Arr a -> Format.pp_print_list ~pp_sep pp_elt fmt (Array.to_list a)

(** {1 Array resizing}                                                        *)
(******************************************************************************)

let append t arr =
  reroot t;
  match !t with
  | Diff _ -> assert false
  | Arr a -> t := Arr (Array.append a arr)

let extend t n x =
  if n < 0 then raise (Invalid_argument "PersistentArray.extend by negative amount");
  reroot t;
  match !t with
  | Diff _ -> assert false
  | Arr a ->
      let len = Array.length a in
      t := Arr (Array.init (len+n) (fun i -> if i < len then Array.unsafe_get a i else x))

(** {1 Iterators}                                                             *)
(******************************************************************************)

let map f t =
  reroot t;
  match !t with
  | Diff _ -> assert false
  | Arr a -> ref (Arr (Array.map f a))

let iter f t =
  reroot t;
  match !t with
  | Diff _ -> assert false
  | Arr a -> Array.iter f a

let fold f init t =
  reroot t;
  match !t with
  | Diff _ -> assert false
  | Arr a -> Array.fold_left f init a

module Versioned = VersionedPersistentArray
