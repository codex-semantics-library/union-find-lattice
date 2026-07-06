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

(** Model implementation of a labeled union-find as a set of classes.
    This obviously doesn't scale very well, but is simple enough to provide a
    reference against which we can test. *)

(** {1 Parameters}                                                            *)
(******************************************************************************)



module Node = struct
  type _ t = IntTerm : int -> int t [@@unboxed]

  let to_int (type a) (IntTerm n : a t) = n

  let polyeq (type a b) (IntTerm a : a t) (IntTerm b : b t) : (a,b) PatriciaTree.cmp =
      if a = b then Eq else Diff

  let pretty (type a) fmt (IntTerm x : a t) = Format.fprintf fmt "%d" x
end

module INode = struct
  include Node
  type nonrec t = int t
  let equal (Node.IntTerm i) (Node.IntTerm j) = Int.equal i j
  let compare (Node.IntTerm i) (Node.IntTerm j) = Int.compare i j
  let of_int x = Node.IntTerm x
end

module Relation (*: Parameters.POLYMORPHIC_GROUP*) = struct
  type ('a, 'b) t =
    | Equal : ('a, 'a) t
    | Add : Z.t -> (int, int) t

  let identity = Equal

  let compose (type a b c) (x: (b, c) t) (y: (a,b) t) : (a,c) t = match x, y with
    | Equal, _ -> y
    | _, Equal -> x
    | Add a, Add b ->
        let sum = Z.(a + b) in
        if sum = Z.zero then Equal else Add sum

  let inverse (type a b) (x : (a,b) t) : (b,a) t = match x with
    | Equal -> x
    | Add a -> Add Z.(-a)

  let equal (type a b) (x: (a, b) t) (y: (a,b) t) = match x, y with
    | Equal, Equal -> true
    | Add a, Add b -> Z.equal a b
    | _ -> false

  let pretty fmt (type a b) (x: (a, b) t) = match x with
    | Equal -> Format.fprintf fmt "Equal"
    | Add x -> Format.fprintf fmt "Add(%a)" Z.pp_print x

  let debug_print fmt (type a b) (x: (a, b) t) = match x with
    | Equal -> Format.fprintf fmt "0"
    | Add x -> Format.fprintf fmt "(%a)" Z.pp_print x

  let of_int = function
    | 0 -> Equal
    | n -> Add (Z.of_int n)

  let hash (type a b) (x: (a, b) t) = match x with
    | Equal -> 0
    | Add z -> Z.hash z
end

module Value (*: Union_Find_Lattice.Parameters.POLYMORPHIC_VALUE *) = struct
  type 'a node = 'a Node.t
  type ('a,'b) relation = ('a,'b) Relation.t
  type _ t =
    | Empty : int t
    | Interval : Z.t option * Z.t option -> int t

  let incl (type a) _ (a: a t) (b: a t) = match a,b with
    | Empty, _ -> true
    | Interval (Some a, Some b), _ when Z.lt b a -> true
    | _, Empty -> false
    | Interval (a_lo,a_hi), Interval(b_lo,b_hi) ->
        let low = match a_lo, b_lo with
        | _, None -> true
        | None, _ -> false
        | Some a, Some b -> Z.leq b a in
        low && match a_hi, b_hi with
        | _, None -> true
        | None, _ -> false
        | Some a, Some b -> Z.leq a b

  let opt_pretty fmt = function
    | None -> Format.fprintf fmt "inf"
    | Some d -> Format.fprintf fmt "%a" Z.pp_print d

  let pretty _ (type a) fmt : a t -> unit = function
    | Empty -> Format.fprintf fmt "Empty"
    | Interval(a,b) -> Format.fprintf fmt "[%a,%a]" opt_pretty a opt_pretty b

  let debug_print (type a) fmt : a t -> unit = function
    | Empty -> Format.fprintf fmt "2 1"
    | Interval(a,b) -> Format.fprintf fmt "(%a) (%a)" opt_pretty a opt_pretty b

  let equal (type a) _ (x: a t) (y: a t) = match x, y with
    | Empty, Empty -> true
    | Interval(xm,xM), Interval(ym, yM) -> xm = ym && xM = yM
    | _ -> false

  let interval min max =
    match min, max with
    | None, _ -> Interval(None, max)
    | _, None -> Interval(min, None)
    | Some min, Some max -> if Z.leq min max then Interval (Some min, Some max) else Empty

  let apply (type a b) _ (x : a t) (b : (a,b) relation) : b t =
    match b with
    | Equal -> x
    | Add n -> match x with
        | Empty -> Empty
        | Interval (min, max) ->
            let map = Option.map (fun x -> Z.(x + n)) in
            interval (map min) (map max)

  let opt_or f a b = match a, b with
    | Some a, Some b -> Some (f a b)
    | (Some _ as a), None
    | None, (Some _ as a) -> a
    | None, None -> None

  let opt_and f a b = match a, b with
    | Some a, Some b -> Some (f a b)
    | _ -> None

  let meet (type a) _ (a : a t) (b : a t) : a t =
    match a, b with
    | Empty, _ -> Empty
    | _, Empty -> Empty
    | Interval (min_a, max_a), Interval (min_b, max_b) ->
        let new_min = opt_or max min_a min_b in
        let new_max = opt_or min max_a max_b in
        interval new_min new_max

  let join (type a) _ (a : a t) (b : a t) : a t option =
    match a, b with
    | Empty, x | x, Empty -> Some x
    | Interval (min_a, max_a), Interval (min_b, max_b) ->
        let new_min = opt_and min min_a min_b in
        let new_max = opt_and max max_a max_b in
        Some (interval new_min new_max)
end

module IValue = struct
  include Value
  type node = int Node.t
  type nonrec t = int t
end

(** {1 Model type}                                             *)
(******************************************************************************)

type parent =
  | Root of int Value.t option
  | Child of int Node.t * (int, int) Relation.t

type t = parent array
(** Model as a non-persistent array with explicit copy operation *)

let pp_option o = Format.pp_print_option ~none:(fun fmt () -> Format.pp_print_string fmt "None") o
let pp_parent fmt = function
  | Root v -> Format.fprintf fmt "R(%a)" (pp_option (Value.pretty ())) v
  | Child(n,r) -> Format.fprintf fmt "C(%a,%a)" Node.pretty n Relation.pretty r
let pretty fmt t =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ") pp_parent fmt (Array.to_list t)

(** {1 Model operations}                                                      *)
(******************************************************************************)

let (let*) = Option.bind
let ( ** ) = Relation.compose
let ( ~~ ) = Relation.inverse

let make n = Array.make n (Root None)

let rec find uf x =
  let Node.IntTerm i = x in
  match uf.(i) with
  | Root v -> x, Relation.identity, v
  | Child(y,r) -> let p,r',v = find uf y in
                  p, r' ** r, v

let rec check_related uf x y =
  let px,rx,_ = find uf x in
  let py,ry,_ = find uf y in
  if px = py then Some ((~~ry) ** rx) else None

let get_value uf x = let n,r,v = find uf x in match v with
  | Some v -> Some (Value.apply n v ~~r)
  | None -> None
let rec add_value ?(no_apply=false) uf x v =
  let Node.IntTerm i = x in
  match uf.(i) with
  | Root None -> uf.(i) <- Root (Some v)
  | Root (Some v') -> uf.(i) <- Root (Some (Value.meet x v v'))
  | Child (y, r) -> add_value uf y (if no_apply then v else Value.apply x v r)

let add_relation uf x y r =
  let xr, rx, v = find uf x in
  let yr, ry, _ = find uf y in
  if xr <> yr then begin
    let Node.IntTerm i = xr in
    let Node.IntTerm j = yr in
    uf.(i) <- Child(yr, ry ** r ** ~~rx);
    Option.iter (add_value uf xr) v;
    Ok ()
  end
  else
    let r' = ~~ry ** rx in
    if Relation.equal r r' then Ok () else Error r'

let rec find_in_list r_l r_r = function
  | [] -> None
  | (x,r_l',r_r')::_ when Relation.equal (r_l' ** r_l) (r_r' ** r_r) -> Some (x, r_l' ** r_l)
  | _::l -> find_in_list r_l r_r l

let value_join_opt r v1 v2 = match v1, v2 with
  | None, _ | _, None -> None
  | Some v1, Some v2 -> Value.join r v1 v2
let join uf_a uf_b =
  let n = Array.length uf_a in
  let res = make n in
  let new_classes = Hashtbl.create 10 in
  for i = 0 to n-1 do
    let node = Node.IntTerm i in
    let r_l, rel_l, v_l = find uf_a node in
    let r_r, rel_r, v_r = find uf_b node in
    let list = match Hashtbl.find_opt new_classes (r_l, r_r) with Some l -> l | None -> [] in
    match find_in_list rel_l rel_r list with
    | Some (x,r) -> res.(i) <- Child(x,r)
    | None -> let value_l = get_value uf_a node in
              let value_r = get_value uf_b node in
              res.(i) <- Root (value_join_opt node value_l value_r);
              Hashtbl.add new_classes (r_l, r_r) ((node, ~~rel_l, ~~rel_r)::list)
  done;
  res

let copy = Array.copy
