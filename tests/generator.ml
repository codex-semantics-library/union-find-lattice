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

(** QCheck generator:
    - creates random union-find partitions and values.
    - also defines a shrinker (to reduce failing tests) and a printer

Main functions:
- [partition]: generates a pair [(operations, size)] that creates a union-find
  with [size] nodes via [operations] (list of [union]s and [add_values]). *)


module Model = Testing_utils.Model

(** [remove_duplicates cast seen list] removes duplicate elements
    (elements whose [cast elt] value is the same) from [list]. [seen] is used to
    store previously seen elements. *)
let rec remove_duplicate cast seen = function
  | [] -> []
  | t::q when Hashtbl.mem seen (cast t) -> remove_duplicate cast seen q
  | t::q ->
      let () = Hashtbl.add seen (cast t) () in
      t :: remove_duplicate cast seen q

(** Transform a list of list into a partition (list of list with no duplicates) *)
let rec make_partition cast seen = function
  | [] -> []
  | t::q -> let t = remove_duplicate cast seen t in
            let q = make_partition cast seen q in
            if t = [] then q else t::q

(** Renumber random numbers into a contiguous 0..max_number segment.
    Takes as argument the [seen] Hashtable, so that the remap does not change relative order. *)
let renumber remap (i,j,k) = (Hashtbl.find remap i, j, k)
let renumber_partition seen p =
  let remap = Hashtbl.create 100 in
  let seen_list =
    Hashtbl.fold (fun i () c -> i::c) seen []
    |> List.sort Int.compare
  in
  List.iteri (fun i nb -> Hashtbl.add remap nb i) seen_list;
  List.map (List.map (renumber remap)) p, List.length seen_list

let to_z = Option.map Z.of_int

let mk_relation = function
  | None -> Model.Relation.Equal
  | Some n -> Model.Relation.of_int n

let not_empty = function [] -> false | _ -> true

type operation =
  | AddRelation of int Model.Node.t * int Model.Node.t * (int, int) Model.Relation.t
  | AddValue of int Model.Node.t * int Model.Value.t

let pp_op fmt = function
  | AddRelation(a,b,r) ->
        Format.fprintf fmt "@[add_relation %a %a %a@]"
        Model.Node.pretty a Model.Node.pretty b Model.Relation.debug_print r
  | AddValue(a, v) ->
        Format.fprintf fmt "@[add_value %a %a@]"
        Model.Node.pretty a Model.Value.debug_print v

let pp_oplist fmt l =
  let len = List.length l in
  let max_elems = 1000 in
  List.iteri (fun i elt ->
    if i = max_elems then Format.fprintf fmt "..."
    else if i > max_elems then ()
    else Format.fprintf fmt "%a%s@ " pp_op elt (if i = len - 1 then "" else ";")
  ) l

let mk_class repr acc (elt, rel, _) = AddRelation(repr, IntTerm elt, mk_relation rel)::acc

let mk_ops acc = function
  | [] -> assert false
  | (x, imin, iMax)::rest ->
      let node = Model.Node.IntTerm x in
      let acc =
        if imin = None && iMax = None
        then acc
        else AddValue(node, Model.Value.interval (to_z imin) (to_z iMax))::acc in
      List.fold_left (mk_class node) acc rest

let mk_partition gen =
  let seen = Hashtbl.create 100 in
  let partition = make_partition (fun (i,_,_) -> i) seen gen in
  let partition, max_node_id = renumber_partition seen partition  in
  let partition = List.filter not_empty partition in
  List.fold_left mk_ops [] partition, max_node_id

let gen_partition =
  let open QCheck.Gen in
  let nb_classes = 10 -- 100 in
  let class_size = 1 -- 20 in
  triple int (option int) (option int) (* Element: id, info_a, info_b: where info_a/info_b is used to generate the relation or the value *)
  |> list_size class_size (* generate a class *)
  |> list_size nb_classes
  |> map mk_partition

let shrink_relation = function
  | Model.Relation.Equal -> QCheck.Iter.return Model.Relation.Equal
  | Model.Relation.Add z ->
      Z.to_int z
      |> QCheck.Shrink.int
      |> QCheck.Iter.map (fun x -> mk_relation (Some x))

(* let shrink_value = function
  | Model.Value.Empty -> QCheck.Iter.return Model.Value.Empty
  | Model.Value.Interval (l,r) ->  *)

let shrink_op = function
  | AddRelation(i,j,r) -> QCheck.Iter.map (fun r -> AddRelation(i,j,r)) (shrink_relation r)
  | AddValue _ as x -> QCheck.Iter.return x


let shrink (l, s) =
  QCheck.Iter.pair
  (QCheck.Shrink.list l)
  (QCheck.Iter.return s)

let print (l, s) = Format.asprintf "[@[<hov>%a@]] size: %d" pp_oplist l s

let partition = QCheck.make ~shrink ~print gen_partition

let gen_relation max =
  let open QCheck.Gen in
  let* n1 = 0 -- (max - 1) in
  let* n2 = 0 -- (max - 1) in
  let+ rel = option int in
  AddRelation(Model.Node.IntTerm n1, Model.Node.IntTerm n2, mk_relation rel)

let gen_value max =
  let open QCheck.Gen in
  let* n = 0 -- (max - 1) in
  let* imin = option int in
  let+ iMax = option int in
  AddValue(Model.Node.IntTerm n, Model.Value.interval (to_z imin) (to_z iMax))

let gen_op_list size =
  let open QCheck.Gen in
  (oneof_weighted [
    (5, gen_relation size);
    (1, gen_value size)
  ])
  |> list_size (10 -- 100)

let gen_partition_with_split =
  let open QCheck.Gen in
  let* (partition, size) = gen_partition in
  tup4 (return partition) (return size) (gen_op_list size) (gen_op_list size)

let shrink (l, s, cl, cr) yield =
  (QCheck.Shrink.list l) (fun x -> yield (x,s,cl,cr));
  (QCheck.Shrink.list cl) (fun x -> yield (l,s,x,cr));
  (QCheck.Shrink.list cr) (fun x -> yield (l,s,cl,x))

let print (l, s, cl, cr) = Format.asprintf "@[<v>let common = [@[<hov>%a@]]@,let size = %d@,let left = [@[<hov>%a@]]@,let right = [@[<hov>%a@]]@]"
  pp_oplist l s pp_oplist cl pp_oplist cr

let partition_with_split = QCheck.make ~shrink ~print gen_partition_with_split
