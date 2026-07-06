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


let () = Random.self_init ()

module UF = struct

  type cluster = { rank: int; size: int }

  type t = {
    uf: (int, int) Hashtbl.t;
    roots: (int, cluster) Hashtbl.t;
    linked: int Dynarray.t;
  }

  let make () = {
    uf=Hashtbl.create 1000;
    roots=Hashtbl.create 100;
    linked=Dynarray.create ();
  }

  let rec find t i = match Hashtbl.find t.uf i with
    | j when j <> i -> find t j
    | _ -> i
    | exception Not_found -> i

  let related t i j = find t i = find t j

  let stats t n = match Hashtbl.find t.roots n with
    | c -> c
    | exception Not_found -> { rank=0; size=1 }

  let seen t i = Hashtbl.mem t.uf i || Hashtbl.mem t.roots i

  let union t i j =
    let i = find t i in
    let j = find t j in
    if i <> j then
    let is = stats t i in
    let js = stats t j in
    let size = is.size + js.size in
    if not (seen t i) then Dynarray.add_last t.linked i;
    if not (seen t j) then Dynarray.add_last t.linked j;
    if is.rank < js.rank then (
      Hashtbl.replace t.uf i j;
      Hashtbl.replace t.roots j { js with size };
      Hashtbl.remove t.roots i
    )
    else (
      let rank = if is.rank = js.rank then is.rank + 1 else is.rank in
      Hashtbl.replace t.uf j i;
      Hashtbl.replace t.roots i { rank; size };
      Hashtbl.remove t.roots j
    )

  type stats = {
    cluster_sizes: (int, Stats.exhaustive) Stats.t;
    cluster_ranks: (int, Stats.exhaustive) Stats.t
  }

  let get_stats uf =
    let cluster_ranks, cluster_sizes = Hashtbl.fold (fun _ {rank;size} (ranks, sizes) -> rank::ranks, size::sizes) uf.roots ([], []) in
    { cluster_ranks=Stats.exhaustive_of_int_list cluster_ranks;
      cluster_sizes=Stats.exhaustive_of_int_list cluster_sizes; }

  let pp_stats fmt uf =
    let { cluster_sizes; cluster_ranks } = get_stats uf in
    let open Stats in
    Format.fprintf fmt "Clusters: %4d;  Size: %d - %d (avg: %.2f ±%d%%);  Ranks: %d - %d (avg: %.2f ±%d%%)@."
      (size cluster_sizes)
      (min cluster_sizes) (max cluster_sizes)
      (average cluster_sizes) (standard_deviation_percent cluster_sizes |> int_of_float)
      (min cluster_ranks) (max cluster_ranks)
      (average cluster_ranks) (standard_deviation_percent cluster_ranks |> int_of_float)
end

module Make(S: sig
  type node
  type value
  type relation

  val node_of_int: int -> node
  val gen_node: int -> node
  val gen_relation: unit -> relation
  val gen_value: unit -> value
end) = struct
  include S

  type operation =
    | Find of node
    | Union of node * node * relation
    | GetValue of node
    | SetValue of node * value

  let gen_find ~nb_nodes = S.gen_node nb_nodes

  let rec simple_gen_union ~nb_nodes =
    let a = Random.int nb_nodes in
    let b = Random.int nb_nodes in
      (S.node_of_int a, S.node_of_int b, S.gen_relation ())

  let rec gen_union uf ~bias ~nb_nodes =
    let a = if bias then Dynarray.get uf.UF.linked (Random.int (Dynarray.length uf.UF.linked)) else Random.int nb_nodes in
    let b = Random.int nb_nodes in
    if UF.related uf a b then gen_union uf ~bias ~nb_nodes
    else
      let () = UF.union uf a b in
      (S.node_of_int a, S.node_of_int b, S.gen_relation ())

  let gen1 ~bias ~nb_nodes ~union_ratio ~nb_ops =
    let uf = UF.make () in
    List.init nb_ops (fun _ ->
      if Random.float 1.0 < union_ratio
      then
        let bias = Dynarray.length uf.UF.linked > 0 && Random.float 1.0 < bias in
        let a, b, rel = gen_union uf ~bias ~nb_nodes in
        Union (a,b,rel)
      else Find (gen_find ~nb_nodes)), uf

  let gen2 ~nb_nodes ~nb_ops =
      let uf = UF.make () in
      let merges = ref 0 in
      let ops = List.init nb_ops (fun _ ->
        let a = Random.int nb_nodes in
        let b = Random.int nb_nodes in
        if UF.related uf a b then incr merges;
        let () = UF.union uf a b in
        Union (S.node_of_int a, S.node_of_int b, S.gen_relation ()))
    in ops, !merges, uf
end

(* random int that may be negative*)
let random_int () =
  if Random.int 2 = 1
  then Random.full_int max_int
  else -(Random.full_int max_int) - 1

let random_z () = random_int () |> Z.of_int

(** Random option, generating 20% of [None] *)
let random_option rand_v () = if Random.int 5 == 0 then None else Some (rand_v ())

include Make(struct
  module Model = Testing_utils.Model
  type node = int Model.Node.t
  type value = int Model.Value.t
  type relation = (int,int) Model.Relation.t

  let node_of_int n = Model.Node.IntTerm n
  let gen_node n = Model.Node.IntTerm (Random.int n)
  let gen_value () = Model.Value.interval (random_option random_z ()) (random_option random_z ())
  let gen_relation () = Model.Relation.of_int (random_int ())
end)
