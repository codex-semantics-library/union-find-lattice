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


open Utils.Range

(** {1 Experiment setup}                                                                          *)
(**************************************************************************************************)
(** Use this file to customize what experiments to run                                            *)

let nb_iterations = 50
(** How often each experiment should be repeated. Fewer iterations is much faster,
    but can lead to higher variance. *)

type gc = Always | OnBranch | Once| Never
let gc = Always
(** How often to force a GC major clean: Always | OnBranch | Once | Never
    Fewer times is faster, but increases variability. *)

let time_gc = true
(** If true, include a GC call in the timed section.
    Should only be used with [gc = Always], else you could time the cleanup
    of stuff you did not allocate. *)

(** {1 Graph generation}                                                      *)
(******************************************************************************)

(** List of parameter (n,u,d) with
    - n the total number of nodes
    - u the number of union performed in the initial common ancestor
    - d the additional union performed per branch

[n_graph] is the graph whose x-axis is [n], its value of [d] is fixed,
and [u] is a fraction of [n].
[d_graph] is the graph whose x-axis is [d], with both [n] and [u] fixed *)
let n_graph_range = {start=25_000; stop=500_001; step=50_000}
let n_graph_udiv = 4
let n_graph_d = 5_000
let n_graph = to_list n_graph_range |>List.map (fun n -> (n, n/n_graph_udiv, n_graph_d))

let d_graph_range = { start=0; stop=200_001; step=10_000 }
let d_graph_n = 200_000
let d_graph_u = 50_000
let d_graph = (to_list d_graph_range |> List.map (fun d -> (d_graph_n, d_graph_u, d)))

let graph_outfile = "results.csv"
(** Output file *)


let graph_worklist = List.map (fun (n,u,o) -> (false,n,u,o,nb_iterations)) (d_graph @ n_graph)

module type UFL = Union_Find_Lattice.Sig.UNION_FIND_LATTICE
  with type node = int Model.Node.t

module Config = Union_Find_Lattice.DefaultConfig

module ConfigAltJoin = struct
  include Config
  let join = `DiffParentEdit
end

let mk_id (x: (module UFL)) ?(is_array=false) name = Instances.{
  instance=x;
  name;
  is_array;
  has_value=false;
}

(** Which union-finds to include in the graphs *)
let graph_suite : (module UFL) Instances.with_id list =
  let open Union_Find_Lattice in
  let open Model in [
    mk_id (module Classic.PatriciaTree(Config)(INode)) "PT";
    mk_id (module Classic.PersistentArray(Config)(INode)) "PA";
    mk_id (module Classic.PersistentArrayNCA(Config)(INode)) "PAN";
    (* (module Array_with_copy.Make(Config)(INode)()); *)
  ]

(** {1 Table generation}                                                      *)
(******************************************************************************)
(** The table requires fewer data points so it can run on slower algorithms   *)

let sample_worklist = [ (true, 200_000, 50_000, 5_000, 1); ]

let sample_outfile = "table.csv"

(** Which union-finds to include in the sample test, these run far less than the graph suite,
    and can thus include slower UF. *)
let sample_suite : _ list =
  let open Union_Find_Lattice in
  let open Model in
  (* graph_suite @ *)
  [
    mk_id (module Classic.ArrayWithCopy(Config)(INode)) "A";
    mk_id (module Classic.PatriciaTree(Config)(INode)) "PT";
    mk_id (module Classic.PersistentArray(Config)(INode)) "PA";
    mk_id (module Classic.PersistentArrayNCA(Config)(INode)) "PAN";
  ]
