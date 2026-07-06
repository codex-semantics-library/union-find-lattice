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

type 'a bench_result = {
  time: 'a; (** time is in seconds*)
  allocated_bytes: 'a; (** in bytes *)
  major_words: 'a;
  promoted_words: 'a;
}

val empty_stat: (float, Stats.compact) Stats.t bench_result
(** Statisctis for an empty bench result *)

val sum: float bench_result -> float bench_result -> float bench_result
(** Sum to results, i.e. obtain the result equivalent to running both n succession. *)

val div: float bench_result -> float -> float bench_result
(** [div res n] divides all components by [n]. *)

val add: (float, Stats.compact) Stats.t bench_result -> float bench_result -> (float, Stats.compact) Stats.t  bench_result
(** Add a value (i.e. a run) to a stats of bench_results *)

val bench: gc:bool -> func:('a -> 'b) -> 'a -> float bench_result * 'b
(** [bench ~func x] returns execution time and memory usage of function [func]
    applied to [x] *)

(** stats for mutliple runs with the same parameters *)
type stats = (float, Stats.compact) Stats.t

type run_result = {
  init: stats bench_result; (** stats for [make n] *)

  build_common: stats bench_result; (** stats for building the initial branch *)
  build_branches: stats bench_result; (** stats for building the left/right branches *)
  build: stats bench_result; (** [build_common + build_branches] *)

  join: stats bench_result; (** stats for the join of two branches *)
  join_root: stats bench_result; (** stats for a join with the root *)
  double_join: stats bench_result; (** stats for a join on top of a join *)
  triple_join: stats bench_result;
  join_all: stats bench_result; (** sum of the three joins *)

  meet: stats bench_result;
  meet_root: stats bench_result;
  double_meet: stats bench_result;
  triple_meet: stats bench_result;
  meet_all: stats bench_result;

  incl: stats bench_result;
  incl_root: stats bench_result;

  find: stats bench_result; (** stats for 10 find queries *)
  union: stats bench_result; (** stats for 10 unions *)

  build_join: stats bench_result;
  build_meet: stats bench_result;
  total: stats bench_result; (** [init + build + join] *)
}
type gen_info = {
  avg_class_sizes: (float, Stats.compact) Stats.t;
  max_class_size: (int, Stats.compact) Stats.t;
  max_class_rank: (int, Stats.compact) Stats.t;
  nb_classes: (int, Stats.compact) Stats.t;
  nb_duplicate_unions: (int, Stats.compact) Stats.t;
}

(** [repeat_run ~simple k ~nb_nodes ~nb_ops ~nb_branch implems] performs the run
    with the given parameters [k] times on all inputs [implems] and returns the results

    @param [~simple] specifies to only run a simplified (faster) bench set (one join instead of 4)
    @param [~nb_nodes] the total number of UF nodes
    @param [~nb_ops] the number of common unions
    @param [~nb_branch] the number of extra unions on each branch *)
val repeat_runs: simple:bool -> int ->
  nb_nodes:int ->
  nb_ops:int ->
  nb_branch_ops:int ->
  (module Testing_utils.XP_config.UFL) Testing_utils.Instances.with_id list ->
  run_result list * gen_info
