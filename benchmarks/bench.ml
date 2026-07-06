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
  time: 'a;
  allocated_bytes: 'a;
  major_words: 'a;
  promoted_words: 'a;
}

let bench ~gc ~func x =
  (* pre-run *)
  if gc then Gc.full_major ();
  let bytes = Gc.allocated_bytes () in
  let stat = Gc.quick_stat () in
  let time = (Unix.times ()).tms_utime in
  (* function call *)
  let res = func x in
  if Testing_utils.XP_config.time_gc then Gc.major ();
  (* post-run *)
  let end_time = (Unix.times ()).tms_utime in
  let time = end_time -. time in
  let memory = (Gc.allocated_bytes ()) -. bytes in
  let post_stat = Gc.quick_stat () in
  {
    time;
    allocated_bytes=memory;
    major_words=post_stat.major_words -. stat.major_words;
    promoted_words=post_stat.promoted_words -. stat.promoted_words
  }, res

let empty_stat = {
  time=Stats.compact_float_empty;
  allocated_bytes=Stats.compact_float_empty;
  major_words=Stats.compact_float_empty;
  promoted_words=Stats.compact_float_empty;
}

let sum result res = {
  time=result.time +. res.time;
  allocated_bytes=result.allocated_bytes +. res.allocated_bytes;
  major_words=result.major_words +. res.major_words;
  promoted_words=result.promoted_words +. res.promoted_words;
}

let add result res = {
  time=Stats.add_value result.time res.time;
  allocated_bytes=Stats.add_value result.allocated_bytes res.allocated_bytes;
  major_words=Stats.add_value result.major_words res.major_words;
  promoted_words=Stats.add_value result.promoted_words res.promoted_words;
}

let div x n = {
  time = x.time /. n;
  allocated_bytes = x.allocated_bytes /. n;
  major_words = x.major_words /. n;
  promoted_words = x.promoted_words /. n;
}

module Model = Testing_utils.Model

let apply_ops (type a) (module UF: Testing_utils.XP_config.UFL with type t = a) ops (x: a) =
  let apply_op x = function
    | Gen.Find n -> ignore (UF.find x n); x
    | Gen.GetValue _ -> x
    | Gen.SetValue _ -> x
    | Gen.Union(i,j,_) -> UF.union x i j
  in
  List.fold_left apply_op x ops

let apply_2_ops (type a) (module UF: Testing_utils.XP_config.UFL with type t = a) is_array init x y =
  let opt_copy = if is_array then UF.copy else Fun.id in
  let x = apply_ops (module UF) x (opt_copy init) in
  let y = apply_ops (module UF) y (opt_copy init) in
  (x,y)

let apply_4_ops (type a) (module UF: Testing_utils.XP_config.UFL with type t = a) is_array init x y z t =
  let opt_copy = if is_array then UF.copy else Fun.id in
  let x = apply_ops (module UF) x (opt_copy init) in
  let y = apply_ops (module UF) y (opt_copy init) in
  let z = apply_ops (module UF) z (opt_copy init) in
  let t = apply_ops (module UF) t (opt_copy init) in
  (x,y,z,t)

(** We run multiple benchmarks on a given input:
    {v
          i <- init
          | <- build_common
          c
         /|\   <- build_branches
        x y z

        join := join x y;
        join_root := join x c;
        double_join := join (join x y) z
    v}

*)
type single_run = {
  init: float bench_result; (** stats for [make n] *)
  build_common: float bench_result; (** stats for building the initial branch *)
  build_branches: float bench_result; (** stats for building the left/right branches *)

  join: float bench_result; (** stats for the join of two branches *)
  join_root: float bench_result; (** stats for a join with the root *)
  double_join: float bench_result; (** stats for a join on top of a join *)
  triple_join: float bench_result;

  meet: float bench_result;
  meet_root: float bench_result;
  double_meet: float bench_result;
  triple_meet: float bench_result;

  incl: float bench_result;
  incl_root: float bench_result;

  find: float bench_result; (** stats for 10 find queries *)
  union: float bench_result; (** stats for 10 unions *)
}
type single_gen = {
  avg_class_size: float;
  max_class_size: int;
  max_class_rank: int;
  nb_classes: int;
  nb_duplicate_unions: int;
}

let not_implemented = { time = -1.; allocated_bytes= -1.; major_words = -1.; promoted_words = -1. }
let timeout = { time = -2.; allocated_bytes= -2.; major_words = -2.; promoted_words = -2. }


let single_run ~simple ~nb_nodes ~nb_ops ~nb_branch_ops plv =
  let common, nb_duplicate_unions, uf = Gen.gen2 ~nb_nodes ~nb_ops in
  let x, _, _ = Gen.gen2 ~nb_nodes ~nb_ops:nb_branch_ops in
  let y, _, _ = Gen.gen2 ~nb_nodes ~nb_ops:nb_branch_ops in
  let z, _, _ = Gen.gen2 ~nb_nodes ~nb_ops:nb_branch_ops in
  let t, _, _ = Gen.gen2 ~nb_nodes ~nb_ops:nb_branch_ops in
  (* let find = List.init 10 (fun _ -> Gen.gen_find ~nb_nodes) in
  let union = List.init 100 (fun _ -> Gen.simple_gen_union ~nb_nodes) in *)
  let gc_init, gc_branch, gc_other = match Testing_utils.XP_config.gc with
  | Testing_utils.XP_config.Always -> true,true,true
  | Once -> true,false,false
  | OnBranch -> true,true,false
  | Never -> false,false,false
  in
  List.map (fun (uf : (module Testing_utils.XP_config.UFL) Testing_utils.Instances.with_id) ->
    let module UF = (val uf.instance) in
    let init, init_value = bench ~gc:gc_init ~func:UF.make nb_nodes in
    let build_common, common = bench ~gc:gc_other ~func:(apply_ops (module UF) common) init_value in
    let build_branches, (x, y, z, t) =
      if simple
      then
        let b, (x,y) = bench ~gc:gc_other ~func:(apply_2_ops (module UF) uf.is_array common x) y in
        b, (x,y,x,y)
      else bench ~gc:gc_other ~func:(apply_4_ops (module UF) uf.is_array common x y z) t in

    let join, triple_join, dj =
      let join, j = bench ~gc:gc_branch ~func:(UF.join x) y in
      if simple
      then join, not_implemented, j
      else
        let dj = UF.join j z in
        let dr = UF.join z t in
        let triple_join, _ = bench ~gc:gc_other ~func:(UF.join dj) dr in
        join, triple_join, dj
    in

    (* let find, _ = bench ~gc:gc_branch ~func:(List.iter (fun i -> ignore (UF.find dj i))) find in
    let union, _ = bench ~gc:gc_other ~func:(List.iter (fun (i,j,_) -> ignore (UF.union dj i j))) union in *)

    let meet = bench ~gc:gc_branch ~func:(UF.meet x) y |> fst in

    let incl, _ = bench ~gc:gc_branch ~func:(UF.incl x) y in
    let incl_root, _ = bench ~gc:gc_other ~func:(UF.incl x) common in

    { init; build_common; build_branches;
      join; join_root=not_implemented; double_join=not_implemented; triple_join; find=not_implemented; union=not_implemented;
      meet; meet_root=not_implemented; double_meet=not_implemented; triple_meet=not_implemented; incl; incl_root}) plv,
    if nb_ops = 0 then
            let open Gen.UF in
      { nb_duplicate_unions; avg_class_size=0.; max_class_size=0;
        nb_classes=0; max_class_rank=0 }
    else
      let uf_stat = Gen.UF.get_stats uf in
      let open Gen.UF in
      { nb_duplicate_unions; avg_class_size=Stats.average uf_stat.cluster_sizes; max_class_size=Stats.max uf_stat.cluster_sizes;
        nb_classes=Stats.size uf_stat.cluster_sizes; max_class_rank=Stats.max uf_stat.cluster_ranks }

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

let empty = {
  init=empty_stat;
  build_common=empty_stat;

  build_branches=empty_stat;
  build=empty_stat;
  join=empty_stat;
  join_root=empty_stat;
  double_join=empty_stat;
  triple_join=empty_stat;
  join_all=empty_stat;
  meet=empty_stat;
  double_meet=empty_stat;
  triple_meet=empty_stat;
  meet_root=empty_stat;
  meet_all=empty_stat;
  incl=empty_stat;
  incl_root=empty_stat;
  find=empty_stat;
  build_join=empty_stat;
  build_meet=empty_stat;
  union=empty_stat;
  total=empty_stat;
}

let empty_gen_info = {
  avg_class_sizes=Stats.compact_float_empty;
  max_class_size=Stats.compact_int_empty;
  max_class_rank=Stats.compact_int_empty;
  nb_classes=Stats.compact_int_empty;
  nb_duplicate_unions=Stats.compact_int_empty;
}


let quad f a b c d = f (f a b) (f c d)
let ternary f a b c = f a (f b c)

let append (m : run_result) (s: single_run) : run_result =
  let build = sum s.build_common s.build_branches in {
    init = add m.init s.init;

    build_common = add m.build_common s.build_common;
    build_branches = add m.build_branches s.build_branches;
    build = add m.build build;

    find = add m.find s.find;
    union = add m.union s.union;

    join = add m.join s.join;
    join_root = add m.join_root s.join_root;
    double_join = add m.double_join s.double_join;
    triple_join = add m.triple_join s.triple_join;
    join_all = add m.join_all (quad sum s.join s.join_root s.double_join s.triple_join);

    meet = add m.meet s.meet;
    meet_root = add m.meet_root s.meet_root;
    double_meet = add m.double_meet s.double_meet;
    triple_meet = add m.triple_meet s.triple_meet;
    meet_all = add m.meet_all (quad sum s.meet s.meet_root s.double_meet s.triple_meet);

    incl = add m.incl s.incl;
    incl_root = add m.incl_root s.incl_root;

    build_join = add m.build_join (sum s.join build);
    build_meet = add m.build_meet (sum s.meet build);
    total = add m.total (quad sum s.init build s.join s.meet);
  }

let combine_gen_info (i: gen_info) (j: single_gen) : gen_info = {
  avg_class_sizes=Stats.add_value i.avg_class_sizes j.avg_class_size;
  max_class_size=Stats.add_value i.max_class_size j.max_class_size;
  max_class_rank=Stats.add_value i.max_class_rank j.max_class_rank;
  nb_classes=Stats.add_value i.nb_classes j.nb_classes;
  nb_duplicate_unions=Stats.add_value i.nb_duplicate_unions j.nb_duplicate_unions;
}

let rec repeat_runs ~simple ((runs, gens) as acc) k ~nb_nodes ~nb_ops ~nb_branch_ops plv =
  (* Format.printf "%d,%.2f,%d@." nb_nodes union_ratio k; *)
  if k = 0 then acc
  else
    let runs', gen' = single_run ~simple ~nb_nodes ~nb_ops ~nb_branch_ops plv in
    let runs = List.map2 append runs runs' in
    let gens = combine_gen_info gens gen' in
    (repeat_runs [@tailcall]) ~simple (runs, gens) (k-1) ~nb_nodes ~nb_ops ~nb_branch_ops plv
let repeat_runs ~simple k ~nb_nodes ~nb_ops ~nb_branch_ops plv
  = repeat_runs ~simple (List.init (List.length plv) (fun _ -> empty), empty_gen_info) k ~nb_nodes ~nb_ops ~nb_branch_ops plv
