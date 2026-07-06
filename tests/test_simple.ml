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

module Model = Testing_utils.Model

let model_op ((model, err) as acc) = function
  | Generator.AddValue(i,v) -> Model.add_value model i v; acc
  | Generator.AddRelation(i,j,_) -> match Model.add_relation model i j Model.Relation.Equal with
      | Ok () -> acc
      | Error r -> (model, (i,j,r)::err)

(** [forall f n] is [f 0 && ... && f n] *)
let rec forall f = function
  | n when n < 0 -> true
  | n when f n -> (forall [@tailcall]) f (n-1)
  | n -> false


module MkTest(S: sig val name: string val is_array: bool val valued: bool end)(
  UF: Union_Find_Lattice.Sig.VALUED_UNION_FIND_LATTICE
        with type node = int Model.Node.t
         and type value = int Model.Value.t
) =
struct
  let iname = S.name

  let count = 100

  let opt_copy = if S.is_array then UF.copy else Fun.id

  let name x = "simple." ^ iname ^ "." ^ x

  let uf_op uf = function
  | Generator.AddValue (i,v) -> UF.set_value ~intersect:true uf i v
  | Generator.AddRelation(i,j,_) -> UF.union uf i j

  let assert_eq ?(msg="") pp l r =
    if l = r then true else
    let msg = if msg <> "" then msg^"\n" else msg in
    QCheck.Test.fail_reportf "%sEquality assert fails: %a != %a" msg pp l pp r

  let is_none = function
    | None -> true
    | Some msg -> QCheck.Test.fail_report msg

  (** Test that [UF.make] satisfies its invariants. *)
  let test_make =
    QCheck.Test.make ~count ~name:(name "make")
    QCheck.(2 -- 100)
    (function size -> UF.make size |> UF.check_invariants |> is_none)

  (** Test that the [uf_op] chain of add_value/add_relation does not break any invariants. *)
  let test_union_chain =
    QCheck.Test.make ~count ~name:(name "union_chain")
    Generator.partition
    (function (ops, size) ->
        let uf = List.fold_left uf_op (UF.make size) ops in
        UF.check_invariants uf |> is_none)

  (** Test that, for all [x] performing [UF.find uf x] is an element related to
      [x] in the model, via the same relation. *)
  let test_find =
    QCheck.Test.make ~count ~name:(name "find")
    Generator.partition
    (function (ops, size) ->
        let uf = List.fold_left uf_op (UF.make size) ops in
        let model, err = List.fold_left model_op (Model.make size, []) ops in err = [] &&
        let test_pos i =
          let node = Model.Node.IntTerm i in
          let parent = UF.find uf node in
          match Model.check_related model node parent with
          | Some Model.Relation.Equal -> true
          | Some _ ->  QCheck.Test.fail_report "Wrong relation in model"
          | None -> QCheck.Test.fail_report "Element not related to its representative"
        in forall test_pos (size-1))

  (** Test that [UF.check_related uf x x] is always identity. *)
  let test_self_related =
    QCheck.Test.make ~count ~name:(name "self_related")
    Generator.partition
    (function (ops, size) ->
        let uf = List.fold_left uf_op (UF.make size) ops in
        let test_pos i =
          let node = Model.Node.IntTerm i in
          UF.check_related uf node node
        in
        forall test_pos (size-1))

  (** Checks that model and uf agree by checking [get_relation a b] for most arbitrary pairs *)
  let check_relations model uf size =
    let scale_a = 3 in (* larger = faster but fewer checks*)
    let scale_b = 14 in (* larger = faster but fewer checks*)
    let test_pos i j =
      let i = i*scale_a in
      let j = j*scale_b in
      let nodei = Model.Node.IntTerm i in
      let nodej = Model.Node.IntTerm j in
      assert_eq
        ~msg:(Format.sprintf "Relation mismatch between nodes %d and %d: " i j)
        Format.pp_print_bool
        (Model.check_related model nodei nodej |> Option.is_some)
        (UF.check_related uf nodei nodej)
    in forall (fun i -> forall (test_pos i) ((i-1)/scale_b)) ((size-1) / scale_a)

  (** Checks that all relation from [uf_a] are in [uf_b] by checking [get_relation a b]
      for most arbitrary pairs *)
  let check_relations_subset uf_a uf_b size =
    let scale_a = 3 in (* larger = faster but fewer checks*)
    let scale_b = 14 in (* larger = faster but fewer checks*)
    let test_pos i j =
      let nodei = Model.Node.IntTerm (i*scale_a) in
      let nodej = Model.Node.IntTerm (j*scale_b) in
      if UF.check_related uf_a nodei nodej then UF.check_related uf_b nodei nodej else true
    in forall (fun i -> forall (test_pos i) ((i-1)/scale_b)) ((size-1) / scale_a)

  (** Checks that related in [uf_join] iff related in [uf_a] and [uf_b] by checking [get_relation a b]
      for most arbitrary pairs *)
  let check_relations_join uf_a uf_b uf_join size =
    let scale_a = 3 in (* larger = faster but fewer checks*)
    let scale_b = 14 in (* larger = faster but fewer checks*)
    let test_pos i j =
      let nodei = Model.Node.IntTerm (i*scale_a) in
      let nodej = Model.Node.IntTerm (j*scale_b) in
      let related_a = UF.check_related uf_a nodei nodej in
      let related_b = UF.check_related uf_b nodei nodej in
      let related_j = UF.check_related uf_join nodei nodej in
      if related_a && related_b then related_j else not related_j
    in forall (fun i -> forall (test_pos i) ((i-1)/scale_b)) ((size-1) / scale_a)

  let check_relations_meet uf_a uf_b uf_join size =
    let scale_a = 3 in (* larger = faster but fewer checks*)
    let scale_b = 14 in (* larger = faster but fewer checks*)
    let test_pos i j =
      let nodei = Model.Node.IntTerm (i*scale_a) in
      let nodej = Model.Node.IntTerm (j*scale_b) in
      let related_a = UF.check_related uf_a nodei nodej in
      let related_b = UF.check_related uf_b nodei nodej in
      let related_j = UF.check_related uf_join nodei nodej in
      if related_a || related_b then related_j else true
    in forall (fun i -> forall (test_pos i) ((i-1)/scale_b)) ((size-1) / scale_a)

  (** test that [check_related] are the same in UF and the model *)
  let test_related =
    QCheck.Test.make ~count ~name:(name "related")
    Generator.partition
    (function (ops, size) ->
        let uf = List.fold_left uf_op (UF.make size) ops in
        let model, err = List.fold_left model_op (Model.make size, []) ops in err = [] &&
        check_relations model uf size
      )

  let test_self_incl =
    QCheck.Test.make ~count ~name:(name "self_incl")
    Generator.partition
    (function (ops, size) ->
        let uf = List.fold_left uf_op (UF.make size) ops in
        UF.incl uf uf
      )

  let test_persistent_find =
    QCheck.Test.make ~count ~name:(name "persistent_find")
    Generator.partition_with_split
    (fun (ops, size, op_l, op_r) ->
        let uf_c = List.fold_left uf_op (UF.make size) ops in
        let uf_l = List.fold_left uf_op (opt_copy uf_c) op_l in
        let uf_r = List.fold_left uf_op (opt_copy uf_c) op_r in
        let model_c, err = List.fold_left model_op (Model.make size, []) ops in err = [] &&
        let model_l, m_err_l = List.fold_left model_op (Model.copy model_c, []) op_l in
        let model_r, m_err_r = List.fold_left model_op (Model.copy model_c, []) op_r in
        [] = m_err_l && [] = m_err_r
        && check_relations model_c uf_c size
        && check_relations model_l uf_l size
        && check_relations model_r uf_r size
      )

  let test_join_invariants =
    QCheck.Test.make ~count ~name:(name "join_invariants")
    Generator.partition_with_split
    (fun (ops, size, op_l, op_r) ->
        let uf_c = List.fold_left uf_op (UF.make size) ops in
        let uf_l = List.fold_left uf_op (opt_copy uf_c) op_l in
        let uf_r = List.fold_left uf_op (opt_copy uf_c) op_r in
        let join = UF.join uf_l uf_r in
        UF.check_invariants join |> is_none)

  let test_join_relations =
    QCheck.Test.make ~count ~name:(name "join_relations")
    Generator.partition_with_split
    (fun (ops, size, op_l, op_r) ->
        let uf_c = List.fold_left uf_op (UF.make size) ops in
        let uf_l = List.fold_left uf_op (opt_copy uf_c) op_l in
        let uf_r = List.fold_left uf_op (opt_copy uf_c) op_r in
        let join = UF.join uf_l uf_r in
        let model_c, err = List.fold_left model_op (Model.make size, []) ops in err = [] &&
        let model_l, _ = List.fold_left model_op (Model.copy model_c, []) op_l in
        let model_r, _ = List.fold_left model_op (Model.copy model_c, []) op_r in
        let model_join = Model.join model_l model_r in
        check_relations model_c uf_c size
        && check_relations model_l uf_l size
        && check_relations model_r uf_r size
        && check_relations model_join join size)

  let test_join_contains_common =
    QCheck.Test.make ~count ~name:(name "join_contains_common")
    Generator.partition_with_split
    (fun (ops, size, op_l, op_r) ->
        let uf_c = List.fold_left uf_op (UF.make size) ops in
        let uf_l = List.fold_left uf_op (opt_copy uf_c) op_l in
        let uf_r = List.fold_left uf_op (opt_copy uf_c) op_r in
        let join = UF.join uf_l uf_r in
        check_relations_subset uf_c join size)

  let test_incl_child =
    QCheck.Test.make ~count ~name:(name "incl_child")
    Generator.partition_with_split
    (fun (ops, size, op_l, op_r) ->
        let uf_c = List.fold_left uf_op (UF.make size) ops in
        let uf_l = List.fold_left uf_op (opt_copy uf_c) op_l in
        UF.incl uf_l uf_c)

  let test_join_is_and =
    QCheck.Test.make ~count ~name:(name "join_contains_is_and")
    Generator.partition_with_split
    (fun (ops, size, op_l, op_r) ->
        let uf_c = List.fold_left uf_op (UF.make size) ops in
        let uf_l = List.fold_left uf_op (opt_copy uf_c) op_l in
        let uf_r = List.fold_left uf_op (opt_copy uf_c) op_r in
        let join = UF.join uf_l uf_r in
        check_relations_join uf_l uf_r join size)

  let test_join_incl =
    QCheck.Test.make ~count ~name:(name "join_incl")
    Generator.partition_with_split
    (fun (ops, size, op_l, op_r) ->
        let uf_c = List.fold_left uf_op (UF.make size) ops in
        let uf_l = List.fold_left uf_op (opt_copy uf_c) op_l in
        let uf_r = List.fold_left uf_op (opt_copy uf_c) op_r in
        let join = UF.join uf_l uf_r in
        UF.incl uf_l join && UF.incl uf_r join)

  let test_meet_incl =
    QCheck.Test.make ~count ~name:(name "meet_incl")
    Generator.partition_with_split
    (fun (ops, size, op_l, op_r) ->
        let uf_c = List.fold_left uf_op (UF.make size) ops in
        let uf_l = List.fold_left uf_op (opt_copy uf_c) op_l in
        let uf_r = List.fold_left uf_op (opt_copy uf_c) op_r in
        let meet = UF.meet uf_l uf_r in
        UF.incl meet uf_l && UF.incl meet uf_r)

  let test_meet_contains_or =
    QCheck.Test.make ~count ~name:(name "meet_contains_or")
    Generator.partition_with_split
    (fun (ops, size, op_l, op_r) ->
        let uf_c = List.fold_left uf_op (UF.make size) ops in
        let uf_l = List.fold_left uf_op (opt_copy uf_c) op_l in
        let uf_r = List.fold_left uf_op (opt_copy uf_c) op_r in
        let meet = UF.meet uf_l uf_r in
        check_relations_meet uf_l uf_r meet size)

  (** check that model and uf agree on values *)
  let check_values model uf size =
    let test_pos i =
      let node = Model.Node.IntTerm i in
      assert_eq
        ~msg:(Format.asprintf "get_value %d:" i) (Utils.Functions.pp_option (Model.Value.pretty node))
        (Model.get_value model node) (UF.get_value uf node)
    in forall test_pos (size-1)

  let test_get_value =
    QCheck.Test.make ~count ~name:(name "get_value")
    Generator.partition
    (function (ops, size) ->
        let uf = List.fold_left uf_op (UF.make size) ops in
        let model, err = List.fold_left model_op (Model.make size, []) ops in err = [] &&
        check_values model uf size)
  let test_persistent_get_value =
    QCheck.Test.make ~count ~name:(name "persistent_get_value")
    Generator.partition_with_split
    (fun (ops, size, op_l, op_r) ->
        let uf_c = List.fold_left uf_op (UF.make size) ops in
        let uf_l = List.fold_left uf_op (opt_copy uf_c) op_l in
        let uf_r = List.fold_left uf_op (opt_copy uf_c) op_r in
        let model_c, err = List.fold_left model_op (Model.make size, []) ops in err = [] &&
        let model_l, _ = List.fold_left model_op (Model.copy model_c, []) op_l in
        let model_r, _ = List.fold_left model_op (Model.copy model_c, []) op_r in
        check_values model_c uf_c size
        && check_values model_l uf_l size
        && check_values model_r uf_r size)

  let test_join_values =
    QCheck.Test.make ~count ~name:(name "join_values")
    Generator.partition_with_split
    (fun (ops, size, op_l, op_r) ->
        let uf_c = List.fold_left uf_op (UF.make size) ops in
        let uf_l = List.fold_left uf_op (opt_copy uf_c) op_l in
        let uf_r = List.fold_left uf_op (opt_copy uf_c) op_r in
        let join = UF.join uf_l uf_r in
        let model_c, err = List.fold_left model_op (Model.make size, []) ops in err = [] &&
        let model_l, _ = List.fold_left model_op (Model.copy model_c, []) op_l in
        let model_r, _ = List.fold_left model_op (Model.copy model_c, []) op_r in
        let model_join = Model.join model_l model_r in
        (* check_values model_c uf_c size
        && check_values model_l uf_l size
        && check_values model_r uf_r size *)
        check_values model_join join size)


  let tests = [
      test_make;
      test_union_chain;
      test_find;
      test_self_related;
      test_related;
      test_self_incl;
      test_persistent_find;
      test_join_invariants;
      test_join_relations;
      test_join_contains_common;
      test_join_is_and;
      test_join_incl;
      test_meet_incl;
      test_incl_child;
      test_meet_contains_or
    ]@ (if S.valued then [
      test_get_value;
      test_persistent_get_value;
      test_join_values;
    ] else [])
end

module ValuedOfUnvalued(F : Union_Find_Lattice.Sig.UNION_FIND_LATTICE) = struct
include F

type value = int Model.Value.t
let get_value _ _ = None
let set_value ~intersect uf _ _ = uf
end

open Testing_utils

let tests = List.concat_map (fun (x: (module Functor_sig.UNION_FIND_LATTICE_FUNCTOR) Instances.with_id) ->
  let module F = (val x.instance) in
  let module U = F(Model.INode) in
  let module UF = MkTest(struct
      let name = x.name
      let valued = x.has_value
      let is_array = x.is_array
    end)(ValuedOfUnvalued(U)) in
  UF.tests
  ) Testing_utils.Instances.union_finds

let tests = tests @ List.concat_map (fun (x: (module Functor_sig.VALUED_UNION_FIND_LATTICE_FUNCTOR) Instances.with_id) ->
  let module F = (val x.instance) in
  let module U = F(Model.INode)(Model.IValue) in
  let module UF = MkTest(struct
      let name = x.name
      let valued = x.has_value
      let is_array = x.is_array
    end)(U) in
  UF.tests) Testing_utils.Instances.valued

(* let tests = []


let mkn n = Model.Node.IntTerm n
let mkr r = Generator.mk_relation (Some r)
let itv i i' = Model.Value.interval (Option.map Z.of_int i)  (Option.map Z.of_int i')
let add_relation i j r = Generator.AddRelation(mkn i, mkn j, mkr r)
let add_value n i i' = Generator.AddValue(mkn n, itv i i')


let ops=  [ ]

let size = 4
let op_l = []
let op_r = [add_relation 3 2 0]


module Config = struct
  let path_compression = `Lazy
  let hash_consed = false
  let join = `DiffParentEdit
  let union_strategy = `Sorted
  let extendable = false
end

module UF = Union_Find_Lattice.Persistent_array.Make(Config)(Model.INode)()

let uf_op uf = function
| Generator.AddValue (i,v) -> uf
| Generator.AddRelation(i,j,_) -> UF.union uf i j
let check_relations model uf size =
  let scale_a = 1 in (* larger = faster but fewer checks*)
  let scale_b = 1 in (* larger = faster but fewer checks*)
  let test_pos i j =
    let nodei = Model.Node.IntTerm (i*scale_a) in
    let nodej = Model.Node.IntTerm (j*scale_b) in
    (* Format.printf "test_pos %d %d %a@." i j (Union_Find_Lattice.Utils.Functions.pp_option Model.Relation.pretty) (Model.check_related model nodei nodej); *)
      (Model.check_related model nodei nodej |> Option.is_some) = (UF.check_related uf nodei nodej)
  in
  let b = forall (fun i -> forall (test_pos i) ((i-1)/scale_b)) ((size-1) / scale_a) in
  Format.printf "Done one check@."; b

let () =
  let uf_c = List.fold_left uf_op (UF.make size) ops in
  let uf_l = List.fold_left uf_op (uf_c) op_l in
  let uf_r = List.fold_left uf_op (uf_c) op_r in
  let join = UF.join uf_l uf_r in
  let model_c, err = List.fold_left model_op (Model.make size, []) ops in
  let model_l, _ = List.fold_left model_op (Model.copy model_c, []) op_l in
  let model_r, _ = List.fold_left model_op (Model.copy model_c, []) op_r in
  let model_join = Model.join model_l model_r in
  Format.printf "C: @[%a@]@." UF.pretty uf_c;
  Format.printf "L: @[%a@]@." UF.pretty uf_l;
  Format.printf "R: @[%a@]@." UF.pretty uf_r;
  Format.printf "J: @[%a@]@." UF.pretty join;
  (* assert (UF.incl uf_l join && UF.incl uf_r join);
  match UF.check_invariants join with
  | None -> ()
  | Some x -> Format.printf "%s@." x *)
  assert(check_relations model_c uf_c size
        && check_relations model_l uf_l size
        && check_relations model_r uf_r size
        && check_relations model_join join size) *)
