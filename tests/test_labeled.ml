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
  | Generator.AddRelation(i,j,r) -> match Model.add_relation model i j r with
      | Ok () -> acc
      | Error r -> (model, (i,j,r)::err)

(** [forall f n] is [f 0 && ... && f n] *)
let rec forall f = function
  | n when n < 0 -> true
  | n when f n -> (forall [@tailcall]) f (n-1)
  | n -> false

module PolymorphicOfMonomorphic(UF : Testing_utils.Functor_sig.LABELED_VALUED_UNION_FIND_LATTICE_FUNCTOR)
= struct
  include UF(struct
      include Model.Node
      type nonrec t = int t
      let equal (IntTerm a) (IntTerm b) = Int.equal a b
    end)(struct
      include Model.Relation
      type nonrec t = (int, int) t
    end)(struct
      include Model.Value
      type nonrec node = int node
      type nonrec t = int t
      type nonrec relation = (int, int) relation
    end)

  type 'a node = 'a Model.Node.t
  type ('a, 'b) relation = ('a, 'b) Model.Relation.t
  type 'a value = 'a Model.Value.t

  type 'a find_result = FindResult: {
    representative: 'b node;
    relation: ('a, 'b) relation;
  } -> 'a find_result
  let find : type a. t -> a node -> a find_result = fun u (IntTerm _ as t) ->
    let representative, relation = find u t in FindResult { representative; relation }
  let add_relation: type a b. t -> a node -> b node -> (a,b) relation -> (t, (a,b) relation) result = fun t (IntTerm _ as x) (IntTerm _ as y) ->
    add_relation t x y
  let check_related: type a b. t -> a node -> b node -> (a,b) relation option = fun t (IntTerm _ as x) (IntTerm _ as y) ->
    check_related t x y

  let get_value: type a. t -> a node -> a value option = fun t (IntTerm _ as x) -> get_value t x
  let set_value: type a. intersect:bool -> t -> a node -> a value -> _ = fun ~intersect t (IntTerm _ as x) -> set_value ~intersect t x
end

module TestPolymorphic(S: sig val name: string val is_array: bool val valued: bool end)(
  UF: Union_Find_Lattice.Sig.POLYMORPHIC_LABELED_VALUED_UNION_FIND_LATTICE
        with type 'a node = 'a Model.Node.t
         and type 'a value = 'a Model.Value.t
         and type ('a, 'b) relation = ('a, 'b) Model.Relation.t
) =
struct
  let iname =  S.name

  let count = 100

  let opt_copy = if S.is_array then UF.copy else Fun.id

  let name x = "polymorphic." ^ iname ^ "." ^ x

  let uf_op (uf, err) = function
  | Generator.AddValue(i,v) -> UF.set_value ~intersect:true uf i v, err
  | Generator.AddRelation(i,j,r) -> match UF.add_relation uf i j r with
      | Ok x -> x, err
      | Error r -> (uf, (i,j,r)::err)

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
        let uf, err = List.fold_left uf_op (UF.make size, []) ops in
        UF.check_invariants uf |> is_none && err = [])

  (** Test that, for all [x] performing [UF.find uf x] is an element related to
      [x] in the model, via the same relation. *)
  let test_find =
    QCheck.Test.make ~count ~name:(name "find")
    Generator.partition
    (function (ops, size) ->
        let uf, err = List.fold_left uf_op (UF.make size, []) ops in err = [] &&
        let model, err = List.fold_left model_op (Model.make size, []) ops in err = [] &&
        let test_pos i =
          let node = Model.Node.IntTerm i in
          let FindResult r = UF.find uf node in
          let IntTerm _ = r.representative in
          match Model.check_related model node r.representative with
          | Some r' -> Model.Relation.equal r' r.relation
          | None -> QCheck.Test.fail_report "Element not related to its representative"
        in forall test_pos (size-1))

  (** check that model and uf agree on values *)
  let check_values model uf size =
    let test_pos i =
      let node = Model.Node.IntTerm i in
      assert_eq
        ~msg:(Format.asprintf "get_value %d:" i) (Utils.Functions.pp_option (Model.Value.pretty node))
        (Model.get_value model node) (UF.get_value uf node)
    in forall test_pos (size-1)

  (** check that values of small are included in large *)
  let check_values_incl small large size =
    let test_pos i =
      let node = Model.Node.IntTerm i in
      match UF.get_value small node, UF.get_value large node with
      | _, None -> true
      | Some v, Some v' ->
        if Model.Value.incl node v v' then true else
              QCheck.Test.fail_reportf "Value inclusion fails: %a has value %a is small but %a in large"
              Model.Node.pretty node (Model.Value.pretty node) v (Model.Value.pretty node) v'
      | None, Some v ->
              QCheck.Test.fail_reportf "Value inclusion fails: %a has no value in small but %a in large"
              Model.Node.pretty node (Model.Value.pretty node) v
    in forall test_pos (size-1)

  let test_get_value =
    QCheck.Test.make ~count ~name:(name "get_value")
    Generator.partition
    (function (ops, size) ->
        let uf, err = List.fold_left uf_op (UF.make size, []) ops in err = [] &&
        let model, err = List.fold_left model_op (Model.make size, []) ops in err = [] &&
        check_values model uf size)

  (** Test that [UF.check_related uf x x] is always identity. *)
  let test_self_related =
    QCheck.Test.make ~count ~name:(name "self_related")
    Generator.partition
    (function (ops, size) ->
        let uf, err = List.fold_left uf_op (UF.make size, []) ops in
        err = [] &&
        let test_pos i =
          let node = Model.Node.IntTerm i in
          UF.check_related uf node node = Some (Model.Relation.identity)
        in
        forall test_pos (size-1)
      )

  (** Checks that model and uf agree by checking [get_relation a b] for most arbitrary pairs *)
  let check_relations model uf size =
    let scale_a = 3 in (* larger = faster but fewer checks*)
    let scale_b = 14 in (* larger = faster but fewer checks*)
    let test_pos i j =
      let nodei = Model.Node.IntTerm (i*scale_a) in
      let nodej = Model.Node.IntTerm (j*scale_b) in
      assert_eq
        ~msg:(Format.sprintf "Relation mismatch between nodes %d and %d: " i j)
        (Utils.Functions.pp_option Model.Relation.pretty)
        (Model.check_related model nodei nodej) (UF.check_related uf nodei nodej)
    in forall (fun i -> forall (test_pos i) ((i-1)/scale_b)) ((size-1) / scale_a)

  (** Checks that all relation from [uf_a] are in [uf_b] by checking [get_relation a b]
      for most arbitrary pairs *)
  let check_relations_subset uf_a uf_b size =
    let scale_a = 3 in (* larger = faster but fewer checks*)
    let scale_b = 14 in (* larger = faster but fewer checks*)
    let test_pos i j =
      let nodei = Model.Node.IntTerm (i*scale_a) in
      let nodej = Model.Node.IntTerm (j*scale_b) in
      match UF.check_related uf_a nodei nodej with
      | None -> true
      | Some _ as r ->
          assert_eq
            ~msg:(Format.sprintf "Relation subset mismatch between nodes %d and %d: " i j)
            (Utils.Functions.pp_option Model.Relation.pretty)
            r (UF.check_related uf_b nodei nodej)
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
      match related_a, related_b with
      | Some l, Some r when Model.Relation.equal l r -> related_j = Some r
      | _ -> related_j = None
    in forall (fun i -> forall (test_pos i) ((i-1)/scale_b)) ((size-1) / scale_a)

  (** test that [check_related] are the same in UF and the model *)
  let test_related =
    QCheck.Test.make ~count ~name:(name "related")
    Generator.partition
    (function (ops, size) ->
        let uf, err = List.fold_left uf_op (UF.make size, []) ops in err = [] &&
        let model, err = List.fold_left model_op (Model.make size, []) ops in err = [] &&
        check_relations model uf size
      )

  let test_persistent_find =
    QCheck.Test.make ~count ~name:(name "persistent_find")
    Generator.partition_with_split
    (fun (ops, size, op_l, op_r) ->
        let uf_c, err = List.fold_left uf_op (UF.make size, []) ops in err = [] &&
        let uf_l, err_l = List.fold_left uf_op (opt_copy uf_c, []) op_l in
        let uf_r, err_r = List.fold_left uf_op (opt_copy uf_c, []) op_r in
        let model_c, err = List.fold_left model_op (Model.make size, []) ops in err = [] &&
        let model_l, m_err_l = List.fold_left model_op (Model.copy model_c, []) op_l in
        let model_r, m_err_r = List.fold_left model_op (Model.copy model_c, []) op_r in
        err_l = m_err_l && err_r = m_err_r
        && check_relations model_c uf_c size
        && check_relations model_l uf_l size
        && check_relations model_r uf_r size
      )

  let test_persistent_get_value =
    QCheck.Test.make ~count ~name:(name "persistent_get_value")
    Generator.partition_with_split
    (fun (ops, size, op_l, op_r) ->
        let uf_c, err = List.fold_left uf_op (UF.make size, []) ops in err = [] &&
        let uf_l, err_l = List.fold_left uf_op (opt_copy uf_c, []) op_l in
        let uf_r, err_r = List.fold_left uf_op (opt_copy uf_c, []) op_r in
        let model_c, err = List.fold_left model_op (Model.make size, []) ops in err = [] &&
        let model_l, m_err_l = List.fold_left model_op (Model.copy model_c, []) op_l in
        let model_r, m_err_r = List.fold_left model_op (Model.copy model_c, []) op_r in
        err_l = m_err_l && err_r = m_err_r
        && check_values model_c uf_c size
        && check_values model_l uf_l size
        && check_values model_r uf_r size)

  let test_join_invariants =
    QCheck.Test.make ~count ~name:(name "join_invariants")
    Generator.partition_with_split
    (fun (ops, size, op_l, op_r) ->
        let uf_c, err = List.fold_left uf_op (UF.make size, []) ops in err = [] &&
        let uf_l, _ = List.fold_left uf_op (opt_copy uf_c, []) op_l in
        let uf_r, _ = List.fold_left uf_op (opt_copy uf_c, []) op_r in
        let join = UF.join uf_l uf_r in
        UF.check_invariants join |> is_none)

  let test_join_relations =
    QCheck.Test.make ~count ~name:(name "join_relations")
    Generator.partition_with_split
    (fun (ops, size, op_l, op_r) ->
        let uf_c, err = List.fold_left uf_op (UF.make size, []) ops in err = [] &&
        let uf_l, _ = List.fold_left uf_op (opt_copy uf_c, []) op_l in
        let uf_r, _ = List.fold_left uf_op (opt_copy uf_c, []) op_r in
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
        let uf_c, err = List.fold_left uf_op (UF.make size, []) ops in err = [] &&
        let uf_l, _ = List.fold_left uf_op (opt_copy uf_c, []) op_l in
        let uf_r, _ = List.fold_left uf_op (opt_copy uf_c, []) op_r in
        let join = UF.join uf_l uf_r in
        check_relations_subset uf_c join size)

  let test_join_is_and =
    QCheck.Test.make ~count ~name:(name "join_contains_is_and")
    Generator.partition_with_split
    (fun (ops, size, op_l, op_r) ->
        let uf_c, err = List.fold_left uf_op (UF.make size, []) ops in err = [] &&
        let uf_l, _ = List.fold_left uf_op (opt_copy uf_c, []) op_l in
        let uf_r, _ = List.fold_left uf_op (opt_copy uf_c, []) op_r in
        let join = UF.join uf_l uf_r in
        check_relations_join uf_l uf_r join size)

  let test_join_values =
    QCheck.Test.make ~count ~name:(name "join_values")
    Generator.partition_with_split
    (fun (ops, size, op_l, op_r) ->
        let uf_c, err = List.fold_left uf_op (UF.make size, []) ops in err = [] &&
        let uf_l, _ = List.fold_left uf_op (opt_copy uf_c, []) op_l in
        let uf_r, _ = List.fold_left uf_op (opt_copy uf_c, []) op_r in
        let join = UF.join uf_l uf_r in
        let model_c, err = List.fold_left model_op (Model.make size, []) ops in err = [] &&
        let model_l, _ = List.fold_left model_op (Model.copy model_c, []) op_l in
        let model_r, _ = List.fold_left model_op (Model.copy model_c, []) op_r in
        let model_join = Model.join model_l model_r in
        (* check_values model_c uf_c size
        && check_values model_l uf_l size
        && check_values model_r uf_r size *)
        check_values_incl uf_l join size && check_values_incl uf_r join size &&
        check_values model_join join size)


  let test_join_incl =
    QCheck.Test.make ~count ~name:(name "join_incl")
    Generator.partition_with_split
    (fun (ops, size, op_l, op_r) ->
        let uf_c, err = List.fold_left uf_op (UF.make size, []) ops in err = [] &&
        let uf_l, _ = List.fold_left uf_op (opt_copy uf_c, []) op_l in
        let uf_r, _ = List.fold_left uf_op (opt_copy uf_c, []) op_r in
        let join = UF.join uf_l uf_r in
        UF.incl uf_l join && UF.incl uf_r join)

  let test_self_incl =
    QCheck.Test.make ~count ~name:(name "self_incl")
    Generator.partition
    (function (ops, size) ->
        let uf, err = List.fold_left uf_op (UF.make size, []) ops in
        err = [] && UF.incl uf uf
      )

  let test_meet_incl =
    QCheck.Test.make ~count ~name:(name "meet_incl")
    Generator.partition_with_split
    (fun (ops, size, op_l, op_r) ->
        let uf_c, err = List.fold_left uf_op (UF.make size, []) ops in err = [] &&
        let uf_l, _ = List.fold_left uf_op (opt_copy uf_c, []) op_l in
        let uf_r, _ = List.fold_left uf_op (opt_copy uf_c, []) op_r in
        let meet = UF.meet uf_l uf_r |> fst in
        UF.incl meet uf_l)

  let check_relations_meet uf_a uf_b uf_meet size =
    let scale_a = 3 in (* larger = faster but fewer checks*)
    let scale_b = 14 in (* larger = faster but fewer checks*)
    let test_pos i j =
      let nodei = Model.Node.IntTerm (i*scale_a) in
      let nodej = Model.Node.IntTerm (j*scale_b) in
      let related_a = UF.check_related uf_a nodei nodej in
      let related_b = UF.check_related uf_b nodei nodej in
      let related_j = UF.check_related uf_meet nodei nodej in
      match related_a, related_b, related_j with
      | Some r, _, None -> QCheck.Test.fail_reportf "Left contains %a --(%a)--> %a, but they are unrelated in the meet"
                           Model.Node.pretty nodei Model.Relation.pretty r Model.Node.pretty nodej
      | Some r, _, Some r' -> if Model.Relation.equal r r' then true else
                              QCheck.Test.fail_reportf "Left: %a --(%a)--> %a, meet: %a --(%a)--> %a"
                                Model.Node.pretty nodei Model.Relation.pretty r Model.Node.pretty nodej
                                Model.Node.pretty nodei Model.Relation.pretty r' Model.Node.pretty nodej
      | None, Some r, None -> QCheck.Test.fail_reportf "Left: unrelated, Right: %a --(%a)--> %a, meet: unrelated"
                              Model.Node.pretty nodei Model.Relation.pretty r Model.Node.pretty nodej
      | None, Some _, Some _ -> true
      | _ -> true
    in forall (fun i -> forall (test_pos i) ((i-1)/scale_b)) ((size-1) / scale_a)

  let test_meet_contains_or =
    QCheck.Test.make ~count ~name:(name "meet_contains_or")
    Generator.partition_with_split
    (fun (ops, size, op_l, op_r) ->
        let uf_c, err = List.fold_left uf_op (UF.make size, []) ops in err = [] &&
        let uf_l, _ = List.fold_left uf_op (opt_copy uf_c, []) op_l in
        let uf_r, _ = List.fold_left uf_op (opt_copy uf_c, []) op_r in
        let meet = UF.meet uf_l uf_r |> fst in
        check_relations_meet uf_l uf_r meet size)


  let test_meet_values =
    QCheck.Test.make ~count ~name:(name "meet_values")
    Generator.partition_with_split
    (fun (ops, size, op_l, op_r) ->
        let uf_c, err = List.fold_left uf_op (UF.make size, []) ops in err = [] &&
        let uf_l, _ = List.fold_left uf_op (opt_copy uf_c, []) op_l in
        let uf_r, _ = List.fold_left uf_op (opt_copy uf_c, []) op_r in
        let meet = UF.meet uf_l uf_r |> fst in
        check_values_incl meet uf_l size && check_values_incl meet uf_r size)

  let tests = [
      test_make;
      test_union_chain;
      test_find;
      test_self_related;
      test_related;
      test_persistent_find;
      test_join_invariants;
      test_join_relations;
      test_join_contains_common;
      test_join_is_and;
      test_self_incl;
      test_join_incl;
      test_meet_incl;
      test_meet_contains_or;
    ] @ (if S.valued then [
      test_get_value;
      test_persistent_get_value;
      test_join_values;
      test_meet_values;
    ] else [])
end

open Testing_utils

(* let tests = List.fold_left (fun tests (x: (module Functor_sig.LABELED_VALUED_UNION_FIND_LATTICE_FUNCTOR) Instances.with_id) ->
  let module F = (val x.instance) in
  let module UF = PolymorphicOfMonomorphic(F) in
  let module UF_Test = TestPolymorphic(struct
      let name = x.name
      let valued = x.has_value
      let is_array = x.is_array
    end)(UF) in
  tests @ UF_Test.tests
  ) [] Testing_utils.Instances.labeled_valued *)

let tests = List.fold_left (fun tests (x: (module Functor_sig.POLYMORPHIC_LABELED_VALUED_UNION_FIND_LATTICE_FUNCTOR) Instances.with_id) ->
  let module F = (val x.instance) in
  let module UF = F(Model.Node)(Model.Relation)(Model.Value) in
  let module UF = TestPolymorphic(struct
      let name = x.name
      let valued = x.has_value
      let is_array = x.is_array
    end)(UF) in
  tests @ UF.tests
  ) [] Testing_utils.Instances.polymorphic_labeled_valued

(* let tests = []


let mkn n = Model.Node.IntTerm n
let mkr r = Generator.mk_relation (Some r)
let itv i i' = Model.Value.interval (Some (Z.of_int i))  (Some (Z.of_int i'))
let add_relation i j r = Generator.AddRelation(mkn i, mkn j, mkr r)
let add_value n i i' = Generator.AddValue(mkn n, itv i i')

let common = [add_relation 702 56 0;
              add_relation 56 1014 0 ]
let size = 1016
let left = []
let right = [add_relation 1014 830 0;
             add_relation 702 475 0 ]




module Config = struct
  let path_compression = `Lazy
  let hash_consed = false
  let join = `Diff
  let union_strategy = `Random
  let extendable = false
end

module ConfigAltJoin = struct
  include Config
  let join = `DiffParentEdit
end

module UF = Union_Find_Lattice.Polymorphic.Patricia_tree.Make(Config)(Model.Node)(Model.Relation)(Model.Value)()

let uf_op (uf, err) = function
| Generator.AddValue(i,v) -> UF.set_value ~intersect:true uf i v, err
| Generator.AddRelation(i,j,r) -> match UF.add_relation uf i j r with
    | Ok x -> x, err
    | Error r -> (uf, (i,j,r)::err)

let check_relations model uf size =
  let scale_a = 1 in (* larger = faster but fewer checks*)
  let scale_b = 4 in (* larger = faster but fewer checks*)
  let test_pos i j =
    (* Format.printf "test_pos %d %d@." i j; *)
    let nodei = Model.Node.IntTerm (i*scale_a) in
    let nodej = Model.Node.IntTerm (j*scale_b) in
      (Model.check_related model nodei nodej) = (UF.check_related uf nodei nodej)
  in forall (fun i -> forall (test_pos i) ((i-1)/scale_b)) ((size-1) / scale_a)

let check_relations_join uf_a uf_b uf_join size =
  let scale_a = 1 in (* larger = faster but fewer checks*)
  let scale_b = 1 in (* larger = faster but fewer checks*)
  let test_pos i j =
    let nodei = Model.Node.IntTerm (i*scale_a) in
    let nodej = Model.Node.IntTerm (j*scale_b) in
    let related_a = UF.check_related uf_a nodei nodej in
    let related_b = UF.check_related uf_b nodei nodej in
    let related_j = UF.check_related uf_join nodei nodej in
    match related_a, related_b with
    | Some l, Some r when Model.Relation.equal l r -> related_j = Some r
    | _ -> related_j = None
  in forall (fun i -> forall (test_pos i) ((i-1)/scale_b)) ((size-1) / scale_a)

let check_values model uf size =
  let test_pos i =
    let node = Model.Node.IntTerm i in
    let pp = Union_Find_Lattice.Utils.Functions.pp_option Model.Value.pretty in
    let v1 = Model.get_value model node in
    let v2 = UF.get_value uf node in
    Format.printf "get_value %d: %a = %a@." i pp v1 pp v2;
    v1 = v2
  in forall test_pos (size-1)

let () =
  let uf_c, err = List.fold_left uf_op (UF.make size, []) common in
  let uf_l, _ = List.fold_left uf_op (uf_c, []) left in
  let uf_r, _ = List.fold_left uf_op (uf_c, []) right in
  let join = UF.join uf_l uf_r in
  let model_c, err = List.fold_left model_op (Model.make size, []) common in
  let model_l, _ = List.fold_left model_op (Model.copy model_c, []) left in
  let model_r, _ = List.fold_left model_op (Model.copy model_c, []) right in
  let model_join = Model.join model_l model_r in
  Format.printf "L: @[%a@]@." UF.pretty uf_l;
  Format.printf "R: @[%a@]@." UF.pretty uf_r;
  Format.printf "J: @[%a@]@." UF.pretty join;
  (match UF.check_invariants join with
   | None -> ()
   | Some err -> Format.printf "Invariants:@.%s" err; assert false);
  assert(check_relations model_c uf_c size
        && check_relations model_l uf_l size
        && check_relations model_r uf_r size
        && check_relations model_join join size && check_relations_join uf_l uf_r join size)


  (* match UF.check_invariants join with
  | None -> ()
  | Some str -> Format.printf "%s@." str; *)
  (*let model_c, err = List.fold_left model_op (Model.make size, []) ops in
  let model_l, _ = List.fold_left model_op (Model.copy model_c, []) op_l in
  let model_r, _ = List.fold_left model_op (Model.copy model_c, []) op_r in
  let model_join = Model.join model_l model_r in
  assert (check_relations model_c uf_c size
  && check_relations model_l uf_l size
  && check_relations model_r uf_r size);
  Format.printf "L =========@.UF: @[%a@]@.Model: @[%a@]@.@." UF.pretty uf_l Model.pretty model_l;
  Format.printf "R =========@.UF: @[%a@]@.Model: @[%a@]@.@." UF.pretty uf_r Model.pretty model_r;
  Format.printf "J =========UF: @[%a@]@.Model: @[%a@]@." UF.pretty join Model.pretty model_join;
  assert (check_relations model_join join size) *) *)
