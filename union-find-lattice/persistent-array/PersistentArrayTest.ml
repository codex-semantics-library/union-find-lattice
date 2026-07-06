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

let test_name name = "persitent-array." ^ name

(** Simple implementation of persistent arrays as functional maps *)
module Model = struct
  module IntMap = Map.Make(Int)
  type 'a t = 'a IntMap.t

  let init n f =
    let rec runner map = function
      | m when m = n -> map
      | m -> runner (IntMap.add m (f m) map) (m + 1)
    in runner IntMap.empty 0
  let make n v = init n (fun _ -> v)
  let get a i = IntMap.find i a
  let set a i v = IntMap.add i v a

  let diff t1 t2 =
    IntMap.merge (fun i v1 v2 -> match v1, v2 with
      | None, None -> None
      | None, _ | _, None -> failwith "unbalanced maps"
      | Some v1, Some v2 ->
          if v1 == v2 then None else Some (v1,v2)
        ) t1 t2
end

let gen_mutlist size nb_mut =  QCheck.Gen.(list_size nb_mut (pair (int_range 0 (size-1)) int))
let nb_mut = QCheck.Gen.int_range 0 5_000

let gen_mutations =
  let open QCheck.Gen in
  let* size = int_range 2 5_000 in
  pair (return size) @@ gen_mutlist size nb_mut

(** Return three mutations lists [common, left, right] to generate two trees from a common version *)
let gen_triple_mut =
  let open QCheck.Gen in
  let* size = int_range 2 5_000 in
  tup4 (return size) (gen_mutlist size nb_mut) (gen_mutlist size nb_mut) (gen_mutlist size nb_mut)

let shrink_mutlist = QCheck.Shrink.(list ~shrink:(pair int int))

(* FIXME: these shrinkers take A LOT of time, making them impractical. *)
(* let shrink_mutations =
  let open QCheck.Shrink in
  pair (fun size -> QCheck.Iter.return size) shrink_mutlist

let shrink_triple_mut =
  let open QCheck.Shrink in
  tup4 (fun size -> QCheck.Iter.return size) shrink_mutlist shrink_mutlist shrink_mutlist *)

let pp_mutlist fmt l =
  let len = List.length l in
  let max_elems = 10 in
  List.iteri (fun i (pos, mut) ->
    if i = max_elems then Format.fprintf fmt "..."
    else if i > max_elems then ()
    else Format.printf "(%d,%d)%s" pos mut (if i = len - 1 then "" else ", ")
  ) l

let pp_mutations fmt (s,l) =
  Format.fprintf fmt "len:%d [%a]" (List.length l) pp_mutlist l
let pp_triple_mutations fmt (s,c,l,r) =
  Format.fprintf fmt "len:%d [%a | %a | %a]" (List.length c) pp_mutlist c pp_mutlist l pp_mutlist r

let mutations = QCheck.make gen_mutations (*~shrink:shrink_mutations*) ~print:(Format.asprintf "%a" pp_mutations)
let triple_mutations = QCheck.make gen_triple_mut (*~shrink:shrink_triple_mut*) ~print:(Format.asprintf "%a" pp_triple_mutations)

let equal size (array, model) =
  try for k = 0 to size - 1 do
    assert (PersistentArray.get array k = Model.get model k)
  done;
  true
  with Assert_failure _ -> false

let test_set_chain = QCheck.Test.make ~count:1_000 ~name:("persitent-array.get_set") mutations (fun
  (size,mutations) ->
    let array = PersistentArray.make size 0 in
    let model = Model.make size 0 in
    let (_, _, l) = List.fold_left (fun (j, (array, model), list) (i, x) ->
        let new_ver = (PersistentArray.set array i x, Model.set model i x) in
        (j+1, new_ver, if j mod 100 == 0 then  new_ver::list else list))
      (0, (array, model), []) mutations in
    List.for_all (equal size) l
)

let versioned_equal size (array, model) =
  try for k = 0 to size - 1 do
    assert (PersistentArray.Versioned.get array k = Model.get model k)
  done;
  true
  with Assert_failure _ -> false

let test_set_chain_v = QCheck.Test.make ~count:1_000 ~name:("persitent-array.versioned.get_set") mutations (fun
  (size,mutations) ->
    let array = PersistentArray.Versioned.make size 0 in
    let model = Model.make size 0 in
    let (_, _, l) = List.fold_left (fun (j, (array, model), list) (i, x) ->
        let new_ver = (PersistentArray.Versioned.set array i x, Model.set model i x) in
        (j+1, new_ver, if j mod 100 == 0 then  new_ver::list else list))
      (0, (array, model), []) mutations in
    List.for_all (versioned_equal size) l
)


let do_mutations acc l = List.fold_left (fun (array, model) (i,x) -> (PersistentArray.set array i x, Model.set model i x)) acc l

let test_diff = QCheck.Test.make ~count:1_000 ~name:("persitent-array.diff") triple_mutations (fun
  (size, common, left, right) ->
    let array = PersistentArray.make size 0 in
    let model = Model.make size 0 in
    let common = do_mutations (array, model) common in
    let (array_l, model_l) = do_mutations common left in
    let (array_r, model_r) = do_mutations common right in
    let diff = PersistentArray.diff array_r array_l |> fst in
    let model_diff = Model.diff model_r model_l in
    (* They aren't exactly equal, since model only sees true differences, whereas
       PA might have i -> [v,v] if a value is changed and then reverted. *)
    Model.IntMap.for_all (fun i v -> PersistentArray.IntHashtable.find diff i = v) model_diff &&
    PersistentArray.IntHashtable.fold (fun i (v,v') bool -> bool && (v = v' || Model.IntMap.find i model_diff = (v,v'))) diff true
    )

let do_mutations acc l = List.fold_left (fun (array, model) (i,x) -> (PersistentArray.Versioned.set array i x, Model.set model i x)) acc l

let test_diff_v = QCheck.Test.make ~count:1_000 ~name:("persitent-array.versioned.diff") triple_mutations (fun
  (size, common, left, right) ->
    let array = PersistentArray.Versioned.make size 0 in
    let model = Model.make size 0 in
    let common = do_mutations (array, model) common in
    let (array_l, model_l) = do_mutations common left in
    let (array_r, model_r) = do_mutations common right in
    let diff, _ = PersistentArray.Versioned.diff array_r array_l in
    let model_diff = Model.diff model_r model_l in
    (* They aren't exactly equal, since model only sees true differences, whereas
       PA might have i -> [v,v] if a value is changed and then reverted. *)
    Model.IntMap.for_all (fun i v -> PersistentArray.IntHashtable.find diff i = v) model_diff &&
    PersistentArray.IntHashtable.fold (fun i (v,v') bool -> bool && (v = v' || Model.IntMap.find i model_diff = (v,v'))) diff true
    )

let tests = [
  test_set_chain;
  test_diff;
  test_set_chain_v;
  test_diff_v;
]

let main () =
    let retcode = QCheck_runner.run_tests ~colors:true ~verbose:true tests in
  exit retcode

let () = main ()
