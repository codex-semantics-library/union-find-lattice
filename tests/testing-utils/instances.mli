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

(** This module groups all confluently-persistent union-find implementations
    into same-signatures list. This allows for easy testing and benchmarking. *)

open Union_Find_Lattice

type 'a with_id = {
  instance: 'a;
  name: string;
  is_array: bool;
  has_value: bool;
}


val union_finds : (module Functor_sig.UNION_FIND_LATTICE_FUNCTOR) with_id list

val valued : (module Functor_sig.VALUED_UNION_FIND_LATTICE_FUNCTOR) with_id list

(** Labeled valued (LV) implementations *)
val labeled_valued : (module Functor_sig.LABELED_VALUED_UNION_FIND_LATTICE_FUNCTOR) with_id list

(** Polymorphic labeled valued (PLV) implementations *)
val polymorphic_labeled_valued : (module Functor_sig.POLYMORPHIC_LABELED_VALUED_UNION_FIND_LATTICE_FUNCTOR) with_id list
