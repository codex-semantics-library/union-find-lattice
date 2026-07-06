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

open Union_Find_Lattice

module Config = DefaultConfig

type 'a with_id = {
  instance: 'a;
  name: string;
  is_array: bool;
  has_value: bool;
}

let mk_id x ?(is_array=false) n = {
  instance=x;
  name=n;
  is_array;
  has_value=false;
}

open Functor_sig

let union_finds = [
  mk_id (module Classic.ArrayWithCopy(Config): UNION_FIND_LATTICE_FUNCTOR) ~is_array:true "A";
  mk_id (module Classic.PatriciaTree(Config): UNION_FIND_LATTICE_FUNCTOR) "PT";
  mk_id (module Classic.PersistentArray(Config): UNION_FIND_LATTICE_FUNCTOR) "PA";
  mk_id (module Classic.PersistentArrayNCA(Config): UNION_FIND_LATTICE_FUNCTOR) "PAN";
]

let mk_id x ?(is_array=false) n = {
  instance=x;
  name=n;
  is_array;
  has_value=true;
}


let valued = [
  mk_id (module Valued.ArrayWithCopy(Config): VALUED_UNION_FIND_LATTICE_FUNCTOR) ~is_array:true "A_V";
  mk_id (module Valued.PatriciaTree(Config): VALUED_UNION_FIND_LATTICE_FUNCTOR) "PT_V";
  mk_id (module Valued.PersistentArray(Config): VALUED_UNION_FIND_LATTICE_FUNCTOR) "PA_V";
  mk_id (module Valued.PersistentArrayNCA(Config): VALUED_UNION_FIND_LATTICE_FUNCTOR) "PAN_V";
]

let labeled_valued = [
  mk_id (module LabeledValued.ArrayWithCopy(Config): LABELED_VALUED_UNION_FIND_LATTICE_FUNCTOR) ~is_array:true "A_LV";
  mk_id (module LabeledValued.PatriciaTree(Config): LABELED_VALUED_UNION_FIND_LATTICE_FUNCTOR) "PT_LV";
  mk_id (module LabeledValued.PersistentArray(Config): LABELED_VALUED_UNION_FIND_LATTICE_FUNCTOR) "PA_LV";
  mk_id (module LabeledValued.PersistentArrayNCA(Config): LABELED_VALUED_UNION_FIND_LATTICE_FUNCTOR) "PAN_LV";
]

let polymorphic_labeled_valued = [
  mk_id (module PolymorphicValued.ArrayWithCopy(Config): POLYMORPHIC_LABELED_VALUED_UNION_FIND_LATTICE_FUNCTOR) ~is_array:true "A_PLV";
  mk_id (module PolymorphicValued.PatriciaTree(Config): POLYMORPHIC_LABELED_VALUED_UNION_FIND_LATTICE_FUNCTOR) "PT_PLV";
  mk_id (module PolymorphicValued.PersistentArray(Config): POLYMORPHIC_LABELED_VALUED_UNION_FIND_LATTICE_FUNCTOR) "PA_PLV";
  mk_id (module PolymorphicValued.PersistentArrayNCA(Config): POLYMORPHIC_LABELED_VALUED_UNION_FIND_LATTICE_FUNCTOR) "PAN_PLV";
]
