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

(** Valued union-find: these attach an optional {!Sig.VALUE} to each equivalence
    class, with values forming a lattice. *)

(** Reference implementation: this implements union-find (with values) as a mutable array.
    {b Warning:} it is NOT persistent, but provides a
    {{!Sig.VALUED_UNION_FIND_LATTICE.copy}[copy]} function *)
module ArrayWithCopy
    (Config: Parameters.ARRAY_CONFIG)
    (Node: Parameters.NODE_WITH_COMPARE)
    (Value: Parameters.VALUE with type node = Node.t) :
  Sig.VALUED_UNION_FIND_LATTICE with type node = Node.t and type value = Value.t

(** Patricia-tree based union-find lattice with attached values. *)
module PatriciaTree
  (Config: Parameters.PATRICIA_TREE_CONFIG)
  (Node: Parameters.NODE)
  (Value: Parameters.VALUE with type node = Node.t)
: Sig.VALUED_UNION_FIND_LATTICE with type node = Node.t and type value = Value.t

(** Persistent-array based union-find lattice with attached values *)
module PersistentArray
  (Config: Parameters.PERSISTENT_ARRAY_CONFIG)
  (Node: Parameters.NODE)
  (Value: Parameters.VALUE with type node = Node.t)
: Sig.VALUED_UNION_FIND_LATTICE with type node = Node.t and type value = Value.t

(** Persistent-array based union-find lattice with attached values.
    Same as {!PersistentArray}, but builds {{!Sig.VALUED_UNION_FIND_LATTICE.join}[join]}
    on the nearest common ancestor instead of on one of the arguments. *)
module PersistentArrayNCA
  (Config: Parameters.PERSISTENT_ARRAY_CONFIG)
  (Node: Parameters.NODE)
  (Value: Parameters.VALUE with type node = Node.t)
: Sig.VALUED_UNION_FIND_LATTICE with type node = Node.t and type value = Value.t
