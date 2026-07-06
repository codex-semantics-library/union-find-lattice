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

(** Classic union-find lattice with no extensions (values or labels) *)

(** Reference implementation: this implements union-find as a mutable array.
    {b Warning:} it is NOT persistent, but provides a
    {{!Sig.UNION_FIND_LATTICE.copy}[copy]} function *)
module ArrayWithCopy
    (Config: Parameters.ARRAY_CONFIG)(
    Node: Parameters.NODE_WITH_COMPARE)
: Sig.UNION_FIND_LATTICE with type node = Node.t


(** Patricia-tree based union-find lattice *)
module PatriciaTree
  (Config : Parameters.PATRICIA_TREE_CONFIG)
  (Node: Parameters.NODE)
: Sig.UNION_FIND_LATTICE with type node = Node.t

(** Persistent array based union-find lattice. *)
module PersistentArray
  (Config: Parameters.PERSISTENT_ARRAY_CONFIG)
  (Node: Parameters.NODE)
: Sig.UNION_FIND_LATTICE with type node = Node.t

(** Persistent array based union-find lattice.
    Same as {!PersistentArray}, but builds {{!Sig.UNION_FIND_LATTICE.join}[join]}
    on the nearest common ancestor instead of on one of the arguments. *)
module PersistentArrayNCA
  (Config: Parameters.PERSISTENT_ARRAY_CONFIG)
  (Node: Parameters.NODE)
: Sig.UNION_FIND_LATTICE with type node = Node.t
