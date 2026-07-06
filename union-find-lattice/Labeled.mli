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

(** Labeled union-find: an extension of union find where {{!Parameters.NODE}nodes} are related
    to each other via a {{!Parameters.GROUP}relation} (for example [x --(+2)--> y]).
    This allows representing more complex relation than equality, like equality
    up to a constant. Relations form a mathematical group. *)

(** Array based labeled union-find lattice with values.
    {b Warning:} this one is NOT persistent, and requires explicit calls to
    {{!Sig.LABELED_VALUED_UNION_FIND_LATTICE.copy}[copy]} on version switch. *)
module ArrayWithCopy
  (Config: Parameters.ARRAY_CONFIG)
  (Node: Parameters.NODE)
  (Relation: Parameters.GROUP) :
Sig.LABELED_UNION_FIND_LATTICE
  with type node = Node.t
   and type relation = Relation.t

(** Patricia Tree based labeled union-find lattice with values. *)
module PatriciaTree
  (Config: Parameters.PATRICIA_TREE_CONFIG)
  (Node: Parameters.NODE)
  (Relation: Parameters.GROUP) :
Sig.LABELED_UNION_FIND_LATTICE
  with type node = Node.t
   and type relation = Relation.t

(** Persistent array based labeled union-find lattice with values. *)
module PersistentArray
  (Config: Parameters.PERSISTENT_ARRAY_CONFIG)
  (Node: Parameters.NODE)
  (Relation: Parameters.GROUP) :
Sig.LABELED_UNION_FIND_LATTICE
  with type node = Node.t
   and type relation = Relation.t

(** Persistent array based labeled union-find lattice with values.
    Same as {!PersitentArray}, but builds
    {{!Sig.LABELED_UNION_FIND_LATTICE.join}[join]} on the
    nearest common ancestor instead of one of the branches. *)
module PersistentArrayNCA
  (Config: Parameters.PERSISTENT_ARRAY_CONFIG)
  (Node: Parameters.NODE)
  (Relation: Parameters.GROUP) :
Sig.LABELED_UNION_FIND_LATTICE
  with type node = Node.t
   and type relation = Relation.t
