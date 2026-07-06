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

(** Polymorphic labeled union-find
    I.E. this is a variant of {!Labeled}, where instead of having simple
    [type node] and [relation], we use polymorphic
    [type 'a node], [('a, 'b) relation]. An [('a,'b) relation] relates a ['a node]
    and a ['b node]. *)

(** Array based polymorphic labeled union-find lattice with values.
    {b Warning:} this one is NOT persistent, and requires explicit calls to
    {{!Sig.POLYMORPHIC_LABELED_VALUED_UNION_FIND_LATTICE.copy}[copy]} on version switch. *)
module ArrayWithCopy
    (Config : Parameters.ARRAY_CONFIG)
    (Node : Parameters.POLYMORPHIC_NODE)
    (Relation : Parameters.POLYMORPHIC_GROUP)
: Sig.POLYMORPHIC_LABELED_UNION_FIND_LATTICE
  with type 'a node = 'a Node.t
   and type ('a, 'b) relation = ('a, 'b) Relation.t

(** Patricia Tree based polymorphic labeled union-find lattice with values. *)
module PatriciaTree
    (Config : Parameters.PATRICIA_TREE_CONFIG)
    (Node : Parameters.POLYMORPHIC_NODE)
    (Relation : Parameters.POLYMORPHIC_GROUP)
: Sig.POLYMORPHIC_LABELED_UNION_FIND_LATTICE
  with type 'a node = 'a Node.t
   and type ('a, 'b) relation = ('a, 'b) Relation.t

(** Persistent array based polymorphic labeled union-find lattice with values. *)
module PersistentArray
    (Config : Parameters.PERSISTENT_ARRAY_CONFIG)
    (Node : Parameters.POLYMORPHIC_NODE)
    (Relation : Parameters.POLYMORPHIC_GROUP)
: Sig.POLYMORPHIC_LABELED_UNION_FIND_LATTICE
  with type 'a node = 'a Node.t
   and type ('a, 'b) relation = ('a, 'b) Relation.t

(** Persistent array based polymorphic labeled union-find lattice with values.
    Same as {!PersitentArray}, but builds
    {{!Sig.POLYMORPHIC_LABELED_VALUED_UNION_FIND_LATTICE.join}[join]} on the
    nearest common ancestor instead of one of the branches. *)
module PersistentArrayNCA
    (Config : Parameters.PERSISTENT_ARRAY_CONFIG)
    (Node : Parameters.POLYMORPHIC_NODE)
    (Relation : Parameters.POLYMORPHIC_GROUP)
: Sig.POLYMORPHIC_LABELED_UNION_FIND_LATTICE
  with type 'a node = 'a Node.t
   and type ('a, 'b) relation = ('a, 'b) Relation.t
